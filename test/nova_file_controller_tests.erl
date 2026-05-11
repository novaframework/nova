-module(nova_file_controller_tests).
-include_lib("eunit/include/eunit.hrl").

nova_file_controller_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun serves_existing_file/1,
      fun returns_404_for_missing_file/1,
      fun rejects_literal_dotdot_traversal/1,
      fun rejects_url_encoded_dotdot_traversal/1,
      fun rejects_mixed_case_url_encoded_traversal/1,
      fun rejects_url_encoded_slash_traversal/1,
      fun rejects_absolute_path_segment/1,
      fun serves_url_encoded_utf8_filename/1
     ]}.

setup() ->
    Dir = filename:join("/tmp",
                        "nova_file_controller_tests_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    Inside = filename:join(Dir, "inside"),
    ok = filelib:ensure_dir(filename:join(Inside, "x")),
    ok = file:write_file(filename:join(Inside, "ok.txt"), <<"safe">>),
    UtfName = unicode:characters_to_list("café.txt"),
    ok = file:write_file(filename:join(Inside, UtfName), <<"unicode">>),
    %% A sibling file outside the served dir, used as the traversal target.
    ok = file:write_file(filename:join(Dir, "secret.txt"), <<"do not leak">>),
    #{dir => Dir, inside => Inside}.

cleanup(#{dir := Dir}) ->
    os:cmd("rm -rf " ++ Dir),
    ok.

req(Inside, Pathinfo) ->
    #{path => <<"/whatever">>,
      headers => #{},
      extra_state => #{static => {dir, Inside},
                       pathinfo => Pathinfo,
                       options => #{}}}.

serves_existing_file(#{inside := Inside}) ->
    fun() ->
        Result = nova_file_controller:get_dir(req(Inside, [<<"ok.txt">>])),
        ?assertMatch({sendfile, 200, _, _, _}, Result)
    end.

returns_404_for_missing_file(#{inside := Inside}) ->
    fun() ->
        Result = nova_file_controller:get_dir(req(Inside, [<<"missing.txt">>])),
        ?assertEqual({status, 404}, Result)
    end.

rejects_literal_dotdot_traversal(#{inside := Inside}) ->
    fun() ->
        Result = nova_file_controller:get_dir(req(Inside, [<<"..">>, <<"secret.txt">>])),
        ?assertEqual({status, 403}, Result)
    end.

rejects_url_encoded_dotdot_traversal(#{inside := Inside}) ->
    fun() ->
        Result = nova_file_controller:get_dir(req(Inside, [<<"%2E%2E">>, <<"secret.txt">>])),
        ?assertEqual({status, 403}, Result)
    end.

rejects_mixed_case_url_encoded_traversal(#{inside := Inside}) ->
    fun() ->
        Result = nova_file_controller:get_dir(req(Inside, [<<"%2e%2E">>, <<"secret.txt">>])),
        ?assertEqual({status, 403}, Result)
    end.

rejects_url_encoded_slash_traversal(#{inside := Inside}) ->
    fun() ->
        Result = nova_file_controller:get_dir(req(Inside, [<<"%2E%2E%2Fsecret.txt">>])),
        ?assertEqual({status, 403}, Result)
    end.

rejects_absolute_path_segment(#{inside := Inside, dir := Dir}) ->
    fun() ->
        Abs = unicode:characters_to_binary(filename:join(Dir, "secret.txt")),
        Result = nova_file_controller:get_dir(req(Inside, [Abs])),
        ?assertEqual({status, 403}, Result)
    end.

serves_url_encoded_utf8_filename(#{inside := Inside}) ->
    fun() ->
        Encoded = unicode:characters_to_binary(uri_string:quote("café.txt")),
        Result = nova_file_controller:get_dir(req(Inside, [Encoded])),
        ?assertMatch({sendfile, 200, _, _, _}, Result)
    end.
