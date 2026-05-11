-module(nova_file_controller_tests).
-include_lib("eunit/include/eunit.hrl").

nova_file_controller_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      %% Happy paths
      fun serves_existing_file/1,
      fun returns_404_for_missing_file/1,
      fun serves_file_in_subdirectory/1,
      fun serves_url_encoded_utf8_filename/1,
      fun single_dot_segment_serves_root/1,
      fun dot_segment_in_middle_serves_file/1,
      fun dotdot_then_redescend_serves_file/1,

      %% Direct traversal vectors
      fun rejects_literal_dotdot_traversal/1,
      fun rejects_url_encoded_dotdot_traversal/1,
      fun rejects_mixed_case_url_encoded_traversal/1,
      fun rejects_url_encoded_slash_traversal/1,
      fun rejects_absolute_path_segment/1,

      %% Depth-counting bugs the canonicaliser must not have
      fun rejects_chained_dotdot/1,
      fun rejects_mixed_subdir_dotdot_escape/1,
      fun rejects_long_traversal_chain/1,
      fun rejects_dotdot_after_descending_back_to_root/1,

      %% Unusual inputs that should not be misread as traversal
      fun double_encoded_dotdot_is_literal_not_traversal/1,
      fun backslash_segment_is_literal_on_unix/1,
      fun empty_segment_does_not_escape/1,
      fun three_dots_is_literal_filename/1,
      fun null_byte_segment_does_not_escape/1
     ]}.

setup() ->
    Dir = filename:join("/tmp",
                        "nova_file_controller_tests_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    Inside = filename:join(Dir, "inside"),
    Sub = filename:join(Inside, "sub"),
    ok = filelib:ensure_dir(filename:join(Sub, "x")),
    ok = file:write_file(filename:join(Inside, "ok.txt"), <<"safe">>),
    ok = file:write_file(filename:join(Sub, "child.txt"), <<"child">>),
    UtfName = unicode:characters_to_list("café.txt"),
    ok = file:write_file(filename:join(Inside, UtfName), <<"unicode">>),
    %% Sibling file outside the served dir; the traversal target.
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

%% ---- Happy paths ----------------------------------------------------------

serves_existing_file(#{inside := Inside}) ->
    fun() ->
        ?assertMatch({sendfile, 200, _, _, _},
                     nova_file_controller:get_dir(req(Inside, [<<"ok.txt">>])))
    end.

returns_404_for_missing_file(#{inside := Inside}) ->
    fun() ->
        ?assertEqual({status, 404},
                     nova_file_controller:get_dir(req(Inside, [<<"missing.txt">>])))
    end.

serves_file_in_subdirectory(#{inside := Inside}) ->
    fun() ->
        ?assertMatch({sendfile, 200, _, _, _},
                     nova_file_controller:get_dir(req(Inside, [<<"sub">>, <<"child.txt">>])))
    end.

serves_url_encoded_utf8_filename(#{inside := Inside}) ->
    fun() ->
        Encoded = unicode:characters_to_binary(uri_string:quote("café.txt")),
        ?assertMatch({sendfile, 200, _, _, _},
                     nova_file_controller:get_dir(req(Inside, [Encoded])))
    end.

single_dot_segment_serves_root(#{inside := Inside}) ->
    fun() ->
        %% "." canonicalises to no segments; falls into the directory clause.
        %% list_dir is off by default, so a directory request returns 403.
        %% We only assert it is NOT 200 and not a traversal sendfile.
        Result = nova_file_controller:get_dir(req(Inside, [<<".">>])),
        ?assert(Result =:= {status, 403} orelse element(1, Result) =:= status),
        ?assertNotMatch({sendfile, 200, _, _, _}, Result)
    end.

dot_segment_in_middle_serves_file(#{inside := Inside}) ->
    fun() ->
        ?assertMatch({sendfile, 200, _, _, _},
                     nova_file_controller:get_dir(req(Inside, [<<".">>, <<"ok.txt">>])))
    end.

dotdot_then_redescend_serves_file(#{inside := Inside}) ->
    fun() ->
        %% sub/../ok.txt resolves to ok.txt at the root of the served dir.
        ?assertMatch({sendfile, 200, _, _, _},
                     nova_file_controller:get_dir(
                       req(Inside, [<<"sub">>, <<"..">>, <<"ok.txt">>])))
    end.

%% ---- Direct traversal vectors ---------------------------------------------

rejects_literal_dotdot_traversal(#{inside := Inside}) ->
    fun() ->
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(req(Inside, [<<"..">>, <<"secret.txt">>])))
    end.

rejects_url_encoded_dotdot_traversal(#{inside := Inside}) ->
    fun() ->
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(req(Inside, [<<"%2E%2E">>, <<"secret.txt">>])))
    end.

rejects_mixed_case_url_encoded_traversal(#{inside := Inside}) ->
    fun() ->
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(req(Inside, [<<"%2e%2E">>, <<"secret.txt">>])))
    end.

rejects_url_encoded_slash_traversal(#{inside := Inside}) ->
    fun() ->
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(req(Inside, [<<"%2E%2E%2Fsecret.txt">>])))
    end.

rejects_absolute_path_segment(#{inside := Inside, dir := Dir}) ->
    fun() ->
        Abs = unicode:characters_to_binary(filename:join(Dir, "secret.txt")),
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(req(Inside, [Abs])))
    end.

%% ---- Depth-counting bugs --------------------------------------------------

rejects_chained_dotdot(#{inside := Inside}) ->
    fun() ->
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(
                       req(Inside, [<<"..">>, <<"..">>, <<"secret.txt">>])))
    end.

rejects_mixed_subdir_dotdot_escape(#{inside := Inside}) ->
    fun() ->
        %% sub/../../secret.txt: enter sub, pop to root, then pop above root.
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(
                       req(Inside, [<<"sub">>, <<"..">>, <<"..">>, <<"secret.txt">>])))
    end.

rejects_long_traversal_chain(#{inside := Inside}) ->
    fun() ->
        Chain = lists:duplicate(20, <<"..">>) ++ [<<"etc">>, <<"passwd">>],
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(req(Inside, Chain)))
    end.

rejects_dotdot_after_descending_back_to_root(#{inside := Inside}) ->
    fun() ->
        %% sub/../../x: depth went 0->1->0->-1, must reject.
        ?assertEqual({status, 403},
                     nova_file_controller:get_dir(
                       req(Inside, [<<"sub">>, <<"..">>, <<"..">>, <<"x">>])))
    end.

%% ---- Unusual inputs -------------------------------------------------------

double_encoded_dotdot_is_literal_not_traversal(#{inside := Inside}) ->
    fun() ->
        %% %252E%252E decodes once to %2E%2E and stays a literal filename.
        %% Must NOT be treated as ".." and must NOT serve a sibling file.
        Result = nova_file_controller:get_dir(
                   req(Inside, [<<"%252E%252E">>, <<"secret.txt">>])),
        ?assertEqual({status, 404}, Result)
    end.

backslash_segment_is_literal_on_unix(#{inside := Inside}) ->
    fun() ->
        %% On Unix, \ is a literal character, not a path separator,
        %% so "..\\secret.txt" is a single weird filename, not traversal.
        Result = nova_file_controller:get_dir(
                   req(Inside, [<<"..\\secret.txt">>])),
        ?assertEqual({status, 404}, Result)
    end.

empty_segment_does_not_escape(#{inside := Inside}) ->
    fun() ->
        %% An empty segment turns the joined path absolute ("/secret.txt"),
        %% which must be rejected outright.
        Result = nova_file_controller:get_dir(
                   req(Inside, [<<>>, <<"secret.txt">>])),
        ?assertEqual({status, 403}, Result)
    end.

three_dots_is_literal_filename(#{inside := Inside}) ->
    fun() ->
        %% "..." is not a traversal marker; it is a normal filename
        %% that happens not to exist. Must 404, never 403 (false positive)
        %% and never 200.
        Result = nova_file_controller:get_dir(req(Inside, [<<"...">>])),
        ?assertEqual({status, 404}, Result)
    end.

null_byte_segment_does_not_escape(#{inside := Inside}) ->
    fun() ->
        %% A NUL byte must not let the request escape; either 404 or 403
        %% is acceptable, but it must not serve the sibling file.
        Result = nova_file_controller:get_dir(
                   req(Inside, [<<"ok.txt", 0, "secret.txt">>])),
        ?assertNotMatch({sendfile, 200, _, _, _}, Result)
    end.
