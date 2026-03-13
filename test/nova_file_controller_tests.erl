-module(nova_file_controller_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% get_file/1
%%====================================================================

get_file_serves_existing_file_test() ->
    Filepath = create_temp_file("hello world"),
    Req = #{extra_state => #{static => {file, Filepath}, options => #{}},
            headers => #{}},
    Result = nova_file_controller:get_file(Req),
    ?assertMatch({sendfile, 200, #{}, {0, 11, Filepath}, _MimeType}, Result),
    file:delete(Filepath).

get_file_priv_file_test() ->
    %% Use a known priv file from nova itself
    PrivDir = code:priv_dir(nova),
    case file:list_dir(PrivDir) of
        {ok, [F|_]} ->
            FullPath = filename:join(PrivDir, F),
            case filelib:is_file(FullPath) of
                true ->
                    Req = #{extra_state => #{static => {priv_file, nova, F},
                                             options => #{}},
                            headers => #{}},
                    Result = nova_file_controller:get_file(Req),
                    ?assertMatch({sendfile, 200, _, _, _}, Result);
                false ->
                    ok
            end;
        _ ->
            ok
    end.

get_file_returns_404_for_bad_req_test() ->
    ?assertEqual({status, 404}, nova_file_controller:get_file(#{})).

get_file_range_request_test() ->
    Filepath = create_temp_file("0123456789"),
    Req = #{extra_state => #{static => {file, Filepath}, options => #{}},
            headers => #{<<"range">> => <<"bytes=2-5">>}},
    Result = nova_file_controller:get_file(Req),
    ?assertMatch({sendfile, 206, _, {2, 4, Filepath}, _}, Result),
    file:delete(Filepath).

get_file_invalid_range_test() ->
    Filepath = create_temp_file("short"),
    Req = #{extra_state => #{static => {file, Filepath}, options => #{}},
            headers => #{<<"range">> => <<"bytes=100-200">>}},
    Result = nova_file_controller:get_file(Req),
    ?assertMatch({status, 416, _}, Result),
    file:delete(Filepath).

%%====================================================================
%% get_dir/1
%%====================================================================

get_dir_with_pathinfo_serves_file_test() ->
    Dir = create_temp_dir(),
    Filepath = filename:join(Dir, "test.txt"),
    ok = file:write_file(Filepath, <<"dir file">>),
    Req = #{extra_state => #{pathinfo => [<<"test.txt">>],
                             static => {dir, Dir},
                             options => #{}},
            headers => #{}},
    Result = nova_file_controller:get_dir(Req),
    ?assertMatch({sendfile, 200, _, _, _}, Result),
    file:delete(Filepath),
    file:del_dir(Dir).

get_dir_pathinfo_not_found_test() ->
    Dir = create_temp_dir(),
    Req = #{extra_state => #{pathinfo => [<<"nope.txt">>],
                             static => {dir, Dir},
                             options => #{}},
            headers => #{}},
    ?assertEqual({status, 404}, nova_file_controller:get_dir(Req)),
    file:del_dir(Dir).

get_dir_returns_404_for_bad_req_test() ->
    ?assertEqual({status, 404}, nova_file_controller:get_dir(#{})).

%%====================================================================
%% Router callback construction (regression for make_fun fix)
%%====================================================================

router_static_callback_is_callable_test() ->
    %% Verify that erlang:make_fun/3 produces a callable function
    Callback = erlang:make_fun(nova_file_controller, get_file, 1),
    ?assert(is_function(Callback, 1)),
    %% Should return 404 for a bad request, not crash
    ?assertEqual({status, 404}, Callback(#{})).

router_static_dir_callback_is_callable_test() ->
    Callback = erlang:make_fun(nova_file_controller, get_dir, 1),
    ?assert(is_function(Callback, 1)),
    ?assertEqual({status, 404}, Callback(#{})).

%%====================================================================
%% Helpers
%%====================================================================

create_temp_file(Content) ->
    Filename = filename:join("/tmp", "nova_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:write_file(Filename, Content),
    Filename.

create_temp_dir() ->
    Dir = filename:join("/tmp", "nova_test_dir_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    Dir.
