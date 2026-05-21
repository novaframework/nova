-module(nova_file_controller_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APP, nova_file_controller_test_app).
-define(UNESCAPED_TEXT, "національна абетка").
-define(ESCAPED_TEXT, uri_string:quote(?UNESCAPED_TEXT)).
-define(PRIV_DIR, "/tmp/nova_file_controller_test").
-define(BASE_PATH, "базовий шлях").
-define(FILE_NAME, ?UNESCAPED_TEXT ++ ".txt").

nova_file_controller_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"Serve an existing file from priv dir", fun test_serve_existing_file/0},
        {"Serve an existing file as priv file", fun test_serve_one_file/0},
        {"Serve an existing dir from priv", fun test_serve_existing_dir/0},
        {"Return 404 for missing file", fun test_serve_missing_file/0}
        % {"Handle directory traversal safely", fun test_serve_outer_file/0}
    ]}.

setup() ->
    case lists:member(code, meck:mocked()) of
        false ->
            meck:new(code, [unstick, passthrough, no_link]);
        _ ->
            ok
    end,
    meck:expect(code, priv_dir, fun(App) ->
        case App of
            ?APP -> ?PRIV_DIR;
            _ -> meck:passthrough([App])
        end
    end),
    FilePath = filename:join(
        [?PRIV_DIR, ?BASE_PATH, ?UNESCAPED_TEXT, ?UNESCAPED_TEXT, ?FILE_NAME]
    ),
    filelib:ensure_dir(FilePath),
    %meck:unload(code),
    ok = file:write_file(FilePath, <<"Hello from EUnit">>),
    ok = file:write_file(filename:join(?PRIV_DIR, "passwd"), <<"user:password">>).

cleanup(_) ->
    meck:delete(code, priv_dir, 1).

test_serve_existing_file() ->
    FilePath = filename:join(
        [?PRIV_DIR, ?BASE_PATH, ?UNESCAPED_TEXT, ?UNESCAPED_TEXT, ?FILE_NAME]
    ),
    Req = #{
        path =>
            <<"/", (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(?FILE_NAME, utf8))/binary>>,
        extra_state =>
            #{
                options => #{list_dir => true},
                static =>
                    {priv_dir, ?APP, ?BASE_PATH},
                pathinfo =>
                    [
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(?FILE_NAME, utf8)
                    ]
            },
        headers => #{}
    },
    Result = nova_file_controller:get_dir(Req),
    ?assertMatch({sendfile, 200, _, {0, _, FilePath}, _}, Result).

test_serve_one_file() ->
    FilePath = filename:join(
        [?PRIV_DIR, ?BASE_PATH, ?UNESCAPED_TEXT, ?UNESCAPED_TEXT, ?FILE_NAME]
    ),
    RelativePath = filename:join(
        [?BASE_PATH, ?UNESCAPED_TEXT, ?UNESCAPED_TEXT, ?FILE_NAME]
    ),
    Req = #{
        path =>
            <<"/", (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(?FILE_NAME, utf8))/binary>>,
        extra_state =>
            #{
                options => #{list_dir => true},
                static =>
                    {priv_file, ?APP, RelativePath},
                pathinfo =>
                    [
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(?FILE_NAME, utf8)
                    ]
            },
        headers => #{}
    },
    Result = nova_file_controller:get_file(Req),
    ?assertMatch({sendfile, 200, _, {0, _, FilePath}, _}, Result).

test_serve_existing_dir() ->
    Req = #{
        path =>
            <<"/", (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary>>,
        extra_state =>
            #{
                options => #{list_dir => true},
                static =>
                    {priv_dir, ?APP, ?BASE_PATH},
                pathinfo =>
                    [
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8)
                    ]
            },
        headers => #{}
    },
    Result = nova_file_controller:get_dir(Req),
    ?assertMatch({ok, _}, Result),
    {ok, Data} = Result,
    {ok, Output} = nova_file_dtl:render(Data),
    ?assert(lists:member(unicode:characters_to_binary(?FILE_NAME), lists:flatten(Output))).

test_serve_missing_file() ->
    FileName = "missing.txt",
    Req = #{
        path =>
            <<"/", (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(?ESCAPED_TEXT, utf8))/binary, "/",
                (unicode:characters_to_binary(FileName, utf8))/binary>>,
        extra_state =>
            #{
                options => #{list_dir => true},
                static =>
                    {priv_dir, ?APP, ?BASE_PATH},
                pathinfo =>
                    [
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(?ESCAPED_TEXT, utf8),
                        unicode:characters_to_binary(FileName, utf8)
                    ]
            },
        headers => #{}
    },
    Result = nova_file_controller:get_dir(Req),
    ?assertMatch({status, 404}, Result).

% test_serve_outer_file() ->
%     FileName = "../passwd",
%     Req = #{
%         path =>
%             <<"/", (unicode:characters_to_binary(FileName, utf8))/binary>>,
%         extra_state =>
%             #{
%                 options => #{list_dir => true},
%                 static =>
%                     {priv_dir, ?APP, ?BASE_PATH},
%                 pathinfo =>
%                     [
%                         unicode:characters_to_binary(FileName, utf8)
%                     ]
%             },
%         headers => #{}
%     },
%     Result = nova_file_controller:get_dir(Req),
%     ?debugFmt("Controller Result Is ~p~n", [Result]),
%     ?assertMatch({status, 403}, Result).
