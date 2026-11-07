%%% Session cookies over a real Cowboy listener, against nova_test_app.
-module(nova_session_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).

%% Logger handler callback: forwards every event to the test process.
-export([log/2]).

-export([
         no_cookie_until_session_is_written/1,
         cookie_has_safe_attributes/1,
         session_survives_requests/1,
         unknown_session_id_is_replaced/1,
         renew_rotates_session_id/1,
         delete_clears_cookie/1,
         invalid_cookie_name_at_runtime_drops_the_cookie/1,
         invalid_cookie_name_fails_boot/1,
         renew_sets_cookie_without_stream_h/1,
         delete_expires_cookie_without_stream_h/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     no_cookie_until_session_is_written,
     cookie_has_safe_attributes,
     session_survives_requests,
     unknown_session_id_is_replaced,
     renew_rotates_session_id,
     delete_clears_cookie,
     invalid_cookie_name_at_runtime_drops_the_cookie,
     invalid_cookie_name_fails_boot,
     {group, without_stream_h}
    ].

groups() ->
    [{without_stream_h, [], [renew_sets_cookie_without_stream_h,
                             delete_expires_cookie_without_stream_h]}].

init_per_suite(Config) ->
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    persistent_term:put(nova_test_app_plugin, counters:new(2, [write_concurrency])),
    {ok, _} = application:ensure_all_started(inets),
    start(#{}, Config).

%% nova_session sets and expires the cookie itself when the listener has no
%% nova_stream_h.
init_per_group(without_stream_h, Config) ->
    application:stop(nova_test_app),
    application:stop(nova),
    start(#{stream_handlers => [cowboy_compress_h, cowboy_stream_h]}, proplists:delete(base, Config)).

end_per_group(_Group, _Config) ->
    ok.

start(Configuration, Config) ->
    Port = free_port(),
    application:set_env(nova, cowboy_configuration, Configuration#{port => Port}),
    {ok, _} = application:ensure_all_started(nova_test_app),
    [{base, "http://localhost:" ++ integer_to_list(Port)} | Config].

end_per_suite(_Config) ->
    persistent_term:erase(nova_test_app_plugin),
    application:stop(nova_test_app),
    application:stop(nova),
    ok.

%%====================================================================
%% Tests
%%====================================================================

no_cookie_until_session_is_written(Config) ->
    {200, Headers, _} = get(Config, "/session/untouched", undefined),
    ?assertEqual([], session_cookies(Headers)).

cookie_has_safe_attributes(Config) ->
    {200, Headers, _} = get(Config, "/session/set/a", undefined),
    [Cookie] = session_cookies(Headers),
    [_Value | Rest] = string:split(Cookie, ";", all),
    Attributes = [string:trim(A) || A <- Rest],
    ?assert(lists:member("Path=/", Attributes)),
    ?assert(lists:member("HttpOnly", Attributes)),
    ?assert(lists:member("SameSite=Lax", Attributes)),
    ?assert(lists:member("Max-Age=1209600", Attributes)),
    %% Secure unless the environment is explicitly dev; this suite runs in test.
    ?assert(lists:member("Secure", Attributes)).

session_survives_requests(Config) ->
    {200, H1, _} = get(Config, "/session/set/kept", undefined),
    Id = session_id(H1),
    {200, H2, Body} = get(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => ~"kept"}, json:decode(Body)),
    ?assertEqual([], session_cookies(H2)).

%% Session fixation: an id the server never issued is not adopted.
unknown_session_id_is_replaced(Config) ->
    Planted = "planted-by-attacker",
    {200, _, Body} = get(Config, "/session/get", Planted),
    ?assertEqual(#{~"value" => null}, json:decode(Body)),
    {200, H2, _} = get(Config, "/session/set/victim", Planted),
    Issued = session_id(H2),
    ?assertNotEqual(Planted, Issued),
    {200, _, Planted1} = get(Config, "/session/get", Planted),
    ?assertEqual(#{~"value" => null}, json:decode(Planted1)),
    {200, _, Victim} = get(Config, "/session/get", Issued),
    ?assertEqual(#{~"value" => ~"victim"}, json:decode(Victim)).

renew_rotates_session_id(Config) ->
    {200, H1, _} = get(Config, "/session/set/before", undefined),
    Old = session_id(H1),
    {200, H2, _} = get(Config, "/session/renew", Old),
    New = session_id(H2),
    ?assertNotEqual(Old, New),
    {200, _, OldBody} = get(Config, "/session/get", Old),
    ?assertEqual(#{~"value" => null}, json:decode(OldBody)),
    {200, _, NewBody} = get(Config, "/session/get", New),
    ?assertEqual(#{~"value" => ~"renewed"}, json:decode(NewBody)).

delete_clears_cookie(Config) ->
    {200, H1, _} = get(Config, "/session/set/bye", undefined),
    Id = session_id(H1),
    {200, H2, _} = get(Config, "/session/delete", Id),
    [Cookie] = session_cookies(H2),
    ?assertMatch("session_id=;" ++ _, Cookie),
    ?assertNotEqual(nomatch, string:find(Cookie, "Max-Age=0")),
    {200, _, Body} = get(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => null}, json:decode(Body)).

renew_sets_cookie_without_stream_h(Config) ->
    {200, H1, _} = get(Config, "/session/renew", undefined),
    Id = session_id(H1),
    {200, H2, Body} = get(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => ~"renewed"}, json:decode(Body)),
    ?assertEqual([], session_cookies(H2)).

delete_expires_cookie_without_stream_h(Config) ->
    {200, H1, _} = get(Config, "/session/renew", undefined),
    Id = session_id(H1),
    {200, H2, _} = get(Config, "/session/delete", Id),
    [Cookie] = session_cookies(H2),
    ?assertMatch("session_id=;" ++ _, Cookie),
    ?assertNotEqual(nomatch, string:find(Cookie, "Max-Age=0")),
    {200, _, Body} = get(Config, "/session/get", Id),
    ?assertEqual(#{~"value" => null}, json:decode(Body)).

%% cow_cookie raises on a name that is not a token. That used to loop on
%% Cowboy's 500 path in nova_stream_h, logging the request on every pass.
invalid_cookie_name_at_runtime_drops_the_cookie(Config) ->
    persistent_term:erase({nova_stream_h, invalid_cookie_logged, ~"app:sid"}),
    ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
    application:set_env(nova, session_cookie_name, ~"app:sid"),
    try
        Request = fun() ->
                          {ok, {{_, Status, _}, Headers, _}} =
                              httpc:request(get, {?config(base, Config) ++ "/session/set/a",
                                                  [{"authorization", "Bearer AUTH_SECRET"}]},
                                            [{timeout, 5000}], [{body_format, binary}]),
                          ?assertEqual(200, Status),
                          ?assertEqual([], set_cookies(Headers))
                  end,
        Request(),
        Request(),
        timer:sleep(200),
        Logs = logged(),
        ?assertMatch([#{cookie := ~"app:sid"}],
                     [R || {report, #{msg := <<"Session cookie not set", _/binary>>} = R} <- Logs]),
        ?assertEqual(nomatch, string:find(io_lib:format("~p", [Logs]), "AUTH_SECRET"))
    after
        application:unset_env(nova, session_cookie_name),
        logger:remove_handler(?MODULE),
        persistent_term:erase({nova_stream_h, invalid_cookie_logged, ~"app:sid"})
    end.

%% Leaves Nova stopped; the next group starts it again.
invalid_cookie_name_fails_boot(_Config) ->
    application:stop(nova_test_app),
    application:stop(nova),
    application:set_env(nova, session_cookie_name, ~"app:sid"),
    try
        {error, Reason} = application:ensure_all_started(nova_test_app),
        ?assertNotEqual(nomatch, string:find(io_lib:format("~p", [Reason]), "invalid_session_cookie_config"))
    after
        application:unset_env(nova, session_cookie_name)
    end.

%%====================================================================
%% Helpers
%%====================================================================

get(Config, Path, SessionId) ->
    Headers = case SessionId of
                  undefined -> [];
                  _ -> [{"cookie", "session_id=" ++ SessionId}]
              end,
    {ok, {{_, Status, _}, RespHeaders, Body}} =
        httpc:request(get, {?config(base, Config) ++ Path, Headers},
                      [{autoredirect, false}], [{body_format, binary}]),
    {Status, RespHeaders, Body}.

set_cookies(Headers) ->
    [Value || {Name, Value} <- Headers, string:lowercase(Name) =:= "set-cookie"].

session_cookies(Headers) ->
    [Value || {Name, Value = "session_id=" ++ _} <- Headers, string:lowercase(Name) =:= "set-cookie"].

session_id(Headers) ->
    [Cookie] = session_cookies(Headers),
    "session_id=" ++ Rest = hd(string:split(Cookie, ";")),
    Rest.

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.

log(#{msg := Msg}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Msg},
    ok.

logged() ->
    receive
        {logged, Msg} -> [Msg | logged()]
    after 0 ->
            []
    end.
