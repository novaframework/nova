%%% Requests without a cookie cannot fill the session table: nova_csrf_plugin
%%% starts a session for each of them, up to max_sessions.
-module(nova_session_cap_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([cookieless_requests_stop_adding_sessions/1,
         session_from_before_the_cap_still_works/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MAX_SESSIONS, 5).
-define(TABLE, nova_session_ets_entries).

all() ->
    [cookieless_requests_stop_adding_sessions,
     session_from_before_the_cap_still_works].

init_per_suite(Config) ->
    application:load(nova),
    application:set_env(nova, max_sessions, ?MAX_SESSIONS),
    Routes = [#{prefix => "/csrf",
                plugins => [{pre_request, nova_csrf_plugin, #{}}],
                routes => [{"/token", fun nova_csrf_pages:token/1, #{methods => [get]}}]}],
    Port = nova_test_listener:start(#{host => Routes}),
    {200, Headers, Token} = nova_test_listener:response(Port, "GET", "/csrf/token", []),
    [Cookie | _] = binary:split(maps:get(~"set-cookie", Headers), ~";"),
    [{port, Port}, {cookie, Cookie}, {token, Token} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop(),
    application:unset_env(nova, max_sessions).

cookieless_requests_stop_adding_sessions(Config) ->
    Answers = [nova_test_listener:get(?config(port, Config), "/csrf/token") || _ <- lists:seq(1, 40)],
    ?assertEqual([200], lists:usort([Status || {Status, _Body} <- Answers])),
    ?assertEqual(?MAX_SESSIONS, ets:info(?TABLE, size)),
    ?assertEqual(~"no token", element(2, lists:last(Answers))).

session_from_before_the_cap_still_works(Config) ->
    ?assertEqual(?MAX_SESSIONS, ets:info(?TABLE, size)),
    ?assertEqual({200, ?config(token, Config)},
                 nova_test_listener:get(?config(port, Config), "/csrf/token", [{"cookie", ?config(cookie, Config)}])).
