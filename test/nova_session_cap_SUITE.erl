%%% Requests without a cookie cannot fill the session table: nova_csrf_plugin
%%% starts a session for each of them, and those are the first to go when
%%% the table holds max_sessions. Nobody is kept from signing in, and nobody
%%% who is signed in is signed out.
-module(nova_session_cap_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([cookieless_requests_do_not_keep_a_client_from_signing_in/1,
         cookieless_requests_do_not_sign_anybody_out/1,
         visitor_gets_a_token_while_the_table_is_full/1,
         visitor_gets_no_token_when_every_session_is_in_use/1,
         sign_in_replaces_the_oldest_session_when_every_session_is_in_use/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MAX_SESSIONS, 50).
-define(TABLE, nova_session_ets_entries).
-define(PAGES, nova_session_cap_pages).

all() ->
    [cookieless_requests_do_not_keep_a_client_from_signing_in,
     cookieless_requests_do_not_sign_anybody_out,
     visitor_gets_a_token_while_the_table_is_full,
     visitor_gets_no_token_when_every_session_is_in_use,
     sign_in_replaces_the_oldest_session_when_every_session_is_in_use].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    application:load(nova),
    application:set_env(nova, max_sessions, ?MAX_SESSIONS),
    Routes = [#{prefix => "/csrf",
                plugins => [{pre_request, nova_csrf_plugin, #{}}],
                routes => [{"/token", fun nova_csrf_pages:token/1, #{methods => [get]}}]},
              #{prefix => "",
                plugins => [],
                routes => [{"/sign-in", fun ?PAGES:sign_in/1, #{methods => [get]}},
                           {"/who", fun ?PAGES:who/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop(),
    application:unset_env(nova, max_sessions).

cookieless_requests_do_not_keep_a_client_from_signing_in(Config) ->
    flood(Config, ?MAX_SESSIONS + 5),
    ?assertEqual(?MAX_SESSIONS, ets:info(?TABLE, size)),
    Cookie = sign_in(Config, "alice"),
    ?assertEqual(~"alice", who(Config, Cookie)),
    ?assertEqual(?MAX_SESSIONS, ets:info(?TABLE, size)).

cookieless_requests_do_not_sign_anybody_out(Config) ->
    Cookies = [{User, sign_in(Config, User)} || User <- ["alice", "bob", "carol"]],
    flood(Config, 4 * ?MAX_SESSIONS),
    ?assertEqual(?MAX_SESSIONS, ets:info(?TABLE, size)),
    [?assertEqual(list_to_binary(User), who(Config, Cookie)) || {User, Cookie} <- Cookies].

visitor_gets_a_token_while_the_table_is_full(Config) ->
    flood(Config, 2 * ?MAX_SESSIONS),
    {200, Headers, Token} = nova_test_listener:response(?config(port, Config), "GET", "/csrf/token", []),
    ?assertNotEqual(~"no token", Token),
    ?assertEqual({200, Token},
                 nova_test_listener:get(?config(port, Config), "/csrf/token", [{"cookie", cookie(Headers)}])).

%% The sessions that are in use are not dropped for a visitor's token.
visitor_gets_no_token_when_every_session_is_in_use(Config) ->
    Cookies = [sign_in(Config, user(N)) || N <- lists:seq(1, ?MAX_SESSIONS)],
    ?assertEqual({200, ~"no token"}, nova_test_listener:get(?config(port, Config), "/csrf/token")),
    ?assertEqual([list_to_binary(user(N)) || N <- lists:seq(1, ?MAX_SESSIONS)],
                 [who(Config, Cookie) || Cookie <- Cookies]).

sign_in_replaces_the_oldest_session_when_every_session_is_in_use(Config) ->
    [First | Others] = [sign_in(Config, user(N)) || N <- lists:seq(1, ?MAX_SESSIONS)],
    Latest = sign_in(Config, "latest"),
    ?assertEqual(~"latest", who(Config, Latest)),
    ?assertEqual(~"nobody", who(Config, First)),
    ?assertEqual(?MAX_SESSIONS - 1, length([Cookie || Cookie <- Others, who(Config, Cookie) =/= ~"nobody"])),
    ?assertEqual(?MAX_SESSIONS, ets:info(?TABLE, size)).

flood(Config, Count) ->
    Answers = [nova_test_listener:get(?config(port, Config), "/csrf/token") || _ <- lists:seq(1, Count)],
    ?assertEqual([200], lists:usort([Status || {Status, _Body} <- Answers])).

user(N) ->
    "user" ++ integer_to_list(N).

sign_in(Config, User) ->
    {200, Headers, _Body} = nova_test_listener:response(?config(port, Config), "GET", "/sign-in?user=" ++ User, []),
    %% The oldest session is the one nearest its expiry.
    timer:sleep(2),
    cookie(Headers).

who(Config, Cookie) ->
    {200, Body} = nova_test_listener:get(?config(port, Config), "/who", [{"cookie", Cookie}]),
    Body.

cookie(#{~"set-cookie" := SetCookie}) ->
    [Cookie | _] = binary:split(iolist_to_binary(SetCookie), ~";"),
    Cookie.
