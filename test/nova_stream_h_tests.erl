-module(nova_stream_h_tests).
-include_lib("eunit/include/eunit.hrl").

%% This module doubles as the next stream handler: it hands back what it is
%% given as commands, so the tests can see what nova_stream_h forwards.
-export([init/3, data/4, info/3, terminate/3]).

init(_StreamID, Req, _Opts) ->
    {[{next_req, Req}], undefined}.

data(_StreamID, _IsFin, _Data, State) ->
    {[], State}.

info(_StreamID, Info, State) ->
    {[Info], State}.

terminate(_StreamID, _Reason, _State) ->
    ok.

%%====================================================================
%% Setup
%%====================================================================

setup() ->
    pg:start_link(nova_scope),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, use_sessions, true),
    application:set_env(nova, session_manager, nova_session_ets),
    {ok, Pid} = nova_session_ets:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    application:unset_env(nova, bootstrap_application),
    application:unset_env(nova, use_sessions),
    application:unset_env(nova, session_manager),
    application:unset_env(nova, session_cookie_opts),
    application:unset_env(nova, session_max_age),
    application:unset_env(nova, session_cookie_name),
    application:unset_env(nova, environment),
    application:unset_env(nova_stream_h_tests_app, session_max_age),
    ok.

stream_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun no_cookie_when_session_is_not_written/0,
      fun cookie_issued_when_session_is_written/0,
      fun default_cookie_attributes/0,
      fun dev_cookie_attributes/0,
      fun secure_over_https/0,
      fun max_age_infinity_has_no_max_age/0,
      fun settings_are_read_from_bootstrap_app/0,
      fun host_prefixed_cookie_name/0,
      fun two_session_cookies_mean_no_session/0,
      fun repeated_identical_cookie_is_one_session/0,
      fun configured_cookie_attributes/0,
      fun invalid_cookie_opts_are_ignored/0,
      fun known_session_is_kept/0,
      fun unknown_session_is_replaced/0,
      fun no_backend_lookup_in_connection_process/0,
      fun switch_protocol_carries_cookie/0,
      fun renew_issues_new_cookie/0,
      fun delete_expires_cookie/0,
      fun write_after_delete_issues_new_cookie/0,
      fun stream_reply_carries_cookie/0,
      fun sessions_disabled/0
     ]}.

%%====================================================================
%% Tests
%%====================================================================

no_cookie_when_session_is_not_written() ->
    {Req, State} = start(req()),
    ?assert(is_binary(maps:get(nova_session_id, Req))),
    ?assertEqual([], set_cookies(respond(State))).

cookie_issued_when_session_is_written() ->
    {#{nova_session_id := Id}, State} = start(req()),
    State1 = event({written, Id}, State),
    [Cookie] = set_cookies(respond(State1)),
    ?assertMatch(<<"session_id=", _/binary>>, Cookie),
    ?assertEqual(Id, cookie_value(Cookie)).

%% Behind a proxy that terminates TLS the request is plain http, so the cookie
%% is Secure unless the environment is explicitly dev.
default_cookie_attributes() ->
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Path=/")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; HttpOnly")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; SameSite=Lax")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Secure")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Max-Age=1209600")).

dev_cookie_attributes() ->
    application:set_env(nova, environment, dev),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertEqual(nomatch, binary:match(Cookie, ~"; Secure")).

max_age_infinity_has_no_max_age() ->
    application:set_env(nova, session_max_age, infinity),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertEqual(nomatch, binary:match(Cookie, ~"Max-Age")).

settings_are_read_from_bootstrap_app() ->
    application:set_env(nova, bootstrap_application, nova_stream_h_tests_app),
    application:set_env(nova_stream_h_tests_app, session_max_age, 60),
    application:set_env(nova, session_max_age, 3600),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Max-Age=60")).

%% Browsers only store a __Host- cookie that is Secure, has Path=/ and no
%% Domain, so those win over session_cookie_opts.
host_prefixed_cookie_name() ->
    application:set_env(nova, environment, dev),
    application:set_env(nova, session_cookie_name, ~"__Host-session_id"),
    application:set_env(nova, session_cookie_opts, #{domain => ~"example.com", path => ~"/app"}),
    ok = nova_session_ets:set_value(~"known", ~"k", ~"v"),
    {Req, _} = start(req(~"session_id=other; __Host-session_id=known")),
    ?assertEqual(~"known", maps:get(nova_session_presented, Req)),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertMatch(<<"__Host-session_id=", _/binary>>, Cookie),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Secure")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Path=/;")),
    ?assertEqual(nomatch, binary:match(Cookie, ~"Domain")).

%% A cookie of the same name set for a parent domain or path arrives next to
%% ours; neither is trusted.
two_session_cookies_mean_no_session() ->
    ok = nova_session_ets:set_value(~"one", ~"k", ~"v"),
    ok = nova_session_ets:set_value(~"two", ~"k", ~"v"),
    {Req, _} = start(req(~"session_id=one; session_id=two")),
    ?assertNot(maps:is_key(nova_session_presented, Req)),
    ?assert(is_binary(maps:get(nova_session_id, Req))),
    ?assert(persistent_term:get({nova_session, ambiguous_cookie_warned}, false)).

repeated_identical_cookie_is_one_session() ->
    {Req, _} = start(req(~"session_id=same; session_id=same")),
    ?assertEqual(~"same", maps:get(nova_session_presented, Req)).

secure_over_https() ->
    {#{nova_session_id := Id}, State} = start((req())#{scheme => ~"https"}),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Secure")).

configured_cookie_attributes() ->
    application:set_env(nova, session_cookie_opts, #{secure => true, same_site => strict,
                                                     domain => ~"example.com"}),
    application:set_env(nova, session_max_age, 3600),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Secure")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; SameSite=Strict")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Domain=example.com")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Max-Age=3600")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; HttpOnly")).

invalid_cookie_opts_are_ignored() ->
    application:set_env(nova, session_cookie_opts, #{path => "/not-a-binary", same_site => sideways,
                                                     unknown => 1, http_only => false}),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Path=/;")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; SameSite=Lax")),
    ?assertEqual(nomatch, binary:match(Cookie, ~"HttpOnly")).

known_session_is_kept() ->
    ok = nova_session_ets:set_value(~"known", ~"k", ~"v"),
    {Req, State} = start(req(~"session_id=known")),
    ?assertEqual(~"known", maps:get(nova_session_presented, Req)),
    ?assertNot(maps:is_key(nova_session_id, Req)),
    ?assertEqual([], set_cookies(respond(event({written, ~"known"}, State)))).

%% A client must not be able to pick its own session id: nova_session falls
%% back to the fresh id, and that is the cookie the client gets.
unknown_session_is_replaced() ->
    {Req, State} = start(req(~"session_id=attacker-chosen")),
    #{nova_session_pending := Id} = Req,
    ?assertNotEqual(~"attacker-chosen", Id),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertEqual(Id, cookie_value(Cookie)).

%% The connection process serves every stream on the connection, so it must
%% not wait for the session backend.
no_backend_lookup_in_connection_process() ->
    meck:new(nova_session_ets, [passthrough]),
    try
        {_Req, _State} = start(req(~"session_id=whatever")),
        ?assertEqual(0, meck:num_calls(nova_session_ets, session_exists, '_'))
    after
        meck:unload(nova_session_ets)
    end.

%% A session written in nova_websocket:init/1 gets its cookie on the 101.
switch_protocol_carries_cookie() ->
    {#{nova_session_id := Id}, State} = start(req()),
    State1 = event({written, Id}, State),
    {[{switch_protocol, Headers, cowboy_websocket, ws_state}], _} =
        nova_stream_h:info(1, {switch_protocol, #{~"upgrade" => ~"websocket"}, cowboy_websocket, ws_state},
                           State1),
    ?assertEqual(~"websocket", maps:get(~"upgrade", Headers)),
    [Cookie] = set_cookies(Headers),
    ?assertEqual(Id, cookie_value(Cookie)).

renew_issues_new_cookie() ->
    ok = nova_session_ets:set_value(~"before-login", ~"k", ~"v"),
    {_Req, State} = start(req(~"session_id=before-login")),
    [Cookie] = set_cookies(respond(event({renewed, ~"after-login"}, State))),
    ?assertEqual(~"after-login", cookie_value(Cookie)).

delete_expires_cookie() ->
    ok = nova_session_ets:set_value(~"logged-in", ~"k", ~"v"),
    {_Req, State} = start(req(~"session_id=logged-in")),
    [Cookie] = set_cookies(respond(event({deleted, ~"fresh"}, State))),
    ?assertMatch(<<"session_id=;", _/binary>>, Cookie),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"Max-Age=0")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Path=/")).

write_after_delete_issues_new_cookie() ->
    ok = nova_session_ets:set_value(~"old", ~"k", ~"v"),
    {_Req, State} = start(req(~"session_id=old")),
    State1 = event({written, ~"new"}, event({deleted, ~"new"}, State)),
    [Cookie] = set_cookies(respond(State1)),
    ?assertEqual(~"new", cookie_value(Cookie)).

stream_reply_carries_cookie() ->
    {#{nova_session_id := Id}, State} = start(req()),
    State1 = event({written, Id}, State),
    {[{headers, 200, Headers}], _} =
        nova_stream_h:info(1, {headers, 200, #{~"set-cookie" => [~"other=1"]}}, State1),
    [Other, Cookie] = set_cookies(Headers),
    ?assertEqual(~"other=1", Other),
    ?assertEqual(Id, cookie_value(Cookie)).

sessions_disabled() ->
    application:set_env(nova, use_sessions, false),
    {Req, State} = start(req()),
    ?assertNot(maps:is_key(nova_session_id, Req)),
    ?assertEqual([], set_cookies(respond(event({written, ~"x"}, State)))).

%%====================================================================
%% Helpers
%%====================================================================

req() ->
    #{scheme => ~"http", headers => #{}}.

req(Cookie) ->
    #{scheme => ~"http", headers => #{~"cookie" => Cookie}}.

start(Req) ->
    {[{next_req, NextReq}], State} =
        nova_stream_h:init(1, Req, #{stream_handlers => [?MODULE]}),
    {NextReq, State}.

event(Event, State) ->
    {[], State1} = nova_stream_h:info(1, {nova_session, Event}, State),
    State1.

respond(State) ->
    {[{response, 200, Headers, <<>>}], _State} =
        nova_stream_h:info(1, {response, 200, #{}, <<>>}, State),
    Headers.

set_cookies(Headers) ->
    [iolist_to_binary(C) || C <- maps:get(~"set-cookie", Headers, [])].

cookie_value(Cookie) ->
    [<<"session_id=", Value/binary>> | _] = binary:split(Cookie, ~";"),
    Value.
