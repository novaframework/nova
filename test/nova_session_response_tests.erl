-module(nova_session_response_tests).
-include_lib("eunit/include/eunit.hrl").

%% The session cookie on the response: what nova_session keeps for the request
%% the process is serving, and what nova_req sends of it.

setup() ->
    pg:start_link(nova_scope),
    application:unset_env(nova, environment),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, use_sessions, true),
    application:set_env(nova, session_manager, nova_session_ets),
    {ok, Pid} = nova_session_ets:start_link(),
    Pid.

cleanup(Pid) ->
    ok = nova_session:end_request(),
    gen_server:stop(Pid),
    application:unset_env(nova, bootstrap_application),
    application:unset_env(nova, use_sessions),
    application:unset_env(nova, session_manager),
    application:unset_env(nova, session_cookie_opts),
    application:unset_env(nova, session_max_age),
    application:unset_env(nova, session_cookie_name),
    application:unset_env(nova, environment),
    application:unset_env(nova_session_response_tests_app, session_max_age),
    ok.

response_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun no_cookie_when_session_is_not_written/0,
      fun cookie_issued_when_session_is_written/0,
      fun cookie_issued_for_an_id_that_is_handed_out/0,
      fun handed_out_id_is_not_issued_over_a_presented_cookie/0,
      fun handed_out_id_follows_what_the_session_does_next/0,
      fun default_cookie_attributes/0,
      fun dev_cookie_attributes/0,
      fun secure_over_https/0,
      fun max_age_infinity_has_no_max_age/0,
      fun settings_are_read_from_bootstrap_app/0,
      fun host_prefixed_cookie_name/0,
      fun two_session_cookies_mean_no_session/0,
      fun repeated_identical_cookie_is_one_session/0,
      fun unparsable_cookie_header_never_sets_cookie/0,
      fun configured_cookie_attributes/0,
      fun invalid_cookie_opts_are_ignored/0,
      fun known_session_is_kept/0,
      fun unknown_session_is_replaced/0,
      fun no_backend_lookup_before_the_session_is_used/0,
      fun upgrade_carries_cookie/0,
      fun renew_issues_new_cookie/0,
      fun delete_expires_cookie/0,
      fun write_after_delete_issues_new_cookie/0,
      fun stream_reply_carries_cookie/0,
      fun cookie_is_handed_out_once/0,
      fun cookie_does_not_depend_on_the_request_handed_back/0,
      fun no_cookie_after_the_request_ended/0,
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
    ?assertMatch(<<"__Host-session_id=", _/binary>>, Cookie),
    ?assertEqual(Id, cookie_value(Cookie)).

%% nova_session:client_id/1 hands the id of the request to a client that
%% sent no cookie. Nothing is stored for it.
cookie_issued_for_an_id_that_is_handed_out() ->
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({issued, Id}, State))),
    ?assertEqual(Id, cookie_value(Cookie)),
    ?assertNot(nova_session_ets:session_exists(Id)).

handed_out_id_is_not_issued_over_a_presented_cookie() ->
    {_Req, State} = start(req(~"__Host-session_id=held")),
    ?assertEqual([], set_cookies(respond(event({issued, ~"other"}, State)))).

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
    ?assertMatch(<<"session_id=", _/binary>>, Cookie),
    ?assertEqual(nomatch, binary:match(Cookie, ~"; Secure")).

secure_over_https() ->
    {#{nova_session_id := Id}, State} = start((req())#{scheme => ~"https"}),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Secure")).

max_age_infinity_has_no_max_age() ->
    application:set_env(nova, session_max_age, infinity),
    {#{nova_session_id := Id}, State} = start(req()),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertEqual(nomatch, binary:match(Cookie, ~"Max-Age")).

settings_are_read_from_bootstrap_app() ->
    application:set_env(nova, bootstrap_application, nova_session_response_tests_app),
    application:set_env(nova_session_response_tests_app, session_max_age, 60),
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
    {Req, _} = start(req(~"__Host-session_id=one; __Host-session_id=two")),
    ?assertNot(maps:is_key(nova_session_presented, Req)),
    ?assert(is_binary(maps:get(nova_session_id, Req))),
    ?assertNot(nova_once:first({nova_session, ambiguous_cookie_warned})).

repeated_identical_cookie_is_one_session() ->
    {Req, _} = start(req(~"__Host-session_id=same; __Host-session_id=same")),
    ?assertEqual(~"same", maps:get(nova_session_presented, Req)).

configured_cookie_attributes() ->
    application:set_env(nova, session_cookie_name, ~"session_id"),
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
    {Req, State} = start(req(~"__Host-session_id=known")),
    ?assertEqual(~"known", maps:get(nova_session_presented, Req)),
    ?assertNot(maps:is_key(nova_session_id, Req)),
    ?assertEqual([], set_cookies(respond(event({written, ~"known"}, State)))).

%% A client must not be able to pick its own session id: nova_session falls
%% back to the fresh id, and that is the cookie the client gets.
unknown_session_is_replaced() ->
    {Req, State} = start(req(~"__Host-session_id=attacker-chosen")),
    #{nova_session_pending := Id} = Req,
    ?assertNotEqual(~"attacker-chosen", Id),
    [Cookie] = set_cookies(respond(event({written, Id}, State))),
    ?assertEqual(Id, cookie_value(Cookie)).

renew_issues_new_cookie() ->
    ok = nova_session_ets:set_value(~"before-login", ~"k", ~"v"),
    {_Req, State} = start(req(~"__Host-session_id=before-login")),
    [Cookie] = set_cookies(respond(event({renewed, ~"after-login"}, State))),
    ?assertEqual(~"after-login", cookie_value(Cookie)).

delete_expires_cookie() ->
    ok = nova_session_ets:set_value(~"logged-in", ~"k", ~"v"),
    {_Req, State} = start(req(~"__Host-session_id=logged-in")),
    [Cookie] = set_cookies(respond(event({deleted, ~"fresh"}, State))),
    ?assertMatch(<<"__Host-session_id=;", _/binary>>, Cookie),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"Max-Age=0")),
    ?assertNotEqual(nomatch, binary:match(Cookie, ~"; Path=/")).

write_after_delete_issues_new_cookie() ->
    ok = nova_session_ets:set_value(~"old", ~"k", ~"v"),
    {_Req, State} = start(req(~"__Host-session_id=old")),
    State1 = event({written, ~"new"}, event({deleted, ~"new"}, State)),
    [Cookie] = set_cookies(respond(State1)),
    ?assertEqual(~"new", cookie_value(Cookie)).

sessions_disabled() ->
    application:set_env(nova, use_sessions, false),
    {Req, State} = start(req()),
    ?assertNot(maps:is_key(nova_session_id, Req)),
    ?assertEqual([], set_cookies(respond(event({written, ~"x"}, State)))).

handed_out_id_follows_what_the_session_does_next() ->
    Issued = fun() ->
                     {#{nova_session_id := Id}, State} = start(req()),
                     {Id, event({issued, Id}, State)}
             end,
    {Id1, State1} = Issued(),
    [Written] = set_cookies(respond(event({written, Id1}, State1))),
    ?assertEqual(Id1, cookie_value(Written)),
    {_Id2, State2} = Issued(),
    [Renewed] = set_cookies(respond(event({renewed, ~"renewed"}, State2))),
    ?assertEqual(~"renewed", cookie_value(Renewed)),
    {_Id3, State3} = Issued(),
    ?assertEqual([], set_cookies(respond(event({deleted, ~"fresh"}, State3)))).

%% The client may hold a valid session cookie Nova cannot read past the rest
%% of the header, so a write, renew or delete leaves it alone.
unparsable_cookie_header_never_sets_cookie() ->
    {Req, State} = start(req(~"__Host-session_id=abc; =x")),
    #{nova_session_id := Id} = Req,
    ?assertNot(maps:is_key(nova_session_presented, Req)),
    ?assertEqual({ok, Id}, nova_session:client_id(Req)),
    ?assertEqual([], set_cookies(respond(event({issued, Id}, State)))),
    ?assertEqual([], set_cookies(respond(event({written, Id}, State)))),
    ?assertEqual([], set_cookies(respond(event({renewed, ~"renewed"}, State)))),
    ?assertEqual([], set_cookies(respond(event({deleted, ~"fresh"}, State)))).

%% A request that never touches its session never asks the backend.
no_backend_lookup_before_the_session_is_used() ->
    meck:new(nova_session_ets, [passthrough]),
    try
        {_Req, _State} = start(req(~"__Host-session_id=whatever")),
        ?assertEqual(0, meck:num_calls(nova_session_ets, session_exists, '_'))
    after
        meck:unload(nova_session_ets)
    end.

%% A session written in nova_websocket:init/1 gets its cookie on the 101.
upgrade_carries_cookie() ->
    {#{nova_session_id := Id}, State} = start(req()),
    Req = event({written, Id}, State),
    {ok, _Req, #{}} = nova_req:upgrade_websocket(Req, #{}, nova_ws_handler, ws_state, #{}),
    [{upgrade_websocket, nova_ws_handler, ws_state, #{}, [Cookie]}] = nova_fake_server:sent(),
    ?assertEqual(Id, cookie_value(iolist_to_binary(Cookie))),
    ?assertEqual(true, nova_req:response_begun(Req)).

stream_reply_carries_cookie() ->
    {#{nova_session_id := Id}, State} = start(req()),
    Req = nova_req:set_resp_cookie(~"other", ~"1", event({written, Id}, State)),
    _ = nova_req:stream_reply(200, Req),
    [{stream_reply, 200, Headers}] = nova_fake_server:sent(),
    [Other, Cookie] = set_cookies(Headers),
    ?assertEqual(~"other=1", Other),
    ?assertEqual(Id, cookie_value(Cookie)).

%% The cookie goes out once: with the headers of a streamed response, and not
%% again with what follows on the same request.
cookie_is_handed_out_once() ->
    {#{nova_session_id := Id}, State} = start(req()),
    Req = event({written, Id}, State),
    ?assertMatch([_Cookie], nova_session:response_cookies(Req)),
    ?assertEqual([], nova_session:response_cookies(Req)).

%% A controller that answers and hands back the request from before the
%% answer still gets the cookie of its session on that answer.
cookie_does_not_depend_on_the_request_handed_back() ->
    {#{nova_session_id := Id} = Req, _State} = start(req()),
    ok = nova_session:set(Req#{nova_session_owner => self()}, ~"k", ~"v"),
    [Cookie] = set_cookies(respond(Req)),
    ?assertEqual(Id, cookie_value(Cookie)).

no_cookie_after_the_request_ended() ->
    {#{nova_session_id := Id}, State} = start(req()),
    _ = event({written, Id}, State),
    ok = nova_session:end_request(),
    ?assertEqual([], set_cookies(respond(State))).

%%====================================================================
%% Helpers
%%====================================================================

req() ->
    nova_fake_server:req(#{scheme => ~"http", method => ~"GET", headers => #{}}).

req(Cookie) ->
    nova_fake_server:req(#{scheme => ~"http", headers => #{~"cookie" => Cookie}}).

%% The request as the pipeline hands it to its first step. What the session
%% keeps for the response is kept by this process, so the request stands in
%% for the state the tests thread through.
start(Req0) ->
    Req = nova_session:start_request(Req0),
    {Req, Req}.

event(Event, Req) ->
    ok = nova_session:response_event(Event),
    Req.

%% The headers of a response to the request. Each call is a response of its
%% own, as if nothing had been sent before.
respond(Req) ->
    ok = nova_req:start_request(),
    _ = nova_req:reply(200, Req),
    [{reply, 200, Headers, <<>>}] = nova_fake_server:sent(),
    Headers.

set_cookies(Headers) ->
    [iolist_to_binary(Cookie) || {~"set-cookie", Cookie} <- Headers].

cookie_value(Cookie) ->
    [<<"__Host-session_id=", Value/binary>> | _] = binary:split(Cookie, ~";"),
    Value.
