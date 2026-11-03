-module(nova_session_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_pubsub.hrl").

%%====================================================================
%% Integration tests for nova_session + nova_session_ets
%%
%% These tests start the real nova_session_ets gen_server with pg-backed
%% pubsub, exercising the full set/get/delete path through ETS.
%%====================================================================

%%--------------------------------------------------------------------
%% Setup / Cleanup
%%--------------------------------------------------------------------

setup() ->
    pg:start_link(nova_scope),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, use_sessions, true),
    application:set_env(nova, session_manager, nova_session_ets),
    {ok, Pid} = nova_session_ets:start_link(),
    meck:new(cowboy_req, [non_strict]),
    meck:expect(cowboy_req, parse_cookies,
                fun(Req) ->
                    case maps:get(cookie_session_id, Req, undefined) of
                        undefined -> [];
                        SId -> [{~"session_id", SId}]
                    end
                end),
    meck:expect(cowboy_req, set_resp_cookie,
                fun(Name, Value, Req, Opts) ->
                    Req#{resp_cookie => #{name => Name, value => Value, opts => Opts}}
                end),
    Pid.

cleanup(Pid) ->
    meck:unload(cowboy_req),
    gen_server:stop(Pid),
    application:unset_env(nova, bootstrap_application),
    application:unset_env(nova, use_sessions),
    application:unset_env(nova, session_manager),
    ok.

%% After a pubsub broadcast, do a gen_server:call to synchronize
sync() ->
    nova_session_ets:get_value(<<"__sync__">>, <<"__sync__">>).

req_with_session(SessionId) ->
    #{nova_session_id => SessionId}.

req_with_cookie(SessionId) ->
    #{cookie_session_id => SessionId}.

%% A request whose connection process is a stand-in that forwards what
%% nova_session tells the stream handler.
stream_req(SessionId) ->
    Self = self(),
    Conn = spawn_link(fun() ->
                              receive
                                  {{_Pid, 7}, Event} -> Self ! {stream_event, Event}
                              end
                      end),
    #{nova_session_id => SessionId, pid => Conn, streamid => 7, nova_session_owner => self()}.

%% The request as nova_stream_h hands it over when the client sent a cookie.
presented_req(Presented, Pending) ->
    (maps:remove(nova_session_id, stream_req(Pending)))#{nova_session_presented => Presented,
                                                         nova_session_pending => Pending}.

no_stream_event() ->
    receive
        {stream_event, Event} -> error({unexpected_stream_event, Event})
    after 200 ->
            ok
    end.

stream_event() ->
    receive
        {stream_event, {nova_session, Event}} -> Event
    after 1000 ->
            error(no_stream_event)
    end.

%%====================================================================
%% Test generators
%%====================================================================

nova_session_ets_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun set_and_get_value/0,
      fun get_missing_session/0,
      fun get_missing_key/0,
      fun set_multiple_keys/0,
      fun delete_session/0,
      fun delete_single_key/0,
      fun cookie_fallback/0,
      fun no_session_returns_error/0,
      fun no_session_delete_returns_ok/0,
      fun sessions_disabled_throws/0,
      fun delete_moves_request_to_new_session/0,
      fun delete_tells_stream_handler/0,
      fun set_tells_stream_handler/0,
      fun renew_drops_old_session/0,
      fun renew_tells_stream_handler/0,
      fun renew_without_session/0,
      fun renew_sets_cookie_without_stream_handler/0,
      fun delete_expires_cookie_without_stream_handler/0,
      fun renew_with_stream_handler_sets_no_cookie/0,
      fun use_sessions_bootstrap_app_wins/0,
      fun known_presented_id_is_used/0,
      fun unknown_presented_id_is_replaced/0,
      fun renew_drops_presented_id/0,
      fun spawned_process_does_not_notify/0,
      fun request_without_owner_does_not_notify/0,
      fun non_error_set_results_notify/0,
      fun delete_key_preserves_other_keys/0,
      fun set_overwrites_existing_key/0,
      fun generate_session_id_format/0
     ]}.

%%====================================================================
%% Tests
%%====================================================================

set_and_get_value() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"username">>, <<"alice">>),
    sync(),
    ?assertEqual({ok, <<"alice">>}, nova_session:get(Req, <<"username">>)).

get_missing_session() ->
    Req = #{},
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)).

get_missing_key() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"exists">>, <<"yes">>),
    sync(),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"missing">>)).

set_multiple_keys() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"k1">>, <<"v1">>),
    ok = nova_session:set(Req, <<"k2">>, <<"v2">>),
    sync(),
    ?assertEqual({ok, <<"v1">>}, nova_session:get(Req, <<"k1">>)),
    ?assertEqual({ok, <<"v2">>}, nova_session:get(Req, <<"k2">>)).

delete_session() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"key">>, <<"val">>),
    sync(),
    {ok, _Req1} = nova_session:delete(Req),
    sync(),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)).

delete_single_key() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"keep">>, <<"yes">>),
    ok = nova_session:set(Req, <<"remove">>, <<"bye">>),
    sync(),
    {ok, _Req1} = nova_session:delete(Req, <<"remove">>),
    sync(),
    ?assertEqual({ok, <<"yes">>}, nova_session:get(Req, <<"keep">>)),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"remove">>)).

cookie_fallback() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_cookie(SId),
    ok = nova_session:set(Req, <<"via_cookie">>, <<"works">>),
    sync(),
    ?assertEqual({ok, <<"works">>}, nova_session:get(Req, <<"via_cookie">>)).

no_session_returns_error() ->
    Req = #{},
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)),
    ?assertEqual({error, session_id_not_set}, nova_session:set(Req, <<"k">>, <<"v">>)).

no_session_delete_returns_ok() ->
    Req = #{},
    ?assertEqual({ok, Req}, nova_session:delete(Req)),
    ?assertEqual({ok, Req}, nova_session:delete(Req, <<"key">>)).

sessions_disabled_throws() ->
    application:set_env(nova, use_sessions, false),
    Req = req_with_session(<<"id">>),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:get(Req, <<"k">>)),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:set(Req, <<"k">>, <<"v">>)),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:delete(Req)),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:delete(Req, <<"k">>)),
    application:set_env(nova, use_sessions, true).

delete_moves_request_to_new_session() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"key">>, <<"val">>),
    {ok, Req1} = nova_session:delete(Req),
    #{nova_session_id := NewId} = Req1,
    ?assertNotEqual(SId, NewId),
    ok = nova_session:set(Req1, ~"other", ~"x"),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"other")),
    ?assertEqual({ok, ~"x"}, nova_session:get(Req1, ~"other")).

delete_tells_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = stream_req(SId),
    {ok, #{nova_session_id := NewId}} = nova_session:delete(Req),
    ?assertEqual({deleted, NewId}, stream_event()).

set_tells_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    ok = nova_session:set(stream_req(SId), ~"k", ~"v"),
    ?assertEqual({written, SId}, stream_event()).

renew_drops_old_session() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, ~"user_id", ~"1"),
    {ok, Req1} = nova_session:renew(Req),
    #{nova_session_id := NewId} = Req1,
    ?assertNotEqual(SId, NewId),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"user_id")),
    ?assertEqual({error, not_found}, nova_session:get(Req1, ~"user_id")),
    ok = nova_session:set(Req1, ~"user_id", ~"2"),
    ?assertEqual({ok, ~"2"}, nova_session:get(Req1, ~"user_id")),
    ?assertNot(nova_session_ets:session_exists(SId)).

renew_tells_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, #{nova_session_id := NewId}} = nova_session:renew(stream_req(SId)),
    ?assertEqual({renewed, NewId}, stream_event()).

%% Without nova_stream_h, renew/1 and delete/1 put the cookie on the request.
renew_sets_cookie_without_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, Req1} = nova_session:renew(req_with_cookie(SId)),
    #{nova_session_id := NewId, resp_cookie := #{name := Name, value := Value, opts := Opts}} = Req1,
    ?assertEqual({~"session_id", NewId}, {Name, Value}),
    ?assertMatch(#{path := ~"/", http_only := true, max_age := 1209600}, Opts),
    {ok, Req2} = nova_session:renew(Req1),
    #{nova_session_id := NewerId, resp_cookie := #{value := NewerId}} = Req2,
    ?assertNotEqual(NewId, NewerId).

delete_expires_cookie_without_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, Req1} = nova_session:delete(req_with_cookie(SId)),
    #{resp_cookie := #{name := ~"session_id", value := <<>>, opts := #{max_age := 0}}} = Req1.

renew_with_stream_handler_sets_no_cookie() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, Req1} = nova_session:renew(req_with_session(SId)),
    ?assertNot(is_map_key(resp_cookie, Req1)),
    {ok, Req2} = nova_session:delete(Req1),
    ?assertNot(is_map_key(resp_cookie, Req2)).

renew_without_session() ->
    {ok, #{nova_session_id := NewId}} = nova_session:renew(nova_test_helper:mock_req(~"GET", ~"/")),
    ?assert(is_binary(NewId)).

%% Same rule as json_lib: the bootstrap application wins, then nova.
use_sessions_bootstrap_app_wins() ->
    application:set_env(nova, bootstrap_application, nova_session_tests_app),
    application:set_env(nova_session_tests_app, use_sessions, true),
    application:set_env(nova, use_sessions, false),
    try
        ?assert(nova_session:enabled()),
        application:unset_env(nova_session_tests_app, use_sessions),
        ?assertNot(nova_session:enabled()),
        application:unset_env(nova, use_sessions),
        ?assert(nova_session:enabled())
    after
        application:unset_env(nova_session_tests_app, use_sessions),
        application:set_env(nova, bootstrap_application, nova),
        application:set_env(nova, use_sessions, true)
    end.

known_presented_id_is_used() ->
    ok = nova_session_ets:set_value(~"known", ~"k", ~"v"),
    Req = presented_req(~"known", ~"pending"),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")),
    ok = nova_session:set(Req, ~"k2", ~"v2"),
    ?assertEqual({written, ~"known"}, stream_event()).

%% A client must not be able to pick its own session id.
unknown_presented_id_is_replaced() ->
    Req = presented_req(~"attacker-chosen", ~"pending"),
    ok = nova_session:set(Req, ~"k", ~"v"),
    ?assertEqual({written, ~"pending"}, stream_event()),
    ?assertNot(nova_session_ets:session_exists(~"attacker-chosen")),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")).

renew_drops_presented_id() ->
    ok = nova_session_ets:set_value(~"before", ~"k", ~"v"),
    {ok, Req1} = nova_session:renew(presented_req(~"before", ~"pending")),
    ?assertNot(maps:is_key(nova_session_presented, Req1)),
    ?assertNot(maps:is_key(nova_session_pending, Req1)),
    ?assertMatch({renewed, _}, stream_event()),
    ?assertEqual({error, not_found}, nova_session:get(Req1, ~"k")).

%% Only the request process tells the stream handler. A message from anywhere
%% else would reach whatever owns the connection by then, a websocket handler
%% for instance.
spawned_process_does_not_notify() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = stream_req(SId),
    Self = self(),
    _ = spawn_link(fun() -> Self ! {spawned, nova_session:set(Req, ~"k", ~"v")} end),
    receive {spawned, Result} -> ?assertEqual(ok, Result) after 1000 -> error(timeout) end,
    no_stream_event(),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")).

request_without_owner_does_not_notify() ->
    {ok, SId} = nova_session:generate_session_id(),
    ok = nova_session:set(maps:remove(nova_session_owner, stream_req(SId)), ~"k", ~"v"),
    no_stream_event().

%% Backends that answer true or {ok, _} wrote the value too.
non_error_set_results_notify() ->
    meck:new(fake_session_backend, [non_strict]),
    application:set_env(nova, session_manager, fake_session_backend),
    try
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> true end),
        ?assertEqual(true, nova_session:set(stream_req(~"a"), ~"k", ~"v")),
        ?assertEqual({written, ~"a"}, stream_event()),
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> {ok, stored} end),
        ?assertEqual({ok, stored}, nova_session:set(stream_req(~"b"), ~"k", ~"v")),
        ?assertEqual({written, ~"b"}, stream_event()),
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> {error, full} end),
        ?assertEqual({error, full}, nova_session:set(stream_req(~"c"), ~"k", ~"v")),
        no_stream_event()
    after
        application:set_env(nova, session_manager, nova_session_ets),
        meck:unload(fake_session_backend)
    end.

delete_key_preserves_other_keys() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"a">>, <<"1">>),
    ok = nova_session:set(Req, <<"b">>, <<"2">>),
    ok = nova_session:set(Req, <<"c">>, <<"3">>),
    sync(),
    {ok, _} = nova_session:delete(Req, <<"b">>),
    sync(),
    ?assertEqual({ok, <<"1">>}, nova_session:get(Req, <<"a">>)),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"b">>)),
    ?assertEqual({ok, <<"3">>}, nova_session:get(Req, <<"c">>)).

set_overwrites_existing_key() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"key">>, <<"old">>),
    sync(),
    ok = nova_session:set(Req, <<"key">>, <<"new">>),
    sync(),
    ?assertEqual({ok, <<"new">>}, nova_session:get(Req, <<"key">>)).

generate_session_id_format() ->
    {ok, Id} = nova_session:generate_session_id(),
    ?assert(is_binary(Id)),
    Decoded = base64:decode(Id),
    ?assertEqual(32, byte_size(Decoded)),
    {ok, Id2} = nova_session:generate_session_id(),
    ?assertNotEqual(Id, Id2).

%%--------------------------------------------------------------------
%% validate_cookie_config/0
%%--------------------------------------------------------------------

validate_cookie_config_test_() ->
    {foreach,
     fun() ->
             application:set_env(nova, bootstrap_application, nova),
             application:unset_env(nova, session_cookie_name),
             application:unset_env(nova, session_cookie_opts)
     end,
     fun(_) ->
             application:unset_env(nova, session_cookie_name),
             application:unset_env(nova, session_cookie_opts),
             application:unset_env(nova, bootstrap_application)
     end,
     [{"the default name is accepted",
       fun() -> ?assertEqual(ok, nova_session:validate_cookie_config()) end},
      {"an empty name is refused",
       fun() ->
               application:set_env(nova, session_cookie_name, <<>>),
               ?assertError({invalid_session_cookie_config, #{name := <<>>}},
                            nova_session:validate_cookie_config())
       end},
      {"a name that is not a binary is refused",
       fun() ->
               application:set_env(nova, session_cookie_name, "session"),
               ?assertError({invalid_session_cookie_config, #{name := "session"}},
                            nova_session:validate_cookie_config())
       end},
      {"the error names the cookie path and domain",
       fun() ->
               application:set_env(nova, session_cookie_name, ~"app:sid"),
               application:set_env(nova, session_cookie_opts, #{path => ~"/app", domain => ~"example.com"}),
               ?assertError({invalid_session_cookie_config,
                             #{name := ~"app:sid", path := ~"/app", domain := ~"example.com"}},
                            nova_session:validate_cookie_config())
       end},
      {"an invalid domain is refused and named",
       fun() ->
               application:set_env(nova, session_cookie_opts, #{domain => <<"a\nb">>}),
               ?assertError({invalid_session_cookie_config,
                             #{name := ~"session_id", path := ~"/", domain := <<"a\nb">>}},
                            nova_session:validate_cookie_config())
       end}]}.
