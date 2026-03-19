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
    meck:expect(cowboy_req, match_cookies,
                fun([{session_id, [], undefined}], Req) ->
                    SId = maps:get(cookie_session_id, Req, undefined),
                    #{session_id => SId}
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
      fun delete_sets_expired_cookie/0,
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

delete_sets_expired_cookie() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"key">>, <<"val">>),
    sync(),
    {ok, Req1} = nova_session:delete(Req),
    ?assertMatch(#{resp_cookie := #{name := <<"session_id">>, opts := #{max_age := 0}}}, Req1).

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
