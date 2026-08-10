-module(nova_session_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for nova_session_id fallback in get_session_id/1
%%
%% When nova_stream_h creates a new session, it injects nova_session_id
%% into the Req map. nova_session should use this before falling back
%% to cookie lookup.
%%====================================================================

get_uses_nova_session_id_from_req_test() ->
    setup_meck(),
    try
        SessionId = <<"new-session-id">>,
        meck:expect(nova_session_ets, get_value,
                    fun(SId, <<"key">>) when SId =:= SessionId -> {ok, <<"val">>} end),
        Req = #{nova_session_id => SessionId},
        ?assertEqual({ok, <<"val">>}, nova_session:get(Req, <<"key">>))
    after
        cleanup_meck()
    end.

set_uses_nova_session_id_from_req_test() ->
    setup_meck(),
    try
        SessionId = <<"new-session-id">>,
        meck:expect(nova_session_ets, set_value,
                    fun(SId, <<"key">>, <<"val">>) when SId =:= SessionId -> ok end),
        Req = #{nova_session_id => SessionId},
        ?assertEqual(ok, nova_session:set(Req, <<"key">>, <<"val">>))
    after
        cleanup_meck()
    end.

get_falls_back_to_cookie_test() ->
    setup_meck(),
    try
        SessionId = <<"cookie-session-id">>,
        meck:expect(cowboy_req, match_cookies,
                    fun([{session_id, [], undefined}], _Req) -> #{session_id => SessionId} end),
        meck:expect(nova_session_ets, get_value,
                    fun(SId, <<"key">>) when SId =:= SessionId -> {ok, <<"val">>} end),
        Req = #{},
        ?assertEqual({ok, <<"val">>}, nova_session:get(Req, <<"key">>))
    after
        cleanup_meck()
    end.

get_returns_error_when_no_session_test() ->
    setup_meck(),
    try
        meck:expect(cowboy_req, match_cookies,
                    fun([{session_id, [], undefined}], _Req) -> #{session_id => undefined} end),
        Req = #{},
        ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>))
    after
        cleanup_meck()
    end.

set_returns_error_when_no_session_test() ->
    setup_meck(),
    try
        meck:expect(cowboy_req, match_cookies,
                    fun([{session_id, [], undefined}], _Req) -> #{session_id => undefined} end),
        Req = #{},
        ?assertEqual({error, session_id_not_set}, nova_session:set(Req, <<"key">>, <<"val">>))
    after
        cleanup_meck()
    end.

nova_session_id_takes_priority_over_cookie_test() ->
    setup_meck(),
    try
        ReqSessionId = <<"req-session-id">>,
        %% cowboy_req:match_cookies should NOT be called since nova_session_id is present
        meck:expect(cowboy_req, match_cookies,
                    fun(_, _) -> error(should_not_be_called) end),
        meck:expect(nova_session_ets, get_value,
                    fun(SId, <<"key">>) when SId =:= ReqSessionId -> {ok, <<"val">>} end),
        Req = #{nova_session_id => ReqSessionId},
        ?assertEqual({ok, <<"val">>}, nova_session:get(Req, <<"key">>))
    after
        cleanup_meck()
    end.

%%====================================================================
%% Helpers
%%====================================================================

setup_meck() ->
    meck:new(nova, [passthrough]),
    meck:new(cowboy_req, [passthrough]),
    meck:new(nova_session_ets, [passthrough]),
    meck:expect(nova, get_env, fun(use_sessions, true) -> true end),
    application:set_env(nova, session_manager, nova_session_ets).

cleanup_meck() ->
    meck:unload(nova_session_ets),
    meck:unload(cowboy_req),
    meck:unload(nova).
