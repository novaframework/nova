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
        Req = (req_without_cookie_header())#{nova_session_id => SessionId},
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
        Req = (req_without_cookie_header())#{nova_session_id => SessionId},
        ?assertEqual(ok, nova_session:set(Req, <<"key">>, <<"val">>))
    after
        cleanup_meck()
    end.

get_falls_back_to_cookie_test() ->
    setup_meck(),
    try
        SessionId = <<"cookie-session-id">>,
        meck:expect(nova_session_ets, get_value,
                    fun(SId, <<"key">>) when SId =:= SessionId -> {ok, <<"val">>} end),
        Req = req_with_cookie_header(<<"session_id=", SessionId/binary>>),
        ?assertEqual({ok, <<"val">>}, nova_session:get(Req, <<"key">>))
    after
        cleanup_meck()
    end.

get_returns_error_when_no_session_test() ->
    setup_meck(),
    try
        ?assertEqual([], cowboy_req:parse_cookies(req_without_cookie_header())),
        ?assertEqual({error, not_found}, nova_session:get(req_without_cookie_header(), <<"key">>)),
        ?assertEqual({error, not_found},
                     nova_session:get(req_with_cookie_header(~"other=cookie-session-id"), <<"key">>)),
        ?assertEqual(0, meck:num_calls(nova_session_ets, get_value, '_'))
    after
        cleanup_meck()
    end.

set_returns_error_when_no_session_test() ->
    setup_meck(),
    try
        ?assertEqual({error, session_id_not_set},
                     nova_session:set(req_without_cookie_header(), <<"key">>, <<"val">>)),
        ?assertEqual(0, meck:num_calls(nova_session_ets, set_value, '_'))
    after
        cleanup_meck()
    end.

get_returns_error_when_cookie_header_does_not_parse_test() ->
    setup_meck(),
    try
        Req = req_with_cookie_header(~"session_id=cookie-session-id; =x"),
        ?assertExit({request_error, {header, ~"cookie"}, _}, cowboy_req:parse_cookies(Req)),
        ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)),
        ?assertEqual(0, meck:num_calls(nova_session_ets, get_value, '_'))
    after
        cleanup_meck()
    end.

nova_session_id_takes_priority_over_cookie_test() ->
    setup_meck(),
    try
        ReqSessionId = <<"req-session-id">>,
        meck:expect(nova_session_ets, get_value,
                    fun(SId, <<"key">>) when SId =:= ReqSessionId -> {ok, <<"val">>} end),
        Req = (req_with_cookie_header(~"session_id=cookie-session-id"))#{nova_session_id => ReqSessionId},
        ?assertEqual({ok, <<"val">>}, nova_session:get(Req, <<"key">>))
    after
        cleanup_meck()
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% Only the headers and the session id matter to nova_session here.
-spec req_without_cookie_header() -> dynamic().
req_without_cookie_header() ->
    #{headers => #{}}.

-spec req_with_cookie_header(binary()) -> dynamic().
req_with_cookie_header(Cookie) ->
    #{headers => #{~"cookie" => Cookie}}.

setup_meck() ->
    meck:new(nova_session_ets, [passthrough]),
    application:set_env(nova, session_manager, nova_session_ets).

cleanup_meck() ->
    meck:unload(nova_session_ets).
