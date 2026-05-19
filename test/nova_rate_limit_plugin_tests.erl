-module(nova_rate_limit_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TABLE, nova_rate_limit_entries).

%%====================================================================
%% Setup / teardown
%%====================================================================

setup() ->
    nova_rate_limit_plugin:init().

cleanup() ->
    catch ets:delete(?TABLE).

%%====================================================================
%% Tests
%%====================================================================

allows_requests_under_limit_test() ->
    setup(),
    try
        Req = req_with_peer(<<"GET">>, <<"/api">>, {{127, 0, 0, 1}, 12345}),
        Opts = #{max_requests => 5, window_ms => 60000},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state)
    after
        cleanup()
    end.

blocks_requests_over_limit_test() ->
    setup(),
    try
        Req = req_with_peer(<<"GET">>, <<"/api">>, {{10, 0, 0, 1}, 9999}),
        Opts = #{max_requests => 3, window_ms => 60000},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        {stop, {reply, 429, _, _}, _, _} =
            nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state)
    after
        cleanup()
    end.

skips_unmatched_paths_test() ->
    setup(),
    try
        Req = req_with_peer(<<"GET">>, <<"/public/index">>, {{127, 0, 0, 1}, 5555}),
        Opts = #{max_requests => 1, window_ms => 60000, paths => [<<"/api">>]},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state)
    after
        cleanup()
    end.

limits_matching_paths_test() ->
    setup(),
    try
        Req = req_with_peer(<<"POST">>, <<"/api/login">>, {{192, 168, 1, 1}, 4444}),
        Opts = #{max_requests => 1, window_ms => 60000, paths => [<<"/api">>]},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        {stop, {reply, 429, _, _}, _, _} =
            nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state)
    after
        cleanup()
    end.

custom_key_fun_test() ->
    setup(),
    try
        Req1 = (req_with_peer(<<"GET">>, <<"/api">>, {{127, 0, 0, 1}, 1111}))#{
            headers => #{<<"x-api-key">> => <<"client-a">>}},
        KeyFun = fun(#{headers := #{<<"x-api-key">> := K}}) -> K end,
        Opts = #{max_requests => 1, window_ms => 60000, key_fun => KeyFun},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req1, #{}, Opts, state),
        {stop, {reply, 429, _, _}, _, _} =
            nova_rate_limit_plugin:pre_request(Req1, #{}, Opts, state)
    after
        cleanup()
    end.

different_ips_tracked_separately_test() ->
    setup(),
    try
        Req1 = req_with_peer(<<"GET">>, <<"/api">>, {{10, 0, 0, 1}, 1111}),
        Req2 = req_with_peer(<<"GET">>, <<"/api">>, {{10, 0, 0, 2}, 2222}),
        Opts = #{max_requests => 1, window_ms => 60000},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req1, #{}, Opts, state),
        {stop, {reply, 429, _, _}, _, _} =
            nova_rate_limit_plugin:pre_request(Req1, #{}, Opts, state),
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req2, #{}, Opts, state)
    after
        cleanup()
    end.

retry_after_header_present_test() ->
    setup(),
    try
        Req = req_with_peer(<<"GET">>, <<"/api">>, {{10, 0, 0, 3}, 3333}),
        Opts = #{max_requests => 1, window_ms => 60000},
        {ok, _, _} = nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        {stop, {reply, 429, Headers, _}, _, _} =
            nova_rate_limit_plugin:pre_request(Req, #{}, Opts, state),
        ?assert(lists:keymember(<<"retry-after">>, 1, Headers))
    after
        cleanup()
    end.

post_request_passthrough_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req, state} = nova_rate_limit_plugin:post_request(Req, #{}, #{}, state).

plugin_info_test() ->
    Info = nova_rate_limit_plugin:plugin_info(),
    ?assertEqual(<<"Nova Rate Limit Plugin">>, maps:get(title, Info)),
    ?assert(is_list(maps:get(options, Info))).

%%====================================================================
%% Helpers
%%====================================================================

req_with_peer(Method, Path, Peer) ->
    Req = nova_test_helper:mock_req(Method, Path),
    Req#{peer => Peer}.
