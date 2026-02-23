-module(nova_cors_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% pre_request/4 — non-OPTIONS adds CORS headers and continues
%%====================================================================

pre_request_non_options_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{allow_origins => <<"*">>},
    {ok, Req1, my_state} = nova_cors_plugin:pre_request(Req, #{}, Opts, my_state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Headers">>, Headers)),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Methods">>, Headers)).

pre_request_specific_origin_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/api">>),
    Opts = #{allow_origins => <<"https://example.com">>},
    {ok, Req1, _} = nova_cors_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"https://example.com">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)).

%%====================================================================
%% pre_request/4 — OPTIONS returns {stop, ...} with 200
%%====================================================================

pre_request_options_stop_test() ->
    Req = nova_test_helper:mock_req(<<"OPTIONS">>, <<"/">>),
    Opts = #{allow_origins => <<"*">>},
    {stop, Req1, my_state} = nova_cors_plugin:pre_request(Req, #{}, Opts, my_state),
    %% cowboy_req:reply/2 sets resp_status_code on the req
    ?assert(is_map(Req1)).

%%====================================================================
%% post_request/4 — passes through unchanged
%%====================================================================

post_request_passthrough_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, state} = nova_cors_plugin:post_request(Req, #{}, #{}, state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% plugin_info/0
%%====================================================================

plugin_info_test() ->
    Info = nova_cors_plugin:plugin_info(),
    ?assertEqual(<<"nova_cors_plugin">>, maps:get(title, Info)),
    ?assert(is_binary(maps:get(version, Info))),
    ?assert(is_binary(maps:get(description, Info))),
    ?assert(is_list(maps:get(options, Info))).
