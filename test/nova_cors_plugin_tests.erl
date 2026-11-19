-module(nova_cors_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% pre_request/4 — non-OPTIONS adds CORS headers and continues
%%====================================================================

pre_request_non_options_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{allow_origins => <<"*">>},
    {ok, Req1, my_state} = nova_cors_plugin:pre_security(Req, #{}, Opts, my_state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Headers">>, Headers)),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Methods">>, Headers)).

pre_request_specific_origin_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/api">>),
    Opts = #{allow_origins => <<"https://example.com">>},
    {ok, Req1, _} = nova_cors_plugin:pre_security(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"https://example.com">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)).

%%====================================================================
%% pre_request/4: OPTIONS returns {stop, ...} with 204
%%====================================================================

pre_request_options_stop_test() ->
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, reply, fun(Status, R) -> R#{replied => Status} end),
    try
        Opts = #{allow_origins => ~"*"},
        {stop, Req1, my_state} = nova_cors_plugin:pre_security(preflight(~"https://a.example"), #{}, Opts,
                                                              my_state),
        ?assertEqual(204, maps:get(replied, Req1))
    after
        meck:unload(cowboy_req)
    end.

%% An OPTIONS request without Origin and Access-Control-Request-Method is not
%% a preflight; the application answers it.
pre_request_plain_options_reaches_app_test() ->
    Opts = #{allow_origins => ~"*"},
    NoMethod = with_origin(~"https://a.example", nova_test_helper:mock_req(~"OPTIONS", ~"/")),
    ?assertMatch({ok, _, state}, nova_cors_plugin:pre_security(NoMethod, #{}, Opts, state)),
    NoOrigin = nova_test_helper:with_header(~"access-control-request-method", ~"POST",
                                            nova_test_helper:mock_req(~"OPTIONS", ~"/")),
    ?assertMatch({ok, _, state}, nova_cors_plugin:pre_security(NoOrigin, #{}, Opts, state)).

preflight(Origin) ->
    nova_test_helper:with_header(~"access-control-request-method", ~"POST",
                                 with_origin(Origin, nova_test_helper:mock_req(~"OPTIONS", ~"/"))).

with_origin(Origin, Req) ->
    nova_test_helper:with_header(~"origin", Origin, Req).

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
