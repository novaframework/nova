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
    ?assertEqual(~"*", maps:get(~"access-control-allow-origin", Headers)),
    ?assertEqual(~"*", maps:get(~"access-control-allow-headers", Headers)),
    ?assertEqual(~"*", maps:get(~"access-control-allow-methods", Headers)),
    ?assertNot(maps:is_key(~"vary", Headers)).

pre_request_specific_origin_test() ->
    Req = with_origin(~"https://example.com", nova_test_helper:mock_req(~"POST", ~"/api")),
    Opts = #{allow_origins => <<"https://example.com">>},
    {ok, Req1, _} = nova_cors_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(~"https://example.com", maps:get(~"access-control-allow-origin", Headers)),
    ?assertEqual(~"origin", maps:get(~"vary", Headers)).

%% Only an origin on the list is echoed back.
pre_request_origin_allowlist_test() ->
    Opts = #{allow_origins => [~"https://a.example", "https://b.example"]},
    Allowed = cors_headers(with_origin(~"https://b.example", nova_test_helper:mock_req(~"GET", ~"/")), Opts),
    ?assertEqual(~"https://b.example", maps:get(~"access-control-allow-origin", Allowed)),
    Refused = cors_headers(with_origin(~"https://evil.example", nova_test_helper:mock_req(~"GET", ~"/")), Opts),
    ?assertNot(maps:is_key(~"access-control-allow-origin", Refused)),
    ?assertEqual(~"origin", maps:get(~"vary", Refused)).

pre_request_string_origin_test() ->
    Headers = cors_headers(with_origin(~"https://a.example", nova_test_helper:mock_req(~"GET", ~"/")),
                           #{allow_origins => "https://a.example"}),
    ?assertEqual(~"https://a.example", maps:get(~"access-control-allow-origin", Headers)).

%% "*" does not cover Authorization, so the requested headers are allowed.
pre_request_allows_requested_headers_test() ->
    Req0 = with_origin(~"https://a.example", nova_test_helper:mock_req(~"OPTIONS", ~"/")),
    Req = nova_test_helper:with_header(~"access-control-request-headers", ~"authorization, content-type", Req0),
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, reply, fun(_Status, R) -> R end),
    try
        {stop, Req1, state} = nova_cors_plugin:pre_request(Req, #{}, #{allow_origins => [~"https://a.example"]},
                                                           state),
        Headers = maps:get(resp_headers, Req1),
        ?assertEqual(~"authorization, content-type", maps:get(~"access-control-allow-headers", Headers))
    after
        meck:unload(cowboy_req)
    end.

pre_request_configured_headers_test() ->
    Headers = cors_headers(nova_test_helper:mock_req(~"GET", ~"/"),
                           #{allow_origins => ~"*", allow_headers => [~"authorization", ~"x-api-key"],
                             allow_methods => [~"GET", ~"POST"], max_age => 600}),
    ?assertEqual(~"authorization, x-api-key", maps:get(~"access-control-allow-headers", Headers)),
    ?assertEqual(~"GET, POST", maps:get(~"access-control-allow-methods", Headers)),
    ?assertEqual(~"600", maps:get(~"access-control-max-age", Headers)).

%%====================================================================
%% pre_request/4: OPTIONS returns {stop, ...} with 204
%%====================================================================

pre_request_options_stop_test() ->
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, reply, fun(Status, R) -> R#{replied => Status} end),
    try
        Req = nova_test_helper:mock_req(~"OPTIONS", ~"/"),
        Opts = #{allow_origins => ~"*"},
        {stop, Req1, my_state} = nova_cors_plugin:pre_request(Req, #{}, Opts, my_state),
        ?assertEqual(204, maps:get(replied, Req1))
    after
        meck:unload(cowboy_req)
    end.

cors_headers(Req, Opts) ->
    {ok, Req1, _} = nova_cors_plugin:pre_request(Req, #{}, Opts, state),
    maps:get(resp_headers, Req1).

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
