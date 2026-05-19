-module(nova_secure_headers_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Default headers
%%====================================================================

default_headers_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, #{}, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"DENY">>, maps:get(<<"x-frame-options">>, Headers)),
    ?assertEqual(<<"nosniff">>, maps:get(<<"x-content-type-options">>, Headers)),
    ?assertEqual(<<"1; mode=block">>, maps:get(<<"x-xss-protection">>, Headers)),
    ?assertEqual(<<"strict-origin-when-cross-origin">>, maps:get(<<"referrer-policy">>, Headers)),
    ?assertMatch(<<"geolocation=()", _/binary>>, maps:get(<<"permissions-policy">>, Headers)).

%%====================================================================
%% Header overrides
%%====================================================================

override_headers_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{headers => #{<<"x-frame-options">> => <<"SAMEORIGIN">>}},
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"SAMEORIGIN">>, maps:get(<<"x-frame-options">>, Headers)).

extra_headers_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{extra_headers => #{<<"x-custom">> => <<"value">>}},
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"value">>, maps:get(<<"x-custom">>, Headers)),
    ?assertEqual(<<"DENY">>, maps:get(<<"x-frame-options">>, Headers)).

%%====================================================================
%% HSTS
%%====================================================================

hsts_disabled_by_default_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, #{}, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(error, maps:find(<<"strict-transport-security">>, Headers)).

hsts_enabled_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{hsts => true},
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"max-age=31536000; includeSubDomains">>,
                 maps:get(<<"strict-transport-security">>, Headers)).

hsts_custom_max_age_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{hsts => true, hsts_max_age => 86400, hsts_include_subdomains => false},
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"max-age=86400">>,
                 maps:get(<<"strict-transport-security">>, Headers)).

%%====================================================================
%% CSP
%%====================================================================

csp_not_set_by_default_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, #{}, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(error, maps:find(<<"content-security-policy">>, Headers)).

csp_set_when_configured_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Policy = <<"default-src 'self'">>,
    Opts = #{csp => Policy},
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(Policy, maps:get(<<"content-security-policy">>, Headers)).

csp_explicitly_disabled_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Opts = #{csp => false},
    {ok, Req1, _} = nova_secure_headers_plugin:pre_request(Req, #{}, Opts, state),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(error, maps:find(<<"content-security-policy">>, Headers)).

%%====================================================================
%% post_request passthrough
%%====================================================================

post_request_passthrough_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, state} = nova_secure_headers_plugin:post_request(Req, #{}, #{}, state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% plugin_info
%%====================================================================

plugin_info_test() ->
    Info = nova_secure_headers_plugin:plugin_info(),
    ?assertEqual(<<"Nova Secure Headers Plugin">>, maps:get(title, Info)),
    ?assert(is_binary(maps:get(version, Info))),
    ?assert(is_list(maps:get(options, Info))).
