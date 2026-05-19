-module(nova_force_ssl_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% HTTPS requests pass through
%%====================================================================

https_passes_through_test() ->
    Req = (nova_test_helper:mock_req(<<"GET">>, <<"/page">>))#{scheme => <<"https">>},
    {ok, Req1, state} = nova_force_ssl_plugin:pre_request(Req, #{}, #{}, state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% HTTP requests get redirected
%%====================================================================

http_redirects_to_https_test() ->
    Req = mock_http_req(<<"GET">>, <<"example.com">>, <<"/page">>, <<>>),
    {stop, _Reply, state} = nova_force_ssl_plugin:pre_request(Req, #{}, #{}, state).

%%====================================================================
%% Excluded paths skip redirect
%%====================================================================

excluded_path_passes_through_test() ->
    Req = mock_http_req(<<"GET">>, <<"example.com">>, <<"/health">>, <<>>),
    Opts = #{excluded_paths => [<<"/health">>]},
    {ok, _, state} = nova_force_ssl_plugin:pre_request(Req, #{}, Opts, state).

%%====================================================================
%% Query string preserved
%%====================================================================

query_string_preserved_test() ->
    Req = mock_http_req(<<"GET">>, <<"example.com">>, <<"/search">>, <<"q=test">>),
    meck:new(cowboy_req, [passthrough]),
    try
        meck:expect(cowboy_req, reply,
                    fun(301, #{<<"location">> := Location}, _Req) ->
                            ?assertEqual(<<"https://example.com/search?q=test">>, Location),
                            _Req
                    end),
        {stop, _, state} = nova_force_ssl_plugin:pre_request(Req, #{}, #{}, state)
    after
        meck:unload(cowboy_req)
    end.

%%====================================================================
%% Custom host and port
%%====================================================================

custom_host_and_port_test() ->
    Req = mock_http_req(<<"GET">>, <<"old.example.com">>, <<"/page">>, <<>>),
    Opts = #{host => <<"new.example.com">>, port => 8443},
    meck:new(cowboy_req, [passthrough]),
    try
        meck:expect(cowboy_req, reply,
                    fun(301, #{<<"location">> := Location}, _Req) ->
                            ?assertEqual(<<"https://new.example.com:8443/page">>, Location),
                            _Req
                    end),
        {stop, _, state} = nova_force_ssl_plugin:pre_request(Req, #{}, Opts, state)
    after
        meck:unload(cowboy_req)
    end.

%%====================================================================
%% post_request passthrough
%%====================================================================

post_request_passthrough_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req, state} = nova_force_ssl_plugin:post_request(Req, #{}, #{}, state).

%%====================================================================
%% plugin_info
%%====================================================================

plugin_info_test() ->
    Info = nova_force_ssl_plugin:plugin_info(),
    ?assert(is_binary(maps:get(title, Info))),
    ?assert(is_list(maps:get(options, Info))).

%%====================================================================
%% Helpers
%%====================================================================

mock_http_req(Method, Host, Path, Qs) ->
    (nova_test_helper:mock_req(Method, Path))#{scheme => <<"http">>,
                                                host => Host,
                                                qs => Qs}.
