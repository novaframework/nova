-module(nova_cors_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

cors_plugin_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Pre-request adds CORS headers", fun test_pre_request_adds_headers/0},
      {"Pre-request handles OPTIONS request", fun test_pre_request_options/0},
      {"Pre-request with custom origin", fun test_pre_request_custom_origin/0},
      {"Post-request passthrough", fun test_post_request/0},
      {"Plugin info", fun test_plugin_info/0}
     ]}.

setup() ->
    %% Mock cowboy_req functions
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, set_resp_header, fun(Header, Value, Req) ->
                                                     Headers = maps:get(resp_headers, Req, #{}),
                                                     Req#{resp_headers => Headers#{Header => Value}}
                                             end),
    meck:expect(cowboy_req, reply, fun(StatusCode, Req) ->
                                           Req#{replied => StatusCode}
                                   end),
    ok.

cleanup(_) ->
    catch meck:unload(cowboy_req),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_pre_request_adds_headers() ->
    %% Test that pre_request adds CORS headers
    Req = #{method => <<"GET">>, path => <<"/">>, resp_headers => #{}},
    Env = #{},
    Options = #{allow_origins => <<"*">>},
    State = #{},

    {ok, Req1, State1} = nova_cors_plugin:pre_request(Req, Env, Options, State),

    %% Verify CORS headers were added
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Headers">>, Headers)),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Methods">>, Headers)),

    %% State should be unchanged
    ?assertEqual(State, State1).

test_pre_request_options() ->
    %% Test that OPTIONS requests stop the chain
    Req = #{method => <<"OPTIONS">>, path => <<"/">>, resp_headers => #{}},
    Env = #{},
    Options = #{allow_origins => <<"*">>},
    State = #{},

    {stop, Req1, State1} = nova_cors_plugin:pre_request(Req, Env, Options, State),

    %% Verify it replied with 200
    ?assertEqual(200, maps:get(replied, Req1)),

    %% Verify CORS headers were still added
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"*">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)).

test_pre_request_custom_origin() ->
    %% Test with custom allowed origins
    Req = #{method => <<"GET">>, path => <<"/">>, resp_headers => #{}},
    Env = #{},
    Options = #{allow_origins => <<"https://example.com">>},
    State = #{},

    {ok, Req1, _State1} = nova_cors_plugin:pre_request(Req, Env, Options, State),

    %% Verify custom origin was set
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"https://example.com">>, maps:get(<<"Access-Control-Allow-Origin">>, Headers)).

test_post_request() ->
    %% Test that post_request is a passthrough
    Req = #{method => <<"GET">>, path => <<"/">>},
    Env = #{},
    Options = #{},
    State = #{test => data},

    {ok, Req1, State1} = nova_cors_plugin:post_request(Req, Env, Options, State),

    %% Should return request and state unchanged
    ?assertEqual(Req, Req1),
    ?assertEqual(State, State1).

test_plugin_info() ->
    %% Test plugin info metadata
    Info = nova_cors_plugin:plugin_info(),

    %% Verify it returns required fields
    ?assertEqual(<<"nova_cors_plugin">>, maps:get(title, Info)),
    ?assertEqual(<<"0.2.0">>, maps:get(version, Info)),
    ?assert(is_list(maps:get(authors, Info))),
    ?assert(is_binary(maps:get(description, Info))),
    ?assert(is_list(maps:get(options, Info))).
