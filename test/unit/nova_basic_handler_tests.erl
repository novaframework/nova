-module(nova_basic_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

basic_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"JSON handler - simple", fun test_json_simple/0},
      {"JSON handler - POST method", fun test_json_post/0},
      {"JSON handler - with status and headers", fun test_json_with_status/0},
      {"Status handler - simple", fun test_status_simple/0},
      {"Status handler - with headers", fun test_status_with_headers/0},
      {"Status handler - with JSON body", fun test_status_with_json_body/0},
      {"Status handler - with binary body", fun test_status_with_binary_body/0},
      {"Redirect handler - binary path", fun test_redirect_binary/0},
      {"Redirect handler - list path", fun test_redirect_list/0},
      {"Sendfile handler", fun test_sendfile/0}
     ]}.

setup() ->
    %% Set JSON library
    application:set_env(nova, json_lib, thoas),
    ok.

cleanup(_) ->
    ok.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

dummy_callback(_) -> ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_json_simple() ->
    %% Test simple JSON response
    JSON = #{<<"message">> => <<"Hello, World!">>},
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_json({json, JSON}, fun dummy_callback/1, Req),

    %% Verify response
    ?assertEqual(200, maps:get(resp_status_code, Req1)),

    %% Verify body is encoded JSON
    Body = maps:get(resp_body, Req1),
    {ok, Decoded} = thoas:decode(Body),
    ?assertEqual(#{<<"message">> => <<"Hello, World!">>}, Decoded),

    %% Verify content-type header
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)).

test_json_post() ->
    %% Test JSON response for POST method (should return 201)
    JSON = #{<<"id">> => 123},
    Req = #{method => <<"POST">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_json({json, JSON}, fun dummy_callback/1, Req),

    %% Verify 201 status code for POST
    ?assertEqual(201, maps:get(resp_status_code, Req1)).

test_json_with_status() ->
    %% Test JSON with custom status code and headers
    JSON = #{<<"error">> => <<"Not found">>},
    Headers = #{<<"x-custom-header">> => <<"custom-value">>},
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_json({json, 404, Headers, JSON}, fun dummy_callback/1, Req),

    %% Verify status code
    ?assertEqual(404, maps:get(resp_status_code, Req1)),

    %% Verify custom header is set
    RespHeaders = maps:get(resp_headers, Req1),
    ?assertEqual(<<"custom-value">>, maps:get(<<"x-custom-header">>, RespHeaders)),

    %% Verify content-type is still set
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, RespHeaders)).

test_status_simple() ->
    %% Test simple status code
    Req = #{method => <<"GET">>, headers => #{}},

    %% Need to set up routing for status pages
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),

    {ok, Req1} = nova_basic_handler:handle_status({status, 404}, fun dummy_callback/1, Req),

    %% Verify status code is set
    ?assertEqual(404, maps:get(resp_status_code, Req1)).

test_status_with_headers() ->
    %% Test status code with custom headers
    Headers = #{<<"x-error">> => <<"Custom error">>},
    Req = #{method => <<"GET">>, headers => #{}},

    %% Set up routing
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),

    {ok, Req1} = nova_basic_handler:handle_status({status, 500, Headers}, fun dummy_callback/1, Req),

    %% Verify status code
    ?assertEqual(500, maps:get(resp_status_code, Req1)),

    %% Verify custom header
    RespHeaders = maps:get(resp_headers, Req1),
    ?assertEqual(<<"Custom error">>, maps:get(<<"x-error">>, RespHeaders)).

test_status_with_json_body() ->
    %% Test status code with JSON body
    JSON = #{<<"error">> => <<"Unauthorized">>},
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_status({status, 401, #{}, JSON}, fun dummy_callback/1, Req),

    %% Verify status code
    ?assertEqual(401, maps:get(resp_status_code, Req1)),

    %% Verify JSON body
    Body = maps:get(resp_body, Req1),
    {ok, Decoded} = thoas:decode(Body),
    ?assertEqual(#{<<"error">> => <<"Unauthorized">>}, Decoded),

    %% Verify content-type
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)).

test_status_with_binary_body() ->
    %% Test status code with binary body
    Body = <<"Custom error message">>,
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_status({status, 500, #{}, Body}, fun dummy_callback/1, Req),

    %% Verify status code
    ?assertEqual(500, maps:get(resp_status_code, Req1)),

    %% Verify body
    ?assertEqual(<<"Custom error message">>, maps:get(resp_body, Req1)).

test_redirect_binary() ->
    %% Test redirect with binary path
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_redirect({redirect, <<"/login">>}, fun dummy_callback/1, Req),

    %% Verify status code is 302
    ?assertEqual(302, maps:get(resp_status_code, Req1)),

    %% Verify location header
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"/login">>, maps:get(<<"location">>, Headers)).

test_redirect_list() ->
    %% Test redirect with list path
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_redirect({redirect, "/dashboard"}, fun dummy_callback/1, Req),

    %% Verify status code is 302
    ?assertEqual(302, maps:get(resp_status_code, Req1)),

    %% Verify location header (should be converted to binary)
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"/dashboard">>, maps:get(<<"location">>, Headers)).

test_sendfile() ->
    %% Test sendfile handler
    FilePath = "/path/to/file.pdf",
    Offset = 0,
    Length = 1024,
    Mime = <<"application/pdf">>,
    Req = #{method => <<"GET">>, headers => #{}},

    {ok, Req1} = nova_basic_handler:handle_sendfile(
                   {sendfile, 200, #{}, {Offset, Length, FilePath}, Mime},
                   fun dummy_callback/1,
                   Req),

    %% Verify status code
    ?assertEqual(200, maps:get(resp_status_code, Req1)),

    %% Verify sendfile body tuple
    ?assertEqual({sendfile, Offset, Length, FilePath}, maps:get(resp_body, Req1)),

    %% Verify content-type header
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(Mime, maps:get(<<"content-type">>, Headers)).
