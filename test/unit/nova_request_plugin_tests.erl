-module(nova_request_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

request_plugin_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Plugin info", fun test_plugin_info/0},
      {"Pre-request - no body", fun test_pre_request_no_body/0},
      {"Pre-request - decode JSON body", fun test_pre_request_json/0},
      {"Pre-request - decode JSON body (empty)", fun test_pre_request_json_empty/0},
      {"Pre-request - invalid JSON", fun test_pre_request_invalid_json/0},
      {"Pre-request - JSON with GET method", fun test_pre_request_json_get/0},
      {"Pre-request - URL encoded body", fun test_pre_request_urlencoded/0},
      {"Pre-request - parse query string", fun test_pre_request_parse_qs/0},
      {"Post-request - passthrough", fun test_post_request/0}
     ]}.

setup() ->
    %% Mock nova:get_env for JSON library
    meck:new(nova, [passthrough]),
    meck:expect(nova, get_env, fun(json_lib, Default) -> Default;
                                  (Key, Default) -> meck:passthrough([Key, Default])
                               end),

    %% Mock cowboy_req for body reading
    meck:new(cowboy_req, [passthrough]),
    ok.

cleanup(_) ->
    meck:unload(),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_plugin_info() ->
    Info = nova_request_plugin:plugin_info(),

    %% Check all required fields
    ?assertMatch(#{title := <<"Nova body plugin">>,
                   version := _,
                   url := _,
                   authors := _,
                   description := _,
                   options := _}, Info).

test_pre_request_no_body() ->
    %% Request without body and no special options
    meck:expect(cowboy_req, has_body, fun(_Req) -> false end),

    Req = #{method => <<"GET">>, headers => #{}},
    Options = #{},

    {ok, Req1, State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should add empty body
    ?assertEqual(<<>>, maps:get(body, Req1)),
    ?assertEqual(#{}, State).

test_pre_request_json() ->
    %% Request with JSON body
    JsonBody = <<"{\"name\":\"test\",\"value\":123}">>,

    meck:expect(cowboy_req, has_body, fun(_Req) -> true end),
    meck:expect(cowboy_req, read_body, fun(_Req) ->
                                               {ok, JsonBody, #{headers => #{<<"content-type">> => <<"application/json">>}}}
                                       end),

    Req = #{method => <<"POST">>,
            headers => #{<<"content-type">> => <<"application/json">>}},
    Options = #{decode_json_body => true},

    {ok, Req1, _State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should have decoded JSON
    ?assertMatch(#{json := #{<<"name">> := <<"test">>, <<"value">> := 123}}, Req1).

test_pre_request_json_empty() ->
    %% Request with empty JSON body should return 400
    meck:expect(cowboy_req, has_body, fun(_Req) -> true end),
    meck:expect(cowboy_req, read_body, fun(Req) ->
                                               {ok, <<>>, Req#{body => <<>>}}
                                       end),
    meck:expect(cowboy_req, reply, fun(400, Req) -> Req#{replied => true} end),

    Req = #{method => <<"POST">>,
            headers => #{<<"content-type">> => <<"application/json">>}},
    Options = #{decode_json_body => true},

    {stop, Req1, _State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should have replied with 400
    ?assertMatch(#{replied := true}, Req1).

test_pre_request_invalid_json() ->
    %% Request with invalid JSON should return 400
    InvalidJson = <<"{invalid json}">>,

    meck:expect(cowboy_req, has_body, fun(_Req) -> true end),
    meck:expect(cowboy_req, read_body, fun(Req) ->
                                               {ok, InvalidJson, Req}
                                       end),
    meck:expect(cowboy_req, reply, fun(400, Req) -> Req#{replied => true} end),

    Req = #{method => <<"POST">>,
            headers => #{<<"content-type">> => <<"application/json">>}},
    Options = #{decode_json_body => true},

    {stop, Req1, _State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should have replied with 400
    ?assertMatch(#{replied := true}, Req1).

test_pre_request_json_get() ->
    %% GET request with decode_json_body option should skip body reading
    meck:expect(cowboy_req, has_body, fun(_Req) -> false end),

    Req = #{method => <<"GET">>, headers => #{}},
    Options = #{decode_json_body => true},

    {ok, Req1, _State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should just add empty body, not try to decode
    ?assertEqual(<<>>, maps:get(body, Req1)),
    ?assertNot(maps:is_key(json, Req1)).

test_pre_request_urlencoded() ->
    %% Request with URL-encoded form data
    FormBody = <<"name=test&value=123">>,

    meck:expect(cowboy_req, has_body, fun(_Req) -> true end),
    meck:expect(cowboy_req, read_body, fun(Req) ->
                                               {ok, FormBody, Req}
                                       end),

    Req = #{method => <<"POST">>,
            headers => #{<<"content-type">> => <<"application/x-www-form-urlencoded">>}},
    Options = #{read_urlencoded_body => true},

    {ok, Req1, _State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should have parsed parameters
    ?assertMatch(#{params := #{<<"name">> := <<"test">>, <<"value">> := <<"123">>}}, Req1).

test_pre_request_parse_qs() ->
    %% Parse query string from URL
    meck:expect(cowboy_req, has_body, fun(_Req) -> false end),
    meck:expect(cowboy_req, parse_qs, fun(_Req) ->
                                              [{<<"page">>, <<"1">>}, {<<"limit">>, <<"10">>}]
                                      end),

    Req = #{method => <<"GET">>, headers => #{}},
    Options = #{parse_qs => true},

    {ok, Req1, _State} = nova_request_plugin:pre_request(Req, #{}, Options, #{}),

    %% Should have parsed query string as map
    ?assertMatch(#{parsed_qs := #{<<"page">> := <<"1">>, <<"limit">> := <<"10">>}}, Req1).

test_post_request() ->
    %% Post-request should just pass through
    Req = #{method => <<"GET">>},

    {ok, Req1, State} = nova_request_plugin:post_request(Req, #{}, #{}, #{}),

    ?assertEqual(Req, Req1),
    ?assertEqual(#{}, State).
