-module(nova_request_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> nova_test_helper:setup_nova_env() end).
-define(CLEANUP, fun(Prev) -> nova_test_helper:cleanup_nova_env(Prev) end).

%%====================================================================
%% should_read_body/1
%%====================================================================

should_read_body_empty_test() ->
    ?assertNot(nova_request_plugin:should_read_body([])).

should_read_body_decode_json_test() ->
    ?assert(nova_request_plugin:should_read_body([{decode_json_body, true}])).

should_read_body_urlencoded_test() ->
    ?assert(nova_request_plugin:should_read_body([{read_urlencoded_body, true}])).

should_read_body_unknown_test() ->
    ?assertNot(nova_request_plugin:should_read_body([{parse_qs, true}])).

should_read_body_mixed_test() ->
    ?assert(nova_request_plugin:should_read_body([{parse_qs, true}, {decode_json_body, true}])).

%%====================================================================
%% modulate_state/3 — skip JSON decode for GET/DELETE
%%====================================================================

modulate_state_skip_json_get_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
         {ok, _Req1, state} = nova_request_plugin:modulate_state(
             Req, [{decode_json_body, true}], state)
     end}.

modulate_state_skip_json_delete_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"DELETE">>, <<"/">>),
         {ok, _Req1, state} = nova_request_plugin:modulate_state(
             Req, [{decode_json_body, true}], state)
     end}.

%%====================================================================
%% modulate_state/3 — skip unknown options
%%====================================================================

modulate_state_skip_unknown_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
    {ok, Req1, state} = nova_request_plugin:modulate_state(
        Req, [{unknown_option, true}], state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% modulate_state/3 — empty options
%%====================================================================

modulate_state_empty_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, state} = nova_request_plugin:modulate_state(Req, [], state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% modulate_state/3 — parse_qs
%%====================================================================

modulate_state_parse_qs_true_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Req1 = Req#{qs => <<"foo=bar&baz=qux">>},
    {ok, Req2, state} = nova_request_plugin:modulate_state(
        Req1, [{parse_qs, true}], state),
    Parsed = maps:get(parsed_qs, Req2),
    ?assertEqual(<<"bar">>, maps:get(<<"foo">>, Parsed)),
    ?assertEqual(<<"qux">>, maps:get(<<"baz">>, Parsed)).

modulate_state_parse_qs_list_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Req1 = Req#{qs => <<"a=1&b=2">>},
    {ok, Req2, state} = nova_request_plugin:modulate_state(
        Req1, [{parse_qs, list}], state),
    Parsed = maps:get(parsed_qs, Req2),
    ?assert(is_list(Parsed)),
    ?assertEqual(2, length(Parsed)).

%%====================================================================
%% modulate_state/3 — decode_json_body on POST with body
%%====================================================================

modulate_state_decode_json_post_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
         Req1 = nova_test_helper:with_json_body(#{<<"name">> => <<"test">>}, Req),
         {ok, Req2, state} = nova_request_plugin:modulate_state(
             Req1, [{decode_json_body, true}], state),
         JSON = maps:get(json, Req2),
         ?assertEqual(<<"test">>, maps:get(<<"name">>, JSON))
     end}.

%% {json_lib, json} (OTP json) returns the term itself instead of {ok, Term};
%% every JSON body used to be answered with a 400.
modulate_state_decode_json_with_otp_json_test_() ->
    {setup,
     fun() -> Prev = nova_test_helper:setup_nova_env(), application:set_env(nova, json_lib, json), Prev end,
     fun(Prev) -> application:set_env(nova, json_lib, thoas), nova_test_helper:cleanup_nova_env(Prev) end,
     fun() ->
         Req = nova_test_helper:with_json_body(~"{\"name\":\"test\"}",
                                               nova_test_helper:mock_req(~"POST", ~"/")),
         {ok, Req1, state} = nova_request_plugin:modulate_state(Req, [{decode_json_body, true}], state),
         ?assertEqual(#{~"name" => ~"test"}, maps:get(json, Req1))
     end}.

%%====================================================================
%% modulate_state/3 — decode_json_body on POST with empty body -> stop
%%====================================================================

modulate_state_decode_json_empty_body_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
         Req1 = nova_test_helper:with_content_type(<<"application/json">>, Req),
         Req2 = Req1#{body => <<>>},
         {stop, _, state} = nova_request_plugin:modulate_state(
             Req2, [{decode_json_body, true}], state)
     end}.

%% Media types are case-insensitive; a byte prefix match missed these.
modulate_state_content_type_case_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Post = fun(ContentType, Body) ->
                        (nova_test_helper:with_content_type(ContentType, nova_test_helper:mock_req(~"POST", ~"/")))#{body => Body}
                end,
         {ok, Json, state} = nova_request_plugin:modulate_state(
                               Post(~"Application/JSON; charset=utf-8", ~"{\"a\":1}"), [{decode_json_body, true}], state),
         ?assertEqual(#{~"a" => 1}, maps:get(json, Json)),
         {ok, Form, state} = nova_request_plugin:modulate_state(
                               Post(~"Application/X-WWW-Form-Urlencoded", ~"a=1"), [{read_urlencoded_body, true}], state),
         ?assertEqual(#{~"a" => ~"1"}, maps:get(params, Form)),
         {ok, Other, state} = nova_request_plugin:modulate_state(
                                Post(~"text/plain", ~"{}"), [{decode_json_body, true}], state),
         ?assertNot(maps:is_key(json, Other))
     end}.

%%====================================================================
%% plugin_info/0
%%====================================================================

plugin_info_test() ->
    Info = nova_request_plugin:plugin_info(),
    ?assertEqual(<<"Nova body plugin">>, maps:get(title, Info)),
    ?assert(is_list(maps:get(options, Info))).

%%====================================================================
%% multipart/form-data is never buffered here
%%====================================================================

multipart_req(ContentType) ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/upload">>),
    Req0 = nova_test_helper:with_content_type(ContentType, Req),
    Req0#{has_body => true}.

multipart_body_not_drained_test_() ->
    {setup,
     fun() ->
         meck:new(cowboy_req, [passthrough]),
         meck:expect(cowboy_req, has_body, fun(_Req) -> true end),
         meck:expect(cowboy_req, read_body,
                     fun(_Req, _Opts) -> erlang:error(body_should_not_be_read) end)
     end,
     fun(_) -> meck:unload(cowboy_req) end,
     [fun() ->
          Req0 = multipart_req(<<"multipart/form-data; boundary=----abc">>),
          {ok, Req, state} = nova_request_plugin:pre_request(
                               Req0, env, #{decode_json_body => true, read_urlencoded_body => true}, state),
          ?assertEqual(<<>>, maps:get(body, Req)),
          ?assertNot(maps:is_key(files, Req))
      end,
      fun() ->
          Req0 = multipart_req(<<"Multipart/Form-Data; boundary=----abc">>),
          {ok, Req, state} = nova_request_plugin:pre_request(Req0, env, #{decode_json_body => true}, state),
          ?assertEqual(<<>>, maps:get(body, Req))
      end]}.

%%====================================================================
%% max_body_length
%%====================================================================

-define(CHUNK, binary:copy(~" ", 64000)).

%% A JSON body sent in Chunks chunks of 64000 bytes: an object padded with
%% spaces. BodyLength is what Cowboy knows from Content-Length.
body_req(Chunks, BodyLength) ->
    Req = nova_test_helper:with_content_type(~"application/json",
                                             nova_test_helper:mock_req(~"POST", ~"/")),
    Req#{has_body => true, body_length => BodyLength, chunks_left => Chunks}.

body_setup() ->
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, read_body,
                fun(#{chunks_left := 1} = Req, #{length := Length}) when Length =< 64000 ->
                        {ok, <<"{}", (binary:part(?CHUNK, 0, 63998))/binary>>, Req#{chunks_left => 0}};
                   (#{chunks_left := N} = Req, #{length := Length}) when Length =< 64000 ->
                        {more, ?CHUNK, Req#{chunks_left => N - 1}}
                end).

body_reads() ->
    meck:num_calls(cowboy_req, read_body, '_').

body_length_test_() ->
    {foreach, fun body_setup/0, fun(_) -> meck:unload(cowboy_req) end,
     [{"a body as long as the limit is decoded",
       fun() ->
           {ok, Req, state} = nova_request_plugin:pre_request(
                                body_req(3, undefined), env,
                                #{decode_json_body => true, max_body_length => 3 * 64000}, state),
           ?assertEqual(#{}, maps:get(json, Req)),
           ?assertEqual(3, body_reads())
       end},
      {"a declared length over the limit is refused before reading",
       fun() ->
           ?assertMatch({stop, {status, 413}, _, state},
                        nova_request_plugin:pre_request(
                          body_req(3, 3 * 64000), env,
                          #{decode_json_body => true, max_body_length => 3 * 64000 - 1}, state)),
           ?assertEqual(0, body_reads())
       end},
      {"a chunked body stops being read once it passes the limit",
       fun() ->
           ?assertMatch({stop, {status, 413}, _, state},
                        nova_request_plugin:pre_request(
                          body_req(1000, undefined), env,
                          #{read_urlencoded_body => true, max_body_length => 100000}, state)),
           ?assertEqual(2, body_reads())
       end},
      {"the default limit is 8000000 bytes",
       fun() ->
           ?assertMatch({stop, {status, 413}, _, state},
                        nova_request_plugin:pre_request(body_req(1000, 8000001), env,
                                                        #{decode_json_body => true}, state)),
           ?assertEqual(0, body_reads()),
           ?assertMatch({stop, {status, 413}, _, state},
                        nova_request_plugin:pre_request(body_req(1000, undefined), env,
                                                        #{decode_json_body => true}, state)),
           ?assertEqual(8000000 div 64000 + 1, body_reads())
       end},
      {"infinity reads the whole body",
       fun() ->
           {ok, Req, state} = nova_request_plugin:pre_request(
                                body_req(200, 200 * 64000), env,
                                #{decode_json_body => true, max_body_length => infinity}, state),
           ?assertEqual(#{}, maps:get(json, Req)),
           ?assertEqual(200, body_reads())
       end}]}.
