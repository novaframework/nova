-module(nova_request_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

%% Logger handler callback: forwards reports to the process that added it.
-export([log/2]).

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

%% RFC 6839: any application/*+json type is JSON; types that only start with
%% application/json are not.
modulate_state_json_suffix_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Decode = fun(ContentType) ->
                          Req = (nova_test_helper:with_content_type(ContentType, nova_test_helper:mock_req(~"POST", ~"/")))#{body => ~"{\"a\":1}"},
                          {ok, Req0, state} = nova_request_plugin:modulate_state(Req, [{decode_json_body, true}], state),
                          maps:get(json, Req0, undefined)
                  end,
         ?assertEqual(#{~"a" => 1}, Decode(~"application/vnd.api+json")),
         ?assertEqual(#{~"a" => 1}, Decode(~"application/merge-patch+json")),
         ?assertEqual(#{~"a" => 1}, Decode(~"Application/Problem+JSON; charset=utf-8")),
         ?assertEqual(#{~"a" => 1}, Decode(~"application/json; charset=utf-8")),
         ?assertEqual(undefined, Decode(~"application/json-seq")),
         ?assertEqual(undefined, Decode(~"application/+json")),
         ?assertEqual(undefined, Decode(~"text/vnd.x+json")),
         ?assertEqual(undefined, Decode(~"application/x+json+xml"))
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
           ?assertMatch({stop, {status, 413, #{~"connection" := ~"close"}}, _, state},
                        nova_request_plugin:pre_request(
                          body_req(3, 3 * 64000), env,
                          #{decode_json_body => true, max_body_length => 3 * 64000 - 1}, state)),
           ?assertEqual(0, body_reads())
       end},
      {"a chunked body stops being read once it passes the limit",
       fun() ->
           ?assertMatch({stop, {status, 413, #{~"connection" := ~"close"}}, _, state},
                        nova_request_plugin:pre_request(
                          body_req(1000, undefined), env,
                          #{read_urlencoded_body => true, max_body_length => 100000}, state)),
           ?assertEqual(2, body_reads())
       end},
      {"the default limit is 1000000 bytes",
       fun() ->
           ?assertMatch({stop, {status, 413, #{~"connection" := ~"close"}}, _, state},
                        nova_request_plugin:pre_request(body_req(1000, 1000001), env,
                                                        #{decode_json_body => true}, state)),
           ?assertEqual(0, body_reads()),
           ?assertMatch({stop, {status, 413, #{~"connection" := ~"close"}}, _, state},
                        nova_request_plugin:pre_request(body_req(1000, undefined), env,
                                                        #{decode_json_body => true}, state)),
           ?assertEqual(1000000 div 64000 + 1, body_reads())
       end},
      {"an invalid limit is warned about once and the default used",
       fun() ->
           persistent_term:erase({nova_request_plugin, invalid_limit, max_body_length, -1}),
           ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
           try
               [?assertMatch({stop, {status, 413, #{~"connection" := ~"close"}}, _, state},
                             nova_request_plugin:pre_request(body_req(1000, 1000001), env,
                                                             #{decode_json_body => true, max_body_length => -1},
                                                             state))
                || _ <- [1, 2]],
               ?assertMatch([#{option := max_body_length, value := -1}], logged())
           after
               logger:remove_handler(?MODULE),
               persistent_term:erase({nova_request_plugin, invalid_limit, max_body_length, -1})
           end
       end},
      {"infinity reads the whole body",
       fun() ->
           {ok, Req, state} = nova_request_plugin:pre_request(
                                body_req(200, 200 * 64000), env,
                                #{decode_json_body => true, max_body_length => infinity}, state),
           ?assertEqual(#{}, maps:get(json, Req)),
           ?assertEqual(200, body_reads())
       end}]}.

%%====================================================================
%% body_read_timeout
%%====================================================================

%% A chunked body that sends one byte per 50 ms: Cowboy hands over what it
%% has when the period passes.
trickle_setup() ->
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, read_body,
                fun(#{chunks_left := 1} = Req, #{period := _}) ->
                        timer:sleep(50),
                        {ok, ~"{}", Req#{chunks_left => 0}};
                   (#{chunks_left := N} = Req, #{period := _}) ->
                        timer:sleep(50),
                        {more, ~" ", Req#{chunks_left => N - 1}}
                end).

read_periods() ->
    [Period || {_, {_, read_body, [_, #{period := Period}]}, _} <- meck:history(cowboy_req)].

strictly_decreasing([A, B | Rest]) -> A > B andalso strictly_decreasing([B | Rest]);
strictly_decreasing(_) -> true.

body_read_timeout_test_() ->
    {foreach, fun trickle_setup/0, fun(_) -> meck:unload(cowboy_req) end,
     [{"a body still arriving at the deadline gets a 408",
       fun() ->
           T0 = erlang:monotonic_time(millisecond),
           ?assertMatch({stop, {status, 408, #{~"connection" := ~"close"}}, _, state},
                        nova_request_plugin:pre_request(body_req(1000, undefined), env,
                                                        #{decode_json_body => true, body_read_timeout => 300},
                                                        state)),
           Elapsed = erlang:monotonic_time(millisecond) - T0,
           ?assert(Elapsed >= 300 andalso Elapsed < 1000),
           ?assert(lists:all(fun(Period) -> Period =< 300 end, read_periods()))
       end},
      {"each read waits no longer than the time left",
       fun() ->
           {stop, {status, 408, #{~"connection" := ~"close"}}, _, state} =
               nova_request_plugin:pre_request(body_req(1000, undefined), env,
                                               #{decode_json_body => true, body_read_timeout => 120}, state),
           Periods = read_periods(),
           ?assert(length(Periods) > 1),
           ?assert(hd(Periods) =< 120),
           ?assert(strictly_decreasing(Periods))
       end},
      {"infinity has no deadline",
       fun() ->
           {ok, Req, state} = nova_request_plugin:pre_request(
                                body_req(5, undefined), env,
                                #{decode_json_body => true, body_read_timeout => infinity}, state),
           ?assertEqual(#{}, maps:get(json, Req)),
           ?assertEqual([15000, 15000, 15000, 15000, 15000], read_periods())
       end},
      {"a body_read_timeout of 0 is warned about and the default used",
       fun() ->
           persistent_term:erase({nova_request_plugin, invalid_limit, body_read_timeout, 0}),
           ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
           try
               {ok, _, state} = nova_request_plugin:pre_request(
                                  body_req(2, undefined), env,
                                  #{decode_json_body => true, body_read_timeout => 0}, state),
               ?assertMatch([#{option := body_read_timeout, value := 0, default := 60000}], logged()),
               ?assert(lists:all(fun(Period) -> Period > 0 end, read_periods()))
           after
               logger:remove_handler(?MODULE),
               persistent_term:erase({nova_request_plugin, invalid_limit, body_read_timeout, 0})
           end
       end}]}.

log(#{msg := {report, Report}}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Report},
    ok;
log(_Event, _Config) ->
    ok.

logged() ->
    receive
        {logged, Report} -> [Report | logged()]
    after 50 ->
            []
    end.
