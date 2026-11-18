-module(nova_stream_h_tests).
-include_lib("eunit/include/eunit.hrl").

%% This module doubles as the next stream handler: it hands back what it is
%% given as commands, so the tests can see what nova_stream_h forwards.
-export([init/3, data/4, info/3, terminate/3]).

init(_StreamID, Req, _Opts) ->
    {[{next_req, Req}], undefined}.

data(_StreamID, _IsFin, _Data, State) ->
    {[], State}.

info(_StreamID, Info, State) ->
    {[Info], State}.

terminate(_StreamID, _Reason, _State) ->
    ok.

%%====================================================================
%% Setup
%%====================================================================

setup() ->
    pg:start_link(nova_scope),
    application:unset_env(nova, environment),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, use_sessions, true),
    application:set_env(nova, session_manager, nova_session_ets),
    {ok, Pid} = nova_session_ets:start_link(),
    nova_once:forget({nova_stream_h, dropped}),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    application:unset_env(nova, bootstrap_application),
    application:unset_env(nova, use_sessions),
    application:unset_env(nova, session_manager),
    application:unset_env(nova, session_cookie_opts),
    application:unset_env(nova, session_max_age),
    application:unset_env(nova, session_cookie_name),
    application:unset_env(nova, environment),
    application:unset_env(nova_stream_h_tests_app, session_max_age),
    ok.

stream_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun request_is_marked_for_nova_handler/0,
      fun tells_whether_the_response_has_begun/0,
      fun second_response_is_dropped/0,
      fun response_after_the_headers_is_dropped/0,
      fun responses_dropped_within_a_second_are_one_warning/0,
      fun refused_headers_leave_room_for_the_answer_of_cowboy/0,
      fun headers_cowboy_is_told_to_ignore_are_an_answer/0
     ]}.

%%====================================================================
%% Tests
%%====================================================================

request_is_marked_for_nova_handler() ->
    {Req, _State} = start(req()),
    ?assertEqual(true, maps:get(nova_stream, Req)).

tells_whether_the_response_has_begun() ->
    {_Req, State} = start(req()),
    ?assertEqual(false, response_begun(State)),
    {[{headers, 200, _}], Streaming} = nova_stream_h:info(1, {headers, 200, #{}}, State),
    ?assertEqual(headers, response_begun(Streaming)),
    {[{data, fin, ~"chunk"}], Streamed} = nova_stream_h:info(1, {data, fin, ~"chunk"}, Streaming),
    ?assertEqual(headers, response_begun(Streamed)),
    {[{response, <<"200 OK">>, _, ~"whole"}], Answered} =
        nova_stream_h:info(1, {response, <<"200 OK">>, #{}, ~"whole"}, State),
    ?assertEqual(true, response_begun(Answered)).

%% Cowboy stops the connection process when a request is answered twice.
second_response_is_dropped() ->
    {_Req, State} = start(req()),
    {[{response, 200, _, ~"first"}], Answered} = nova_stream_h:info(1, {response, 200, #{}, ~"first"}, State),
    {{[], Dropped}, [{warning, {report, #{status := 403, method := ~"GET"}}}]} =
        nova_test_log:collect(fun() -> nova_stream_h:info(1, {response, 403, #{}, ~"second"}, Answered) end),
    ?assertEqual({{[], Dropped}, []},
                 nova_test_log:collect(fun() -> nova_stream_h:info(1, {headers, 200, #{}}, Dropped) end)),
    ?assertEqual(true, response_begun(Dropped)).

%% A controller that answers twice does it on every request, and the
%% connection process is where the warning is logged from.
responses_dropped_within_a_second_are_one_warning() ->
    {_Req, State} = start((req())#{path => ~"/"}),
    {[{response, 200, _, ~"first"}], Answered} = nova_stream_h:info(1, {response, 200, #{}, ~"first"}, State),
    Again = fun() -> nova_stream_h:info(1, {response, 403, #{}, ~"second"}, Answered) end,
    {{[], _}, [{warning, {report, #{status := 403, path := ~"/", not_logged := 0}}}]} = nova_test_log:collect(Again),
    ?assertMatch({{[], _}, []}, nova_test_log:collect(fun() -> Again(), Again() end)).

response_after_the_headers_is_dropped() ->
    {_Req, State} = start(req()),
    {[{headers, 200, _}], Streaming} = nova_stream_h:info(1, {headers, 200, #{}}, State),
    {{[], Dropped}, [{warning, _}]} =
        nova_test_log:collect(fun() -> nova_stream_h:info(1, {response, 500, #{}, ~"page"}, Streaming) end),
    ?assertEqual(headers, response_begun(Dropped)),
    ?assertMatch({[{data, fin, ~"chunk"}], _}, nova_stream_h:info(1, {data, fin, ~"chunk"}, Dropped)).

%% Cowboy sends nothing when it refuses the headers of a response and answers
%% 500 itself, through here.
refused_headers_leave_room_for_the_answer_of_cowboy() ->
    {_Req, State} = start(req()),
    Refused = #{~"location" => ~"/next\r\nset-cookie: a=b"},
    [begin
         {[_Forwarded], State1} = nova_stream_h:info(1, Response, State),
         ?assertEqual(false, response_begun(State1)),
         ?assertMatch({[{response, 500, #{~"content-length" := ~"0"}, <<>>}], _},
                      nova_stream_h:info(1, {response, 500, #{~"content-length" => ~"0"}, <<>>}, State1))
     end || Response <- [{response, 302, Refused, <<>>}, {headers, 302, Refused}]].

%% With invalid_response_headers set to ignore Cowboy sends them as they are.
headers_cowboy_is_told_to_ignore_are_an_answer() ->
    {[{next_req, _Req}], State} =
        nova_stream_h:init(1, req(), #{stream_handlers => [?MODULE], invalid_response_headers => ignore}),
    {[_Forwarded], Answered} =
        nova_stream_h:info(1, {response, 302, #{~"location" => ~"/next\r\nset-cookie: a=b"}, <<>>}, State),
    ?assertEqual(true, response_begun(Answered)).

response_begun(State) ->
    Ref = make_ref(),
    {[], State} = nova_stream_h:info(1, {nova_response_begun, self(), Ref}, State),
    receive
        {Ref, Sent} -> Sent
    after 0 ->
            error(no_answer)
    end.

%%====================================================================
%% Helpers
%%====================================================================

req() ->
    #{scheme => ~"http", method => ~"GET", headers => #{}}.

start(Req) ->
    {[{next_req, NextReq}], State} =
        nova_stream_h:init(1, Req, #{stream_handlers => [?MODULE]}),
    {NextReq, State}.

%% Not a documented function of Cowboy, so a new Cowboy is checked for it here.
cowboy_validates_response_headers_test() ->
    {module, cowboy_http} = code:ensure_loaded(cowboy_http),
    ?assert(erlang:function_exported(cowboy_http, validate_response_headers, 2)).
