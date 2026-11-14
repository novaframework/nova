-module(nova_telemetry_tests).
-include_lib("eunit/include/eunit.hrl").

-export([forward_event/4]).

-define(PREFIX, [nova, telemetry_test]).

span_test_() ->
    {setup,
     fun() -> application:ensure_all_started(telemetry) end,
     [fun() -> with_handler(fun start_and_stop/0) end,
      fun() -> with_handler(fun exception_strips_arguments/0) end,
      fun() -> with_handler(fun exception_reason_is_reduced_to_its_tag/0) end,
      fun() -> with_handler(fun exception_location_keeps_file_and_line/0) end]}.

with_handler(Test) ->
    Id = make_ref(),
    ok = telemetry:attach_many(Id, [?PREFIX ++ [Phase] || Phase <- [start, stop, exception]],
                               fun ?MODULE:forward_event/4, self()),
    try
        Test()
    after
        telemetry:detach(Id)
    end.

start_and_stop() ->
    Span = nova_telemetry:start(?PREFIX, #{app => test_app}),
    {StartM, Start} = next_event(start),
    ?assertMatch(#{monotonic_time := T, system_time := S} when is_integer(T) andalso is_integer(S), StartM),
    #{telemetry_span_context := Context} = Start,
    ?assert(is_reference(Context)),
    ?assertEqual(#{app => test_app, telemetry_span_context => Context}, Start),

    ?assertEqual(ok, nova_telemetry:stop(Span, #{status => 200})),
    {StopM, Stop} = next_event(stop),
    #{duration := Duration, monotonic_time := StopTime} = StopM,
    ?assertEqual(StopTime - maps:get(monotonic_time, StartM), Duration),
    ?assert(Duration >= 0),
    ?assertEqual(#{app => test_app, status => 200, telemetry_span_context => Context}, Stop).

exception_strips_arguments() ->
    Span = nova_telemetry:start(?PREFIX, #{app => test_app}),
    {StartM, #{telemetry_span_context := Context}} = next_event(start),
    Stacktrace = [{mod, with_args, [~"secret", #{token => ~"t"}], [{line, 1}]},
                  {mod, with_arity, 2, []}],
    ?assertEqual(ok, nova_telemetry:exception(Span, error, boom, Stacktrace)),
    {ExcM, Exc} = next_event(exception),
    #{duration := Duration, monotonic_time := StopTime} = ExcM,
    ?assertEqual(StopTime - maps:get(monotonic_time, StartM), Duration),
    ?assertEqual(#{app => test_app,
                   telemetry_span_context => Context,
                   kind => error,
                   reason => boom,
                   stacktrace => [{mod, with_args, 2, [{line, 1}]}, {mod, with_arity, 2, []}]},
                 Exc).

exception_reason_is_reduced_to_its_tag() ->
    Req = #{method => ~"GET", headers => #{~"cookie" => ~"session_id=do-not-export-me"}},
    [begin
         Span = nova_telemetry:start(?PREFIX, #{app => test_app}),
         _ = next_event(start),
         ?assertEqual(ok, nova_telemetry:exception(Span, Class, Reason, [])),
         {_, Exc} = next_event(exception),
         ?assertEqual(#{app => test_app, kind => Class, reason => Tag, stacktrace => []},
                      maps:remove(telemetry_span_context, Exc))
     end || {Class, Reason, Tag} <- [{error, {badmatch, Req}, badmatch},
                                     {error, {case_clause, {stop, Req}}, case_clause},
                                     {exit, {timeout, {gen_server, call, [Req]}}, timeout},
                                     {throw, {not_found, Req, extra}, not_found},
                                     {error, undef, undef},
                                     {throw, Req, unknown},
                                     {error, [Req], unknown},
                                     {error, ~"do-not-export-me", unknown},
                                     {error, {{nested, Req}, Req}, unknown},
                                     {error, {~"do-not-export-me", Req}, unknown},
                                     {exit, {}, unknown}]].

exception_location_keeps_file_and_line() ->
    Span = nova_telemetry:start(?PREFIX, #{app => test_app}),
    _ = next_event(start),
    ?assertEqual(ok, nova_telemetry:exception(Span, error, boom, stacktrace_with_extras())),
    {_, Exc} = next_event(exception),
    ?assertEqual([{mod, fun_a, 1, [{file, "mod.erl"}, {line, 7}]},
                  {mod, fun_b, 1, [{line, 9}]},
                  {mod, fun_c, 0, []}],
                 maps:get(stacktrace, Exc)).

%% Frames as erlang:raise/3 lets a caller build them, not only as the
%% runtime produces them.
-spec stacktrace_with_extras() -> dynamic().
stacktrace_with_extras() ->
    [{mod, fun_a, 1, [{file, "mod.erl"}, {line, 7}, {error_info, #{cause => ~"do-not-export-me"}}]},
     {mod, fun_b, [~"do-not-export-me"], [{line, 9}, ~"do-not-export-me"]},
     {mod, fun_c, 0, ~"do-not-export-me"},
     {not_a_frame, ~"do-not-export-me"}].

reason_tag_test_() ->
    [?_assertEqual(normal, nova_telemetry:reason_tag(normal)),
     ?_assertEqual(remote, nova_telemetry:reason_tag({remote, 1000, ~"payload"})),
     ?_assertEqual(unknown, nova_telemetry:reason_tag({1000, ~"payload"})),
     ?_assertEqual(unknown, nova_telemetry:reason_tag(~"payload"))].

forward_event([nova, telemetry_test, Phase], Measurements, Metadata, Pid) ->
    Pid ! {telemetry, Phase, Measurements, Metadata}.

next_event(Phase) ->
    receive
        {telemetry, Phase, Measurements, Metadata} -> {Measurements, Metadata}
    after 1000 ->
            error({no_event, Phase})
    end.
