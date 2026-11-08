-module(nova_telemetry_tests).
-include_lib("eunit/include/eunit.hrl").

-export([forward_event/4]).

-define(PREFIX, [nova, telemetry_test]).

span_test_() ->
    {setup,
     fun() -> application:ensure_all_started(telemetry) end,
     [fun() -> with_handler(fun start_and_stop/0) end,
      fun() -> with_handler(fun exception_strips_arguments/0) end]}.

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

forward_event([nova, telemetry_test, Phase], Measurements, Metadata, Pid) ->
    Pid ! {telemetry, Phase, Measurements, Metadata}.

next_event(Phase) ->
    receive
        {telemetry, Phase, Measurements, Metadata} -> {Measurements, Metadata}
    after 1000 ->
            error({no_event, Phase})
    end.
