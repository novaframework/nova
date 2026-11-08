-module(nova_telemetry).
-moduledoc false.

-export([
         start/2,
         stop/2,
         exception/4
        ]).

-export_type([span/0]).

-opaque span() :: {telemetry:event_name(), integer(), telemetry:event_metadata()}.

-spec start(telemetry:event_name(), telemetry:event_metadata()) -> span().
start(Prefix, Metadata) ->
    StartTime = erlang:monotonic_time(),
    Metadata0 = Metadata#{telemetry_span_context => erlang:make_ref()},
    telemetry:execute(Prefix ++ [start],
                      #{monotonic_time => StartTime, system_time => erlang:system_time()},
                      Metadata0),
    {Prefix, StartTime, Metadata0}.

-spec stop(span(), telemetry:event_metadata()) -> ok.
stop({Prefix, StartTime, Metadata}, StopMetadata) ->
    StopTime = erlang:monotonic_time(),
    telemetry:execute(Prefix ++ [stop],
                      #{monotonic_time => StopTime, duration => StopTime - StartTime},
                      maps:merge(Metadata, StopMetadata)).

%% Handlers export exceptions, so call arguments (a Req, plugin options or
%% state, a frame) are reduced to their arity before they leave Nova.
-spec exception(span(), error | exit | throw, term(), erlang:stacktrace()) -> ok.
exception({Prefix, StartTime, Metadata}, Class, Reason, Stacktrace) ->
    StopTime = erlang:monotonic_time(),
    telemetry:execute(Prefix ++ [exception],
                      #{monotonic_time => StopTime, duration => StopTime - StartTime},
                      Metadata#{kind => Class,
                                reason => Reason,
                                stacktrace => without_arguments(Stacktrace)}).

without_arguments(Stacktrace) ->
    [{Module, Function, arity(Arguments), Location} || {Module, Function, Arguments, Location} <- Stacktrace].

arity(Arguments) when is_list(Arguments) -> length(Arguments);
arity(Arity)                             -> Arity.
