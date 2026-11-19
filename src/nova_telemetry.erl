-module(nova_telemetry).
-moduledoc false.

-export([
         start/2,
         stop/2,
         exception/4,
         reason_tag/1
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

-spec exception(span(), error | exit | throw, term(), erlang:stacktrace()) -> ok.
exception({Prefix, StartTime, Metadata}, Class, Reason, Stacktrace) ->
    StopTime = erlang:monotonic_time(),
    telemetry:execute(Prefix ++ [exception],
                      #{monotonic_time => StopTime, duration => StopTime - StartTime},
                      Metadata#{kind => Class,
                                reason => reason_tag(Reason),
                                stacktrace => without_arguments(Stacktrace)}).

-spec reason_tag(term()) -> atom().
reason_tag(Reason) when is_atom(Reason) ->
    Reason;
reason_tag(Reason) when is_tuple(Reason), tuple_size(Reason) > 0 ->
    case element(1, Reason) of
        Tag when is_atom(Tag) -> Tag;
        _ -> unknown
    end;
reason_tag(_Reason) ->
    unknown.

without_arguments(Stacktrace) ->
    [{Module, Function, arity(Arguments), file_and_line(Location)}
     || {Module, Function, Arguments, Location} <- Stacktrace].

arity(Arguments) when is_list(Arguments) -> length(Arguments);
arity(Arity)                             -> Arity.

file_and_line(Location) when is_list(Location) ->
    [Entry || {Key, _Value} = Entry <- Location, Key =:= file orelse Key =:= line];
file_and_line(_Location) ->
    [].
