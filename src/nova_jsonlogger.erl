%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Originally derived from https://github.com/kivra/jsonformat.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(nova_jsonlogger).
-moduledoc """
JSON formatter for the OTP `logger`.

Renders every log record as a single line of JSON, optionally shaped to
match a popular log schema. See `m:nova_jsonlogger#schemas` and the
[Structured logging](guides/logging.md) guide for the full reference.

## Usage

Wire it as the formatter on a `logger` handler, typically in `sys.config`:

```erlang
{kernel, [
  {logger, [
    {handler, default, logger_std_h, #{
      formatter => {nova_jsonlogger, #{schema => ecs, new_line => true}}
    }}
  ]}
]}.
```

## Config

```erlang
#{
  schema             => nova | ecs | otel | gcp | datadog, %% default: nova
  new_line           => boolean(),                          %% default: false
  new_line_type      => nl | crlf | cr | unix | windows,
  key_mapping        => #{atom() => atom()},                %% post-schema rename
  format_funs        => #{atom() => fun((_) -> _)},         %% post-rename transform
  meta_with          => [atom()],                           %% allowlist meta keys
  meta_without       => [atom()],                           %% denylist meta keys
  redact             => [[atom() | binary()]],              %% paths to scrub
  max_term_size      => non_neg_integer(),                  %% default: 8192 bytes
  max_string_length  => non_neg_integer()                   %% default: 8192 bytes
}
```

## Schema fields

The schema layer recognises a fixed vocabulary of semantic fields and renders
each under the chosen schema's conventional key: `time`, `level`, `text` (the
message), `trace_id`, `span_id`, a structured `error`, and source location
(`file`/`line`/`mfa`). A field is emitted only if present in the log record.

## Trace correlation

`nova_jsonlogger` does not generate trace context. If a log record's metadata
carries `trace_id`/`span_id`, they are relocated to the schema's conventional
keys (`trace.id`/`span.id` for ECS, `TraceId`/`SpanId` for OTel, and so on).
Populate them upstream, either by:

- calling `logger:update_process_metadata(#{trace_id => Id, span_id => Id})`
  in your request handling, or
- adding the optional `opentelemetry_nova` plugin, which writes the hex
  trace/span ids into logger metadata per request.

`nova_jsonlogger` itself has no OpenTelemetry dependency.
""".

-export([
    format/2,
    system_time_to_iso8601/1,
    system_time_to_iso8601_nano/1
]).

-ifdef(TEST).
-export([jsonify/3, cap_binary/2, severity_number/1]).
-endif.

-type schema() :: nova | ecs | otel | gcp | datadog.
-type redact_path() :: [atom() | binary()].

-type config() :: #{
    schema => schema(),
    new_line => boolean(),
    new_line_type => nl | crlf | cr | unix | windows | macos9,
    key_mapping => #{atom() => atom()},
    format_funs => #{atom() => fun((_) -> _)},
    meta_with => [atom()],
    meta_without => [atom()],
    redact => [redact_path()],
    max_term_size => non_neg_integer(),
    max_string_length => non_neg_integer()
}.

-export_type([config/0, schema/0, redact_path/0]).

-define(NEW_LINE, false).
-define(DEFAULT_MAX_TERM_SIZE, 8192).
-define(DEFAULT_MAX_STRING_LENGTH, 8192).
-define(REDACTED, <<"[REDACTED]">>).
-define(TRUNCATED_MARKER, <<"...[truncated]">>).

%%%_* API ==============================================================
-doc """
Format one `t:logger:log_event/0` as a JSON line according to `Config`.

Called by the OTP `logger` framework once it has matched a handler that
declared `{nova_jsonlogger, Config}` as its formatter.
""".
-spec format(logger:log_event(), config()) -> unicode:chardata().
format(
    #{msg := {report, #{format := Format, args := Args, label := {error_logger, _}}}} = Map, Config
) ->
    Report = #{text => io_lib:format(Format, Args)},
    format(Map#{msg := {report, Report}}, Config);
format(#{level := Level, msg := {report, Msg}, meta := Meta}, Config) when is_map(Msg) ->
    Data0 = merge_meta(Msg, Meta#{level => Level}, Config),
    Data1 = extract_error(Data0),
    Data2 = redact(Data1, Config),
    Data3 = apply_schema(Data2, Config),
    Data4 = apply_key_mapping(Data3, Config),
    Data5 = apply_format_funs(Data4, Config),
    encode(pre_encode(Data5, Config), Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
    format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
    Report = #{text => unicode:characters_to_binary(String)},
    format(Map#{msg := {report, Report}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
    format(Map#{msg := {string, io_lib:format(Format, Terms)}}, Config).

-doc "Convert `Epoch` (microseconds since the Unix epoch) to an RFC 3339 binary in UTC.".
-spec system_time_to_iso8601(integer()) -> binary().
system_time_to_iso8601(Epoch) ->
    system_time_to_iso8601(Epoch, microsecond).

-doc "Convert `Epoch` (microseconds since the Unix epoch) to an RFC 3339 binary with nanosecond precision in UTC.".
-spec system_time_to_iso8601_nano(integer()) -> binary().
system_time_to_iso8601_nano(Epoch) ->
    system_time_to_iso8601(1000 * Epoch, nanosecond).

-spec system_time_to_iso8601(integer(), erlang:time_unit()) -> binary().
system_time_to_iso8601(Epoch, Unit) ->
    binary:list_to_bin(calendar:system_time_to_rfc3339(Epoch, [{unit, Unit}, {offset, "Z"}])).

%%%_* Pipeline =========================================================
merge_meta(Msg, Meta0, Config) ->
    Meta1 = meta_without(Meta0, Config),
    Meta2 = meta_with(Meta1, Config),
    maps:merge(Msg, Meta2).

%%% Structured error extraction. Looks for crash_report (logger SASL
%%% convention) or stacktrace meta and converts to a normalised
%%% #{error => #{type, reason, message, stacktrace}} entry.
extract_error(Data) ->
    case maps:take(crash_report, Data) of
        {CrashReport, Rest} when is_list(CrashReport) ->
            Rest#{error => normalise_crash_report(CrashReport)};
        _ ->
            extract_stacktrace(Data)
    end.

extract_stacktrace(Data) ->
    case maps:take(stacktrace, Data) of
        {Stack, Rest} when is_list(Stack) ->
            ErrorBase = maps:get(error, Rest, #{}),
            ErrorBase1 = ErrorBase#{stacktrace => normalise_stacktrace(Stack)},
            ErrorBase2 = maybe_take_into(class, type, Rest, ErrorBase1),
            ErrorBase3 = maybe_take_into(reason, reason, Rest, ErrorBase2),
            Rest1 = maps:without([class, reason], Rest),
            Rest1#{error => ErrorBase3};
        _ ->
            Data
    end.

maybe_take_into(SrcKey, DestKey, Src, Dest) ->
    case maps:find(SrcKey, Src) of
        {ok, V} -> Dest#{DestKey => V};
        error -> Dest
    end.

normalise_crash_report([Proc | _]) when is_list(Proc) ->
    M0 = error_info(lists:keyfind(error_info, 1, Proc)),
    M1 = take_into(registered_name, Proc, M0),
    take_into(pid, Proc, M1);
normalise_crash_report(_) ->
    #{}.

error_info({error_info, {Class, Reason, Stack}}) ->
    #{type => Class, reason => Reason, stacktrace => normalise_stacktrace(Stack)};
error_info({error_info, Other}) ->
    #{reason => Other};
error_info(false) ->
    #{}.

take_into(Key, Proplist, Acc) ->
    case lists:keyfind(Key, 1, Proplist) of
        {Key, V} -> Acc#{Key => V};
        false -> Acc
    end.

normalise_stacktrace(Stack) ->
    [stack_frame(F) || F <- Stack].

stack_frame({M, F, A, Info}) when is_atom(M), is_atom(F), is_integer(A) ->
    base_frame(M, F, A, Info);
stack_frame({M, F, Args, Info}) when is_atom(M), is_atom(F), is_list(Args) ->
    base_frame(M, F, length(Args), Info);
stack_frame(Other) ->
    #{raw => Other}.

base_frame(M, F, A, Info) ->
    Base = #{mfa => {M, F, A}},
    File =
        case lists:keyfind(file, 1, Info) of
            {file, FName} -> Base#{file => unicode:characters_to_binary(FName)};
            false -> Base
        end,
    case lists:keyfind(line, 1, Info) of
        {line, L} -> File#{line => L};
        false -> File
    end.

%%% Redaction. Walks a list of paths (each a list of keys) and replaces
%%% the value at that path with <<"[REDACTED]">>. Missing paths are no-ops.
redact(Data, #{redact := Paths}) when is_list(Paths) ->
    lists:foldl(fun redact_path/2, Data, Paths);
redact(Data, _) ->
    Data.

redact_path([], _Data) ->
    ?REDACTED;
redact_path([K], Data) when is_map(Data) ->
    case maps:is_key(K, Data) of
        true -> Data#{K := ?REDACTED};
        false -> Data
    end;
redact_path([K | Rest], Data) when is_map(Data) ->
    case maps:find(K, Data) of
        {ok, Inner} when is_map(Inner) ->
            Data#{K := redact_path(Rest, Inner)};
        _ ->
            Data
    end;
redact_path(_, Data) ->
    Data.

%%% Schema rendering. Each schema applies a canonical transform from the
%%% (msg + meta) map onto the schema's expected field names.
%%% trace_id/span_id are read from the log record's metadata (see merge_meta).
%%% This module never sets them; rename/3 is a no-op when they are absent.
apply_schema(Data, Config) ->
    Schema = maps:get(schema, Config, nova),
    case Schema of
        nova -> to_nova(Data);
        ecs -> to_ecs(Data);
        otel -> to_otel(Data);
        gcp -> to_gcp(Data);
        datadog -> to_datadog(Data)
    end.

to_nova(Data) ->
    Data1 = format_timestamp(time, Data, microsecond),
    Data1.

to_ecs(Data) ->
    Data1 = rename(time, '@timestamp', format_timestamp(time, Data, microsecond)),
    Data2 = render_severity_text(level, 'log.level', Data1, fun severity_lowercase/1),
    Data3 = rename(trace_id, 'trace.id', Data2),
    Data4 = rename(span_id, 'span.id', Data3),
    Data5 = rename(text, message, Data4),
    Data6 = rename_error(Data5, fun ecs_error_keys/1),
    rename_source_location(Data6, ecs).

to_otel(Data) ->
    Data1 = rename('time', 'Timestamp', format_timestamp('time', Data, nanosecond)),
    Data2 = render_severity(level, 'SeverityText', 'SeverityNumber', Data1),
    Data3 = rename(trace_id, 'TraceId', Data2),
    Data4 = rename(span_id, 'SpanId', Data3),
    Data5 = rename(text, 'Body', Data4),
    rename_error(Data5, fun otel_error_keys/1).

to_gcp(Data) ->
    Data1 = format_timestamp(time, Data, microsecond),
    Data2 = render_severity_text(level, severity, Data1, fun severity_uppercase/1),
    Data3 = rename(trace_id, 'logging.googleapis.com/trace', Data2),
    Data4 = rename(span_id, 'logging.googleapis.com/spanId', Data3),
    Data5 = rename(text, message, Data4),
    rename_source_location(Data5, gcp).

to_datadog(Data) ->
    Data1 = rename(time, timestamp, format_timestamp(time, Data, microsecond)),
    Data2 = render_severity_text(level, status, Data1, fun severity_lowercase/1),
    Data3 = rename(trace_id, 'dd.trace_id', Data2),
    Data4 = rename(span_id, 'dd.span_id', Data3),
    rename(text, message, Data4).

rename(Old, New, Data) ->
    case maps:take(Old, Data) of
        {V, Rest} -> Rest#{New => V};
        error -> Data
    end.

render_severity(LevelKey, TextKey, NumberKey, Data) ->
    case maps:take(LevelKey, Data) of
        {Level, Rest} ->
            Rest#{
                TextKey => severity_uppercase(Level),
                NumberKey => severity_number(Level)
            };
        error ->
            Data
    end.

render_severity_text(LevelKey, NewKey, Data, Fmt) ->
    case maps:take(LevelKey, Data) of
        {Level, Rest} -> Rest#{NewKey => Fmt(Level)};
        error -> Data
    end.

severity_lowercase(L) when is_atom(L) -> atom_to_binary(L, utf8);
severity_lowercase(L) -> L.

severity_uppercase(L) when is_atom(L) ->
    list_to_binary(string:to_upper(atom_to_list(L)));
severity_uppercase(L) ->
    L.

%% OTel severity numbers per spec: TRACE 1-4, DEBUG 5-8, INFO 9-12,
%% WARN 13-16, ERROR 17-20, FATAL 21-24. We map Erlang's levels to the
%% lowest number in their band.
severity_number(debug) -> 5;
severity_number(info) -> 9;
severity_number(notice) -> 10;
severity_number(warning) -> 13;
severity_number(error) -> 17;
severity_number(critical) -> 21;
severity_number(alert) -> 22;
severity_number(emergency) -> 24;
severity_number(_) -> 0.

format_timestamp(Key, Data, Precision) ->
    case maps:find(Key, Data) of
        {ok, T} when is_integer(T) ->
            Data#{Key => format_time(T, Precision)};
        _ ->
            Data
    end.

format_time(T, microsecond) -> system_time_to_iso8601(T);
format_time(T, nanosecond) -> system_time_to_iso8601_nano(T).

rename_error(Data, KeyFn) ->
    case maps:take(error, Data) of
        {Err, Rest} when is_map(Err) ->
            maps:merge(Rest, schema_error(Err, KeyFn));
        _ ->
            Data
    end.

schema_error(Err, KeyFn) ->
    maps:fold(
        fun(K, V, Acc) ->
            case KeyFn(K) of
                undefined -> Acc;
                NewK -> Acc#{NewK => V}
            end
        end,
        #{},
        Err
    ).

ecs_error_keys(type) -> 'error.type';
ecs_error_keys(reason) -> 'error.message';
ecs_error_keys(stacktrace) -> 'error.stack_trace';
ecs_error_keys(_) -> undefined.

otel_error_keys(type) -> 'exception.type';
otel_error_keys(reason) -> 'exception.message';
otel_error_keys(stacktrace) -> 'exception.stacktrace';
otel_error_keys(_) -> undefined.

rename_source_location(Data, ecs) ->
    File = maps:get(file, Data, undefined),
    Line = maps:get(line, Data, undefined),
    Mfa = maps:get(mfa, Data, undefined),
    D1 = maps:without([file, line, mfa], Data),
    D2 = case File of undefined -> D1; _ -> D1#{'log.origin.file.name' => File} end,
    D3 = case Line of undefined -> D2; _ -> D2#{'log.origin.file.line' => Line} end,
    case Mfa of undefined -> D3; _ -> D3#{'log.origin.function' => Mfa} end;
rename_source_location(Data, gcp) ->
    File = maps:get(file, Data, undefined),
    Line = maps:get(line, Data, undefined),
    Mfa = maps:get(mfa, Data, undefined),
    case {File, Line, Mfa} of
        {undefined, undefined, undefined} ->
            Data;
        _ ->
            Loc0 = #{},
            Loc1 = case File of undefined -> Loc0; _ -> Loc0#{file => File} end,
            Loc2 = case Line of undefined -> Loc1; _ -> Loc1#{line => Line} end,
            Loc3 = case Mfa of undefined -> Loc2; _ -> Loc2#{'function' => Mfa} end,
            (maps:without([file, line, mfa], Data))#{
                'logging.googleapis.com/sourceLocation' => Loc3
            }
    end.

%%%_* JSON encoding ====================================================
pre_encode(Data, Config) ->
    MaxTerm = maps:get(max_term_size, Config, ?DEFAULT_MAX_TERM_SIZE),
    MaxStr = maps:get(max_string_length, Config, ?DEFAULT_MAX_STRING_LENGTH),
    walk(Data, MaxTerm, MaxStr).

walk(Data, MaxTerm, MaxStr) when is_map(Data) ->
    maps:fold(
        fun
            (K, V, Acc) when is_map(V) ->
                maps:put(jsonify_key(K), walk(V, MaxTerm, MaxStr), Acc);
            (K, Vs, Acc) when is_list(Vs), Vs =/= [], is_map(hd(Vs)) ->
                maps:put(
                    jsonify_key(K),
                    [walk(V, MaxTerm, MaxStr) || V <- Vs, is_map(V)],
                    Acc
                );
            (K, V, Acc) ->
                maps:put(jsonify_key(K), jsonify(V, MaxTerm, MaxStr), Acc)
        end,
        maps:new(),
        Data
    );
walk(Data, _, _) ->
    Data.

encode(Data, Config) ->
    JsonLib = nova:get_env(json_lib, thoas),
    Json = JsonLib:encode(Data),
    case new_line(Config) of
        true -> [Json, new_line_type(Config)];
        false -> Json
    end.

jsonify_key(A) when is_atom(A) -> A;
jsonify_key(B) when is_binary(B) -> B;
jsonify_key(I) when is_integer(I) -> integer_to_binary(I);
jsonify_key(Other) -> unicode:characters_to_binary(io_lib:format("~p", [Other])).

jsonify(A, _, _) when is_atom(A) -> A;
jsonify(B, _, MaxStr) when is_binary(B) -> cap_binary(B, MaxStr);
jsonify(I, _, _) when is_integer(I) -> I;
jsonify(F, _, _) when is_float(F) -> F;
jsonify(B, _, _) when is_boolean(B) -> B;
jsonify(P, _, MaxStr) when is_pid(P) -> cap_binary(list_to_binary(pid_to_list(P)), MaxStr);
jsonify(P, _, MaxStr) when is_port(P) -> cap_binary(list_to_binary(port_to_list(P)), MaxStr);
jsonify(F, _, MaxStr) when is_function(F) ->
    cap_binary(list_to_binary(erlang:fun_to_list(F)), MaxStr);
jsonify({M, F, A}, _, _) when is_atom(M), is_atom(F), is_integer(A) ->
    <<
        (atom_to_binary(M, utf8))/binary,
        $:,
        (atom_to_binary(F, utf8))/binary,
        $/,
        (integer_to_binary(A))/binary
    >>;
jsonify(L, MaxTerm, MaxStr) when is_list(L) ->
    try list_to_binary(L) of
        S -> cap_binary(S, MaxStr)
    catch
        error:badarg ->
            cap_term(L, MaxTerm)
    end;
jsonify(Any, MaxTerm, _) ->
    cap_term(Any, MaxTerm).

cap_binary(B, Max) when byte_size(B) =< Max -> B;
cap_binary(B, Max) ->
    Head = binary:part(B, 0, Max),
    <<Head/binary, (?TRUNCATED_MARKER)/binary>>.

cap_term(Term, Max) ->
    Bin = unicode:characters_to_binary(io_lib:format("~0p", [Term])),
    cap_binary(Bin, Max).

%%%_* Config helpers ===================================================
apply_format_funs(Data, #{format_funs := Callbacks}) ->
    maps:fold(
        fun
            (K, Fun, Acc) when is_map_key(K, Data) -> maps:update_with(K, Fun, Acc);
            (_, _, Acc) -> Acc
        end,
        Data,
        Callbacks
    );
apply_format_funs(Data, _) ->
    Data.

apply_key_mapping(Data, #{key_mapping := Mapping}) ->
    DataOnlyMapped =
        maps:fold(
            fun
                (K, V, Acc) when is_map_key(K, Data) -> Acc#{V => maps:get(K, Data)};
                (_, _, Acc) -> Acc
            end,
            #{},
            Mapping
        ),
    DataNoMapped = maps:without(maps:keys(Mapping), Data),
    maps:merge(DataNoMapped, DataOnlyMapped);
apply_key_mapping(Data, _) ->
    Data.

new_line(Config) -> maps:get(new_line, Config, ?NEW_LINE).

new_line_type(#{new_line_type := nl}) -> <<"\n">>;
new_line_type(#{new_line_type := unix}) -> <<"\n">>;
new_line_type(#{new_line_type := crlf}) -> <<"\r\n">>;
new_line_type(#{new_line_type := windows}) -> <<"\r\n">>;
new_line_type(#{new_line_type := cr}) -> <<"\r">>;
new_line_type(#{new_line_type := macos9}) -> <<"\r">>;
new_line_type(_Default) -> <<"\n">>.

meta_without(Meta, Config) ->
    maps:without(maps:get(meta_without, Config, [report_cb]), Meta).

meta_with(Meta, #{meta_with := Ks}) ->
    maps:with(Ks, Meta);
meta_with(Meta, _ConfigNotPresent) ->
    Meta.

