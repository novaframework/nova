-module(nova_jsonlogger_tests).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_MAX_TERM_SIZE, 8192).
-define(DEFAULT_MAX_STRING_LENGTH, 8192).
-define(TRUNCATED_MARKER, <<"...[truncated]">>).

-define(assertJSONEqual(Expected, Actual),
    (fun() ->
        JsonLib = nova:get_env(json_lib, thoas),
        ?assertEqual(JsonLib:decode(Expected), JsonLib:decode(Actual))
    end)()
).

format(Event, Config) ->
    nova_jsonlogger:format(Event, Config).

format_test() ->
    ?assertJSONEqual(
        <<"{\"level\":\"alert\",\"text\":\"derp\"}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{}}, #{})
    ),
    ?assertJSONEqual(
        <<"{\"herp\":\"derp\",\"level\":\"alert\"}">>,
        format(#{level => alert, msg => {report, #{herp => derp}}, meta => #{}}, #{})
    ).

format_funs_test() ->
    Config1 = #{
        format_funs => #{
            time => fun(T) when is_binary(T) -> <<T/binary, "!">> end,
            level => fun(alert) -> info end
        }
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(
        format(#{level => alert, msg => {string, "derp"}, meta => #{time => 1}}, Config1)
    ),
    ?assertEqual(<<"info">>, maps:get(<<"level">>, Decoded)),
    ?assertEqual(<<"derp">>, maps:get(<<"text">>, Decoded)),
    Time = maps:get(<<"time">>, Decoded),
    ?assert(is_binary(Time)),
    ?assertEqual($!, binary:last(Time)).

key_mapping_test() ->
    Config1 = #{
        key_mapping => #{
            level => lvl,
            text => message
        }
    },
    ?assertJSONEqual(
        <<"{\"lvl\":\"alert\",\"message\":\"derp\"}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{}}, Config1)
    ),

    Config2 = #{
        key_mapping => #{
            level => lvl,
            text => level
        }
    },
    ?assertJSONEqual(
        <<"{\"level\":\"derp\",\"lvl\":\"alert\"}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{}}, Config2)
    ).

list_format_test() ->
    ErrorReport =
        #{
            level => error,
            meta => #{},
            msg => {report, #{report => [{hej, "hopp"}]}}
        },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(ErrorReport, #{})),
    ?assertEqual(<<"error">>, maps:get(<<"level">>, Decoded)),
    ReportBin = maps:get(<<"report">>, Decoded),
    ?assert(is_binary(ReportBin)).

meta_without_test() ->
    Error = #{
        level => info,
        msg => {report, #{answer => 42}},
        meta => #{secret => xyz}
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D1} = JsonLib:decode(format(Error, #{})),
    ?assertEqual(42, maps:get(<<"answer">>, D1)),
    ?assertEqual(<<"info">>, maps:get(<<"level">>, D1)),
    ?assertEqual(<<"xyz">>, maps:get(<<"secret">>, D1)),

    Config2 = #{meta_without => [secret]},
    {ok, D2} = JsonLib:decode(format(Error, Config2)),
    ?assertNot(maps:is_key(<<"secret">>, D2)).

meta_with_test() ->
    Error = #{
        level => info,
        msg => {report, #{answer => 42}},
        meta => #{secret => xyz}
    },
    Config = #{meta_with => [level]},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Error, Config)),
    ?assertNot(maps:is_key(<<"secret">>, D)),
    ?assertEqual(<<"info">>, maps:get(<<"level">>, D)).

newline_test() ->
    ConfigDefault = #{new_line => true},
    Out = format(#{level => alert, msg => {string, "derp"}, meta => #{}}, ConfigDefault),
    ?assertEqual(<<"\n">>, lists:last(Out)).

newline_types_test() ->
    Base = #{level => alert, msg => {string, "x"}, meta => #{}},
    ?assertEqual(<<"\n">>, lists:last(format(Base, #{new_line => true, new_line_type => nl}))),
    ?assertEqual(<<"\r\n">>, lists:last(format(Base, #{new_line => true, new_line_type => crlf}))),
    ?assertEqual(<<"\r">>, lists:last(format(Base, #{new_line => true, new_line_type => cr}))),
    ?assertEqual(<<"\n">>, lists:last(format(Base, #{new_line => true}))).

no_newline_test() ->
    Base = #{level => alert, msg => {string, "x"}, meta => #{}},
    Result = format(Base, #{}),
    ?assert(is_binary(Result)).

jsonify_types_test() ->
    Max = ?DEFAULT_MAX_STRING_LENGTH,
    ?assertEqual(hello, nova_jsonlogger:jsonify(hello, ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assertEqual(<<"bin">>, nova_jsonlogger:jsonify(<<"bin">>, ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assertEqual(42, nova_jsonlogger:jsonify(42, ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assertEqual(3.14, nova_jsonlogger:jsonify(3.14, ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assertEqual(true, nova_jsonlogger:jsonify(true, ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assert(is_binary(nova_jsonlogger:jsonify(self(), ?DEFAULT_MAX_TERM_SIZE, Max))),
    ?assert(is_binary(nova_jsonlogger:jsonify(fun erlang:now/0, ?DEFAULT_MAX_TERM_SIZE, Max))),
    ?assertEqual(<<"hello">>, nova_jsonlogger:jsonify("hello", ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assert(is_binary(nova_jsonlogger:jsonify([{a, b}], ?DEFAULT_MAX_TERM_SIZE, Max))),
    ?assertEqual(<<"lists:reverse/1">>, nova_jsonlogger:jsonify({lists, reverse, 1}, ?DEFAULT_MAX_TERM_SIZE, Max)),
    ?assert(is_binary(nova_jsonlogger:jsonify(make_ref(), ?DEFAULT_MAX_TERM_SIZE, Max))).

format_keyval_list_test() ->
    Event = #{level => info, msg => {report, [{key, <<"val">>}]}, meta => #{}},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(Event, #{})),
    ?assertEqual(<<"val">>, maps:get(<<"key">>, Decoded)).

format_format_terms_test() ->
    Event = #{level => info, msg => {"hello ~s", ["world"]}, meta => #{}},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(Event, #{})),
    ?assertEqual(<<"hello world">>, maps:get(<<"text">>, Decoded)).

format_error_logger_test() ->
    Event = #{
        level => error,
        msg =>
            {report, #{
                format => "error: ~p",
                args => [bad],
                label => {error_logger, error}
            }},
        meta => #{}
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(Event, #{})),
    ?assert(maps:is_key(<<"text">>, Decoded)).

nested_map_test() ->
    Event = #{level => info, msg => {report, #{outer => #{inner => value}}}, meta => #{}},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(Event, #{})),
    ?assertEqual(#{<<"inner">> => <<"value">>}, maps:get(<<"outer">>, Decoded)).

list_of_maps_test() ->
    Event = #{level => info, msg => {report, #{items => [#{a => 1}, #{b => 2}]}}, meta => #{}},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(Event, #{})),
    ?assertEqual([#{<<"a">> => 1}, #{<<"b">> => 2}], maps:get(<<"items">>, Decoded)).

system_time_to_iso8601_test() ->
    Epoch = erlang:system_time(microsecond),
    Result = nova_jsonlogger:system_time_to_iso8601(Epoch),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 10).

system_time_to_iso8601_nano_test() ->
    Epoch = erlang:system_time(microsecond),
    Result = nova_jsonlogger:system_time_to_iso8601_nano(Epoch),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 10).

format_port_test() ->
    Port = hd(erlang:ports()),
    Result = nova_jsonlogger:jsonify(Port, ?DEFAULT_MAX_TERM_SIZE, ?DEFAULT_MAX_STRING_LENGTH),
    ?assert(is_binary(Result)).

%%%--- Schema tests ----------------------------------------------------
nova_schema_default_timestamp_test() ->
    Event = #{level => info, msg => {report, #{x => 1}}, meta => #{time => 1}},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, Decoded} = JsonLib:decode(format(Event, #{})),
    Time = maps:get(<<"time">>, Decoded),
    ?assert(is_binary(Time)),
    ?assert(binary:match(Time, <<"T">>) =/= nomatch).

ecs_schema_test() ->
    Event = #{
        level => info,
        msg => {report, #{text => <<"hello">>}},
        meta => #{
            time => 1700000000000000,
            trace_id => <<"abc">>,
            span_id => <<"def">>
        }
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => ecs})),
    ?assertEqual(<<"info">>, maps:get(<<"log.level">>, D)),
    ?assert(maps:is_key(<<"@timestamp">>, D)),
    ?assertEqual(<<"hello">>, maps:get(<<"message">>, D)),
    ?assertEqual(<<"abc">>, maps:get(<<"trace.id">>, D)),
    ?assertEqual(<<"def">>, maps:get(<<"span.id">>, D)).

otel_schema_test() ->
    Event = #{
        level => warning,
        msg => {report, #{text => <<"watch out">>}},
        meta => #{
            time => 1700000000000000,
            trace_id => <<"abc">>,
            span_id => <<"def">>
        }
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => otel})),
    ?assertEqual(<<"WARNING">>, maps:get(<<"SeverityText">>, D)),
    ?assertEqual(13, maps:get(<<"SeverityNumber">>, D)),
    ?assertEqual(<<"watch out">>, maps:get(<<"Body">>, D)),
    ?assertEqual(<<"abc">>, maps:get(<<"TraceId">>, D)),
    ?assertEqual(<<"def">>, maps:get(<<"SpanId">>, D)),
    ?assert(maps:is_key(<<"Timestamp">>, D)).

gcp_schema_test() ->
    Event = #{
        level => error,
        msg => {report, #{text => <<"bad">>}},
        meta => #{trace_id => <<"abc">>, span_id => <<"def">>}
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => gcp})),
    ?assertEqual(<<"ERROR">>, maps:get(<<"severity">>, D)),
    ?assertEqual(<<"bad">>, maps:get(<<"message">>, D)),
    ?assertEqual(<<"abc">>, maps:get(<<"logging.googleapis.com/trace">>, D)),
    ?assertEqual(<<"def">>, maps:get(<<"logging.googleapis.com/spanId">>, D)).

datadog_schema_test() ->
    Event = #{
        level => info,
        msg => {report, #{text => <<"hi">>}},
        meta => #{trace_id => <<"abc">>, span_id => <<"def">>}
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => datadog})),
    ?assertEqual(<<"info">>, maps:get(<<"status">>, D)),
    ?assertEqual(<<"hi">>, maps:get(<<"message">>, D)),
    ?assertEqual(<<"abc">>, maps:get(<<"dd.trace_id">>, D)),
    ?assertEqual(<<"def">>, maps:get(<<"dd.span_id">>, D)).

ecs_source_location_test() ->
    Event = #{
        level => info,
        msg => {report, #{text => <<"hi">>}},
        meta => #{file => "foo.erl", line => 42, mfa => {mod, fun_, 1}}
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => ecs})),
    ?assertEqual(<<"foo.erl">>, maps:get(<<"log.origin.file.name">>, D)),
    ?assertEqual(42, maps:get(<<"log.origin.file.line">>, D)),
    ?assertEqual(<<"mod:fun_/1">>, maps:get(<<"log.origin.function">>, D)).

gcp_source_location_test() ->
    Event = #{
        level => info,
        msg => {report, #{text => <<"hi">>}},
        meta => #{file => "foo.erl", line => 42, mfa => {mod, fun_, 1}}
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => gcp})),
    Loc = maps:get(<<"logging.googleapis.com/sourceLocation">>, D),
    ?assertEqual(<<"foo.erl">>, maps:get(<<"file">>, Loc)),
    ?assertEqual(42, maps:get(<<"line">>, Loc)).

%%%--- Severity number boundaries --------------------------------------
severity_number_test() ->
    ?assertEqual(5, nova_jsonlogger:severity_number(debug)),
    ?assertEqual(9, nova_jsonlogger:severity_number(info)),
    ?assertEqual(13, nova_jsonlogger:severity_number(warning)),
    ?assertEqual(17, nova_jsonlogger:severity_number(error)),
    ?assertEqual(22, nova_jsonlogger:severity_number(alert)),
    ?assertEqual(24, nova_jsonlogger:severity_number(emergency)),
    ?assertEqual(0, nova_jsonlogger:severity_number(weird_value)).

%%%--- Structured error tests ------------------------------------------
extract_error_from_stacktrace_test() ->
    Event = #{
        level => error,
        msg => {report, #{text => <<"boom">>}},
        meta => #{
            class => error,
            reason => badarg,
            stacktrace => [{mymod, myfun, 2, [{file, "mymod.erl"}, {line, 17}]}]
        }
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{})),
    Err = maps:get(<<"error">>, D),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Err)),
    ?assertEqual(<<"badarg">>, maps:get(<<"reason">>, Err)),
    [Frame | _] = maps:get(<<"stacktrace">>, Err),
    ?assertEqual(<<"mymod:myfun/2">>, maps:get(<<"mfa">>, Frame)),
    ?assertEqual(<<"mymod.erl">>, maps:get(<<"file">>, Frame)),
    ?assertEqual(17, maps:get(<<"line">>, Frame)).

extract_error_ecs_test() ->
    Event = #{
        level => error,
        msg => {report, #{}},
        meta => #{
            class => error,
            reason => badarg,
            stacktrace => [{mymod, myfun, 2, [{file, "mymod.erl"}, {line, 17}]}]
        }
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => ecs})),
    ?assertEqual(<<"error">>, maps:get(<<"error.type">>, D)),
    ?assertEqual(<<"badarg">>, maps:get(<<"error.message">>, D)),
    ?assert(is_list(maps:get(<<"error.stack_trace">>, D))).

extract_error_otel_test() ->
    Event = #{
        level => error,
        msg => {report, #{}},
        meta => #{
            class => exit,
            reason => normal,
            stacktrace => [{m, f, 0, []}]
        }
    },
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, #{schema => otel})),
    ?assertEqual(<<"exit">>, maps:get(<<"exception.type">>, D)),
    ?assertEqual(<<"normal">>, maps:get(<<"exception.message">>, D)),
    ?assert(is_list(maps:get(<<"exception.stacktrace">>, D))).

%%%--- Redaction tests -------------------------------------------------
redact_top_level_test() ->
    Event = #{
        level => info,
        msg => {report, #{user => <<"alice">>, password => <<"secret">>}},
        meta => #{}
    },
    Config = #{redact => [[password]]},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, Config)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"password">>, D)),
    ?assertEqual(<<"alice">>, maps:get(<<"user">>, D)).

redact_nested_test() ->
    Event = #{
        level => info,
        msg =>
            {report, #{
                req => #{
                    headers => #{authorization => <<"bearer abc">>, host => <<"x">>}
                }
            }},
        meta => #{}
    },
    Config = #{redact => [[req, headers, authorization]]},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, Config)),
    Headers = maps:get(<<"headers">>, maps:get(<<"req">>, D)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"authorization">>, Headers)),
    ?assertEqual(<<"x">>, maps:get(<<"host">>, Headers)).

redact_missing_path_test() ->
    Event = #{level => info, msg => {report, #{a => 1}}, meta => #{}},
    Config = #{redact => [[nope, nothing]]},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, Config)),
    ?assertEqual(1, maps:get(<<"a">>, D)).

%%%--- Size cap tests --------------------------------------------------
cap_binary_test() ->
    Big = binary:copy(<<"x">>, 1000),
    Capped = nova_jsonlogger:cap_binary(Big, 100),
    ?assert(byte_size(Capped) > 100),
    ?assert(byte_size(Capped) < 200),
    ?assertEqual(?TRUNCATED_MARKER, binary:part(Capped, byte_size(Capped) - byte_size(?TRUNCATED_MARKER), byte_size(?TRUNCATED_MARKER))).

cap_term_in_pipeline_test() ->
    Huge = lists:duplicate(5000, {complex, term, here}),
    Event = #{level => info, msg => {report, #{blob => Huge}}, meta => #{}},
    Config = #{max_term_size => 200},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, Config)),
    Blob = maps:get(<<"blob">>, D),
    ?assert(is_binary(Blob)),
    ?assert(byte_size(Blob) < 400).

cap_string_in_pipeline_test() ->
    Big = binary:copy(<<"a">>, 5000),
    Event = #{level => info, msg => {report, #{s => Big}}, meta => #{}},
    Config = #{max_string_length => 50},
    JsonLib = nova:get_env(json_lib, thoas),
    {ok, D} = JsonLib:decode(format(Event, Config)),
    S = maps:get(<<"s">>, D),
    ?assert(byte_size(S) < 200).
