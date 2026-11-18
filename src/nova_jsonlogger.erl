%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This module is copied from https://github.com/kivra/jsonformat
%%%
%%% What is changed is that we don't use jsx but the json library in Nova
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(nova_jsonlogger).
-moduledoc """
Custom formatter for the Erlang OTP logger application which outputs single-line JSON formatted data.

With `redact => true` in its configuration the formatter takes request data and secrets out of every report and its metadata before encoding them, the way Nova does for its own crash logs: a Cowboy request map is replaced by its method and path, and the value of a key whose name looks sensitive (`password`, `access_token`, `session_id` and the like, plus what `redact_keys` lists) by `redacted`. That also covers what your own code and other libraries log. It is off by default.

```erlang
{kernel, [{logger, [{handler, default, logger_std_h,
                     #{formatter => {nova_jsonlogger, #{redact => true, new_line => true}}}}]}]}
```
""".
%%%_* Exports ==========================================================
-export([format/2,
         system_time_to_iso8601/1,
         system_time_to_iso8601_nano/1]).

%%%_* Types ============================================================
-doc """
The configuration of the formatter. All keys are optional.

- `new_line` - end every entry with a line break. Default `false`.
- `new_line_type` - the line break: `nl` or `unix` for `\n` (the default),
  `crlf` or `windows` for `\r\n`, `cr` or `macos9` for `\r`.
- `key_mapping` - renames keys of the entry, as `#{From => To}`.
- `format_funs` - funs applied to the value of a key, after the renaming, as
  `#{Key => Fun}`.
- `meta_with` - the only metadata keys to include.
- `meta_without` - metadata keys to leave out. Default `[report_cb]`.
- `redact` - take request data and secrets out of the entry. Default `false`.
""".
-type config() :: #{
    new_line => boolean(),
    new_line_type => nl | crlf | cr | unix | windows | macos9,
    key_mapping => #{atom() => atom()},
    format_funs => #{atom() => fun((_) -> _)},
    meta_with => [atom()],
    meta_without => [atom()],
    redact => boolean()
}.

-export_type([config/0]).

%%%_* Macros ===========================================================
%%%_* Options ----------------------------------------------------------
-define(NEW_LINE, false).
-define(PRINTED_CHARS_LIMIT, 4096).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-doc """
The formatter callback of `logger`. Returns the log event as one JSON object:
the keys of a report, or `text` for a string or a format with arguments,
merged with `level` and the metadata. A metadata key replaces a report key
of the same name.
""".
-spec format(logger:log_event(), config()) -> unicode:chardata().
format(
    #{msg := {report, #{format := Format, args := Args, label := {error_logger, _}}}} = Map, Config
) when is_list(Args) ->
    Report = #{text => rendered(Format, Args, Config)},
    format(Map#{msg := {report, Report}}, Config);
format(#{level := Level, msg := {report, Msg}, meta := Meta}, Config) when is_map(Msg) ->
    Data0 = merge_meta(redact(Msg, Config), redact(Meta#{level => Level}, Config), Config),
    Data1 = apply_key_mapping(Data0, Config),
    Data2 = apply_format_funs(Data1, Config),
    encode(pre_encode(Data2, Config), Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
    format(Map#{msg := {report, maps:from_list([Pair || {_Key, _Value} = Pair <- KeyVal])}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
    format(Map#{msg := {report, #{text => String}}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
    format(Map#{msg := {string, rendered(Format, Terms, Config)}}, Config).

-doc """
Formats a system time in microseconds, such as `logger:timestamp/0`, as an
RFC 3339 timestamp in UTC with microsecond precision. Meant for
`format_funs`.
""".
-spec system_time_to_iso8601(integer()) -> binary().
system_time_to_iso8601(Epoch) ->
    system_time_to_iso8601(Epoch, microsecond).

-doc """
As `system_time_to_iso8601/1`, with nanosecond precision. The argument is
still in microseconds.
""".
-spec system_time_to_iso8601_nano(integer()) -> binary().
system_time_to_iso8601_nano(Epoch) ->
    system_time_to_iso8601(1000 * Epoch, nanosecond).

-spec system_time_to_iso8601(integer(), microsecond | nanosecond) -> binary().
system_time_to_iso8601(Epoch, Unit) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(Epoch, [{unit, Unit}, {offset, "Z"}])).

%%%_* Private functions ================================================
%% The arguments are redacted before they are rendered: once they are text
%% there is no key left to tell a secret by. A redacted argument may no
%% longer be what its control sequence takes, and is then printed as a term.
-spec rendered(io:format(), [term()], config()) -> io_lib:chars().
rendered(Format, Args, Config) ->
    Redacted = redacted_args(Args, Config),
    try
        io_lib:format(Format, Redacted)
    catch
        error:_ ->
            io_lib:format("~0tp ~0tp", [Format, Redacted], [{chars_limit, ?PRINTED_CHARS_LIMIT}])
    end.

pre_encode(Data, Config) ->
    #{jsonify(K) => pre_encode_value(V, Config) || K := V <- Data}.

pre_encode_value(V, Config) when is_map(V) ->
    pre_encode(V, Config);
pre_encode_value([First | _] = Vs, Config) when is_map(First) ->
    [pre_encode(V, Config) || V <- Vs, is_map(V)];
pre_encode_value(V, _Config) ->
    jsonify(V).

%% The arguments are redacted as one list, so that a list of pairs is taken
%% for the proplist it is.
-spec redacted_args([term()], config()) -> [term()].
redacted_args(Args, Config) ->
    case redact(Args, Config) of
        Redacted when is_list(Redacted) -> Redacted;
        Other -> [Other]
    end.

redact(Data, #{redact := true}) ->
    nova_redact:term(Data);
redact(Data, _Config) ->
    Data.

merge_meta(Msg, Meta0, Config) ->
    Meta1 = meta_without(Meta0, Config),
    Meta2 = meta_with(Meta1, Config),
    maps:merge(Msg, Meta2).

encode(Data, Config) ->
    Json = nova_json:encode(Data),
    case new_line(Config) of
        true -> [Json, new_line_type(Config)];
        false -> Json
    end.

jsonify(A) when is_atom(A) -> A;
jsonify(B) when is_binary(B) ->
    case unicode:characters_to_binary(B) of
        B -> B;
        _NotUtf8 -> printed(B)
    end;
jsonify(I) when is_integer(I) -> I;
jsonify(F) when is_float(F) -> F;
jsonify(B) when is_boolean(B) -> B;
jsonify(P) when is_pid(P) -> jsonify(pid_to_list(P));
jsonify(P) when is_port(P) -> jsonify(port_to_list(P));
jsonify(F) when is_function(F) -> jsonify(erlang:fun_to_list(F));
jsonify(L) when is_list(L) ->
    try unicode:characters_to_binary(L) of
        S when is_binary(S) -> S;
        _NotUtf8 -> printed(L)
    catch
        error:badarg ->
            printed(L)
    end;
jsonify({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    <<(atom_to_binary(M, utf8))/binary, $:, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>;
jsonify(Any) ->
    printed(Any).

printed(Term) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Term], [{chars_limit, ?PRINTED_CHARS_LIMIT}])).

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

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decoded(Json) ->
    json:decode(iolist_to_binary(Json)).

-define(assertJSONEqual(Expected, Actual),
    ?assertEqual(decoded(Expected), decoded(Actual))
).

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
            time => fun(Epoch) -> Epoch + 1 end,
            level => fun(alert) -> info end
        }
    },
    ?assertJSONEqual(
        <<"{\"level\":\"info\",\"text\":\"derp\",\"time\":2}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{time => 1}}, Config1)
    ),

    Config2 = #{
        format_funs => #{
            time => fun(Epoch) -> Epoch + 1 end,
            foobar => fun(alert) -> info end
        }
    },
    ?assertJSONEqual(
        <<"{\"level\":\"alert\",\"text\":\"derp\",\"time\":2}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{time => 1}}, Config2)
    ).

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
    ),

    Config3 = #{
        key_mapping => #{
            level => lvl,
            foobar => level
        }
    },
    ?assertJSONEqual(
        <<"{\"lvl\":\"alert\",\"text\":\"derp\"}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{}}, Config3)
    ),

    Config4 = #{
        key_mapping => #{time => timestamp},
        format_funs => #{timestamp => fun(T) -> T + 1 end}
    },
    ?assertJSONEqual(
        <<"{\"level\":\"alert\",\"text\":\"derp\",\"timestamp\":2}">>,
        format(#{level => alert, msg => {string, "derp"}, meta => #{time => 1}}, Config4)
    ).

list_format_test() ->
    ErrorReport =
        #{
            level => error,
            meta => #{time => 1},
            msg => {report, #{report => [{hej, "hopp"}]}}
        },
    ?assertJSONEqual(
        <<"{\"level\":\"error\",\"report\":\"[{hej,\\\"hopp\\\"}]\",\"time\":1}">>,
        format(ErrorReport, #{})
    ).

invalid_utf8_test() ->
    Report = #{
        id => <<255, 0>>,
        parts => [<<"ok ">>, <<255>>],
        name => [114, 228, 107, 97]
    },
    #{<<"id">> := Id, <<"name">> := Name, <<"parts">> := Parts} =
        decoded(format(#{level => info, msg => {report, Report}, meta => #{}}, #{})),
    ?assertEqual(<<"<<255,0>>">>, Id),
    ?assertEqual(<<"r", 195, 164, "ka">>, Name),
    ?assert(is_binary(Parts)).

printed_term_is_bounded_test() ->
    Report = #{
        id => binary:copy(<<255, 0>>, 512 * 1024),
        parts => lists:duplicate(100000, <<255, 0>>),
        term => {reason, lists:seq(1, 100000)},
        short => {reason, <<255, 0>>}
    },
    Json = iolist_to_binary(format(#{level => info, msg => {report, Report}, meta => #{}}, #{})),
    #{~"id" := Id, ~"parts" := Parts, ~"term" := Term, ~"short" := Short} = json:decode(Json),
    ?assertMatch(<<"<<255,0,255,0,", _/binary>>, Id),
    ?assertMatch(<<"[<<255,0>>,<<255,0>>,", _/binary>>, Parts),
    ?assertMatch(<<"{reason,[1,2,3,", _/binary>>, Term),
    [?assert(byte_size(Printed) < 2 * 4096) || Printed <- [Id, Parts, Term]],
    ?assertEqual(~"{reason,<<255,0>>}", Short).

meta_without_test() ->
    Error = #{
        level => info,
        msg => {report, #{answer => 42}},
        meta => #{secret => xyz}
    },
    ?assertEqual(
        {ok, #{
            <<"answer">> => 42,
            <<"level">> => <<"info">>,
            <<"secret">> => <<"xyz">>
        }},
        {ok, decoded(format(Error, #{}))}
    ),
    Config2 = #{meta_without => [secret]},
    ?assertEqual(
        {ok, #{
            <<"answer">> => 42,
            <<"level">> => <<"info">>
        }},
        {ok, decoded(format(Error, Config2))}
    ),
    ok.

meta_with_test() ->
    Error = #{
        level => info,
        msg => {report, #{answer => 42}},
        meta => #{secret => xyz}
    },
    ?assertEqual(
        {ok, #{
            <<"answer">> => 42,
            <<"level">> => <<"info">>,
            <<"secret">> => <<"xyz">>
        }},
        {ok, decoded(format(Error, #{}))}
    ),
    Config2 = #{meta_with => [level]},
    ?assertEqual(
        {ok, #{
            <<"answer">> => 42,
            <<"level">> => <<"info">>
        }},
        {ok, decoded(format(Error, Config2))}
    ),
    ok.

redact_test() ->
    Req = #{pid => self(), streamid => 1, method => ~"GET", path => ~"/login",
            headers => #{~"cookie" => ~"session_id=abc"}},
    Event = #{level => error,
              msg => {report, #{req => Req, access_token => ~"t", user => ~"ada",
                                params => #{~"password_confirmation" => ~"p", ~"name" => ~"ada"}}},
              meta => #{client_secret => ~"s", time => 1}},
    Redacted = json:decode(iolist_to_binary(format(Event, #{redact => true}))),
    ?assertEqual(#{~"level" => ~"error", ~"time" => 1, ~"user" => ~"ada",
                   ~"access_token" => ~"redacted", ~"client_secret" => ~"redacted",
                   ~"params" => #{~"password_confirmation" => ~"redacted", ~"name" => ~"ada"},
                   ~"req" => #{~"method" => ~"GET", ~"path" => ~"/login", ~"redacted" => ~"nova_req"}},
                 Redacted),
    [begin
         #{~"access_token" := Token, ~"client_secret" := Secret, ~"req" := #{~"headers" := Headers}} =
             json:decode(iolist_to_binary(format(Event, Config))),
         ?assertEqual({~"t", ~"s", #{~"cookie" => ~"session_id=abc"}}, {Token, Secret, Headers})
     end || Config <- [#{}, #{redact => false}]].

newline_test() ->
    ConfigDefault = #{new_line => true},
    ?assertEqual(
        <<"{\"level\":\"alert\",\"text\":\"derp\"}\n">>,
        iolist_to_binary(format(#{level => alert, msg => {string, "derp"}, meta => #{}}, ConfigDefault))
    ),
    ConfigCRLF = #{
        new_line_type => crlf,
        new_line => true
    },
    ?assertEqual(
        <<"{\"level\":\"alert\",\"text\":\"derp\"}\r\n">>,
        iolist_to_binary(format(#{level => alert, msg => {string, "derp"}, meta => #{}}, ConfigCRLF))
    ).

newline_types_test() ->
    Base = #{level => alert, msg => {string, "x"}, meta => #{}},
    %% nl
    ?assertMatch([_Json, <<"\n">>], format(Base, #{new_line => true, new_line_type => nl})),
    %% unix
    ?assertMatch([_Json, <<"\n">>], format(Base, #{new_line => true, new_line_type => unix})),
    %% crlf
    ?assertMatch([_Json, <<"\r\n">>], format(Base, #{new_line => true, new_line_type => crlf})),
    %% windows
    ?assertMatch([_Json, <<"\r\n">>], format(Base, #{new_line => true, new_line_type => windows})),
    %% cr
    ?assertMatch([_Json, <<"\r">>], format(Base, #{new_line => true, new_line_type => cr})),
    %% macos9
    ?assertMatch([_Json, <<"\r">>], format(Base, #{new_line => true, new_line_type => macos9})),
    %% default (no new_line_type specified)
    ?assertMatch([_Json, <<"\n">>], format(Base, #{new_line => true})).

no_newline_test() ->
    Base = #{level => alert, msg => {string, "x"}, meta => #{}},
    Result = iolist_to_binary(format(Base, #{})),
    ?assertEqual(<<"{\"level\":\"alert\",\"text\":\"x\"}">>, Result).

jsonify_types_test() ->
    %% atom
    ?assertEqual(hello, jsonify(hello)),
    %% binary
    ?assertEqual(<<"bin">>, jsonify(<<"bin">>)),
    %% integer
    ?assertEqual(42, jsonify(42)),
    %% float
    ?assertEqual(3.14, jsonify(3.14)),
    %% boolean
    ?assertEqual(true, jsonify(true)),
    ?assertEqual(false, jsonify(false)),
    %% pid
    ?assert(is_binary(jsonify(self()))),
    %% function
    ?assert(is_binary(jsonify(fun erlang:monotonic_time/0))),
    %% list (string)
    ?assertEqual(<<"hello">>, jsonify("hello")),
    %% list (non-string, falls through to io_lib:format)
    ?assert(is_binary(jsonify([{a, b}]))),
    %% MFA tuple
    ?assertEqual(<<"lists:reverse/1">>, jsonify({lists, reverse, 1})),
    %% arbitrary term
    ?assert(is_binary(jsonify(make_ref()))).

format_keyval_list_test() ->
    Event = #{level => info, msg => {report, [{key, <<"val">>}]}, meta => #{}},
    Decoded = decoded(format(Event, #{})),
    ?assertEqual(<<"val">>, maps:get(<<"key">>, Decoded)).

format_format_terms_test() ->
    Event = #{level => info, msg => {"hello ~s", ["world"]}, meta => #{}},
    Decoded = decoded(format(Event, #{})),
    ?assertEqual(<<"hello world">>, maps:get(<<"text">>, Decoded)).

format_error_logger_test() ->
    Event = #{level => error,
              msg => {report, #{format => "error: ~p", args => [bad], label => {error_logger, error}}},
              meta => #{}},
    Decoded = decoded(format(Event, #{})),
    ?assert(maps:is_key(<<"text">>, Decoded)).

nested_map_test() ->
    Event = #{level => info, msg => {report, #{outer => #{inner => value}}}, meta => #{}},
    Decoded = decoded(format(Event, #{})),
    ?assertEqual(#{<<"inner">> => <<"value">>}, maps:get(<<"outer">>, Decoded)).

list_of_maps_test() ->
    Event = #{level => info, msg => {report, #{items => [#{a => 1}, #{b => 2}]}}, meta => #{}},
    Decoded = decoded(format(Event, #{})),
    ?assertEqual([#{<<"a">> => 1}, #{<<"b">> => 2}], maps:get(<<"items">>, Decoded)).

system_time_to_iso8601_test() ->
    Epoch = erlang:system_time(microsecond),
    Result = system_time_to_iso8601(Epoch),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 10).

system_time_to_iso8601_nano_test() ->
    Epoch = erlang:system_time(microsecond),
    Result = system_time_to_iso8601_nano(Epoch),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 10).

format_port_test() ->
    %% Ports are jsonified as binaries
    Port = hd(erlang:ports()),
    Result = jsonify(Port),
    ?assert(is_binary(Result)).

-endif.
