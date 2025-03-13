-module(nova_jsonlogger).
%%%_* Exports ==========================================================
-export([format/2,
         system_time_to_iso8601/1,
         system_time_to_iso8601_nano/1]).

%%%_* Types ============================================================
-type config() :: #{
    new_line => boolean(),
    new_line_type => nl | crlf | cr | unix | windows | macos9,
    key_mapping => #{atom() => atom()},
    format_funs => #{atom() => fun((_) -> _)}
}.

-export_type([config/0]).

%%%_* Macros ===========================================================
%%%_* Options ----------------------------------------------------------
-define(NEW_LINE, false).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec format(logger:log_event(), config()) -> unicode:chardata().
format(
    #{msg := {report, #{format := Format, args := Args, label := {error_logger, _}}}} = Map, Config
) ->
    Report = #{text => io_lib:format(Format, Args)},
    format(Map#{msg := {report, Report}}, Config);
format(#{level := Level, msg := {report, Msg}, meta := Meta}, Config) when is_map(Msg) ->
    Data0 = merge_meta(Msg, Meta#{level => Level}, Config),
    Data1 = apply_key_mapping(Data0, Config),
    Data2 = apply_format_funs(Data1, Config),
    encode(pre_encode(Data2, Config), Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
    format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
    Report = #{text => unicode:characters_to_binary(String)},
    format(Map#{msg := {report, Report}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
    format(Map#{msg := {string, io_lib:format(Format, Terms)}}, Config).

%%% Useful for converting logger:timestamp() to a readable timestamp.
-spec system_time_to_iso8601(integer()) -> binary().
system_time_to_iso8601(Epoch) ->
    system_time_to_iso8601(Epoch, microsecond).

-spec system_time_to_iso8601_nano(integer()) -> binary().
system_time_to_iso8601_nano(Epoch) ->
    system_time_to_iso8601(1000 * Epoch, nanosecond).

-spec system_time_to_iso8601(integer(), erlang:time_unit()) -> binary().
system_time_to_iso8601(Epoch, Unit) ->
    binary:list_to_bin(calendar:system_time_to_rfc3339(Epoch, [{unit, Unit}, {offset, "Z"}])).

%%%_* Private functions ================================================
pre_encode(Data, Config) ->
    maps:fold(
        fun
            (K, V, Acc) when is_map(V) ->
                maps:put(jsonify(K), pre_encode(V, Config), Acc);
            % assume list of maps
            (K, Vs, Acc) when is_list(Vs), is_map(hd(Vs)) ->
                maps:put(jsonify(K), [pre_encode(V, Config) || V <- Vs, is_map(V)], Acc);
            (K, V, Acc) ->
                maps:put(jsonify(K), jsonify(V), Acc)
        end,
        maps:new(),
        Data
    ).

merge_meta(Msg, Meta0, Config) ->
    Meta1 = meta_without(Meta0, Config),
    Meta2 = meta_with(Meta1, Config),
    maps:merge(Msg, Meta2).

encode(Data, Config) ->
    JsonLib = nova:get_env(json_lib, thoas),
    Json = JsonLib:encode(Data),
    case new_line(Config) of
        true -> [Json, new_line_type(Config)];
        false -> Json
    end.

jsonify(A) when is_atom(A) -> A;
jsonify(B) when is_binary(B) -> B;
jsonify(I) when is_integer(I) -> I;
jsonify(F) when is_float(F) -> F;
jsonify(B) when is_boolean(B) -> B;
jsonify(P) when is_pid(P) -> jsonify(pid_to_list(P));
jsonify(P) when is_port(P) -> jsonify(port_to_list(P));
jsonify(F) when is_function(F) -> jsonify(erlang:fun_to_list(F));
jsonify(L) when is_list(L) ->
    try list_to_binary(L) of
        S -> S
    catch
        error:badarg ->
            unicode:characters_to_binary(io_lib:format("~0p", [L]))
    end;
jsonify({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    <<(atom_to_binary(M, utf8))/binary, $:, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>;
jsonify(Any) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Any])).

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

-define(assertJSONEqual(Expected, Actual),
    ?assertEqual(thoas:decode(Expected), thoas:decode(Actual))
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
        thoas:decode(format(Error, #{}))
    ),
    Config2 = #{meta_without => [secret]},
    ?assertEqual(
        {ok, #{
            <<"answer">> => 42,
            <<"level">> => <<"info">>
        }},
        thoas:decode(format(Error, Config2))
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
        thoas:decode(format(Error, #{}))
    ),
    Config2 = #{meta_with => [level]},
    ?assertEqual(
        {ok, #{
            <<"answer">> => 42,
            <<"level">> => <<"info">>
        }},
        thoas:decode(format(Error, Config2))
    ),
    ok.

newline_test() ->
    ConfigDefault = #{new_line => true},
    ?assertEqual(
        [<<"{\"level\":\"alert\",\"text\":\"derp\"}">>, <<"\n">>],
        format(#{level => alert, msg => {string, "derp"}, meta => #{}}, ConfigDefault)
    ),
    ConfigCRLF = #{
        new_line_type => crlf,
        new_line => true
    },
    ?assertEqual(
        [<<"{\"level\":\"alert\",\"text\":\"derp\"}">>, <<"\r\n">>],
        format(#{level => alert, msg => {string, "derp"}, meta => #{}}, ConfigCRLF)
    ).

-endif.