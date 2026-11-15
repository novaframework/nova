-module(nova_jsonlogger_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"Bearer CANARYTOKEN").

request() ->
    #{pid => self(), streamid => 1, method => ~"GET", path => ~"/account",
      headers => #{~"authorization" => ?SECRET}}.

crash() ->
    {function_clause, [{some_controller, index, [request()], []}]}.

formatted(Msg, Config) ->
    iolist_to_binary(nova_jsonlogger:format(#{level => error, msg => Msg, meta => #{}}, Config)).

report_is_redacted_test() ->
    Line = formatted({report, #{req => request()}}, #{redact => true}),
    ?assertEqual(nomatch, binary:match(Line, ~"CANARYTOKEN")),
    ?assertMatch({_, _}, binary:match(Line, ~"/account")).

format_string_is_redacted_test() ->
    Line = formatted({"Ranch listener ~p had its request process exit with reason ~0p~n", [http, crash()]},
                     #{redact => true}),
    ?assertEqual(nomatch, binary:match(Line, ~"CANARYTOKEN")),
    ?assertMatch({_, _}, binary:match(Line, ~"Ranch listener http")),
    ?assertMatch({_, _}, binary:match(Line, ~"/account")).

error_logger_report_is_redacted_test() ->
    Report = #{label => {error_logger, error_msg}, format => "exit with reason ~0p", args => [crash()]},
    Line = formatted({report, Report}, #{redact => true}),
    ?assertEqual(nomatch, binary:match(Line, ~"CANARYTOKEN")),
    ?assertMatch({_, _}, binary:match(Line, ~"exit with reason")).

pairs_among_the_arguments_are_redacted_test() ->
    Line = formatted({"headers ~p", [[{~"authorization", ?SECRET}, {~"accept", ~"text/html"}]]}, #{redact => true}),
    ?assertEqual(nomatch, binary:match(Line, ~"CANARYTOKEN")),
    ?assertMatch({_, _}, binary:match(Line, ~"text/html")).

format_string_is_left_alone_without_redact_test() ->
    Line = formatted({"reason ~0p", [crash()]}, #{}),
    ?assertMatch({_, _}, binary:match(Line, ~"CANARYTOKEN")).

%% The formatter encodes the report itself and never calls report_cb, so
%% what a library would have printed through it is redacted with the rest.
report_with_a_report_cb_is_redacted_test() ->
    Callback = fun(Report) -> {"~p", [Report]} end,
    Event = #{level => error,
              msg => {report, #{label => {proc_lib, crash}, report => [[{initial_call, {m, f, [request()]}}]]}},
              meta => #{report_cb => Callback}},
    Line = iolist_to_binary(nova_jsonlogger:format(Event, #{redact => true})),
    ?assertEqual(nomatch, binary:match(Line, ~"CANARYTOKEN")).

arguments_that_no_longer_fit_the_format_are_printed_test() ->
    Line = formatted({"~s", [request()]}, #{redact => true}),
    ?assertEqual(nomatch, binary:match(Line, ~"CANARYTOKEN")),
    ?assertMatch({_, _}, binary:match(Line, ~"/account")).

text_is_encoded_whatever_it_holds_test_() ->
    [?_assertEqual(#{~"level" => ~"error", ~"text" => Text}, json:decode(formatted(Msg, #{})))
     || {Msg, Text} <- [{{string, "plain"}, ~"plain"},
                        {{string, [~"deep ", ["chars ", 228]]}, <<"deep chars ä"/utf8>>},
                        {{"~s and ~p", ["one", two]}, ~"one and two"}]].

text_that_is_not_unicode_is_printed_as_a_term_test() ->
    ?assertMatch(#{~"text" := <<"<<", _/binary>>}, json:decode(formatted({string, <<255>>}, #{}))).
