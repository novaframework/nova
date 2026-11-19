-module(nova_jsonlogger_tests).
-include_lib("eunit/include/eunit.hrl").

formatted(Msg, Config) ->
    iolist_to_binary(nova_jsonlogger:format(#{level => error, msg => Msg, meta => #{}}, Config)).

text_is_encoded_whatever_it_holds_test_() ->
    [?_assertEqual(#{~"level" => ~"error", ~"text" => Text}, json:decode(formatted(Msg, #{})))
     || {Msg, Text} <- [{{string, "plain"}, ~"plain"},
                        {{string, [~"deep ", ["chars ", 228]]}, <<"deep chars ä"/utf8>>},
                        {{"~s and ~p", ["one", two]}, ~"one and two"}]].

text_that_is_not_unicode_is_printed_as_a_term_test() ->
    ?assertMatch(#{~"text" := <<"<<", _/binary>>}, json:decode(formatted({string, <<255>>}, #{}))).
