-module(nova_settings_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").

%% A value that is not a secret, or not a list of them, is refused whatever
%% it is, and what it holds is in nothing that is raised or logged.
secret_that_cannot_be_used_is_not_told_test_() ->
    Old = ~"the-secret-in-use-of-32-bytes-or-more",
    New = ~"the-secret-to-come-of-32-bytes-or-more",
    [{lists:flatten(io_lib:format("~0p", [Shape])),
      fun() ->
              application:set_env(nova, secret_key_base, Value),
              try
                  {Raised, Events} = nova_test_log:collect(fun() ->
                                                                   try nova_settings:validate()
                                                                   catch Class:Reason:Stacktrace ->
                                                                           {Class, Reason, Stacktrace}
                                                                   end
                                                           end),
                  ?assertMatch({error, {invalid_setting, secret_key_base, redacted}, _}, Raised),
                  ?assertMatch([#{setting := secret_key_base, value := redacted}], nova_test_log:errors(Events)),
                  Told = iolist_to_binary(io_lib:format("~0p", [{Raised, Events}])),
                  [?assertEqual({Secret, nomatch}, {Secret, binary:match(Told, Secret)}) || Secret <- [Old, New]]
              after
                  nova_test_settings:unset(secret_key_base)
              end
      end}
     || {Shape, Value} <- [{too_short, binary:part(Old, 0, 31)},
                           {improper_list, nova_test_helper:improper_list([New], Old)},
                           {improper_tail, nova_test_helper:improper_list([New, Old], New)},
                           {nested_list, [[New, Old]]},
                           {pair, {New, Old}},
                           {map, #{New => Old}},
                           {string, binary_to_list(Old)},
                           {one_too_short, [New, binary:part(Old, 0, 31)]},
                           {empty_list, []}]].

%% Everything Nova signs lets whoever sees it try secrets at home, so a
%% secret has to come from a random source. What plainly does not is
%% refused: one byte over and over, a few bytes over and over, the examples
%% the documentation has had, and the name of a variable nothing filled in.
%% Nothing here measures what a passphrase is worth.
secret_that_is_plainly_not_random_stops_the_boot_test_() ->
    [{lists:flatten(io_lib:format("~0p", [Shape])),
      fun() ->
              application:set_env(nova, secret_key_base, Value),
              try
                  {Raised, Events} = nova_test_log:collect(fun() ->
                                                                   try nova_settings:validate()
                                                                   catch Class:Reason:Stacktrace ->
                                                                           {Class, Reason, Stacktrace}
                                                                   end
                                                           end),
                  ?assertMatch({error, {invalid_setting, secret_key_base, redacted}, _}, Raised),
                  ?assertMatch([#{setting := secret_key_base, value := redacted}], nova_test_log:errors(Events)),
                  Told = iolist_to_binary(io_lib:format("~0p", [{Raised, Events}])),
                  [?assertEqual({Secret, nomatch}, {Secret, binary:match(Told, Written)})
                   || Secret <- lists:flatten([Value]),
                      Written <- [Secret, iolist_to_binary(io_lib:format("~0p", [Secret]))]]
              after
                  nova_test_settings:unset(secret_key_base)
              end
      end}
     || {Shape, Value} <- [{one_byte, binary:copy(~"s", 32)},
                           {one_byte_many_times, binary:copy(<<0>>, 512)},
                           {two_bytes, binary:copy(~"ab", 16)},
                           {a_word_repeated, binary:copy(~"changeme", 4)},
                           {another_word_repeated, binary:copy(~"password", 8)},
                           {seven_bytes_in_any_order, ~"abcdefgabcdefggfedcbagfedcbaabcdefg"},
                           {digits_of_a_date, ~"20261115202611152026111520261115"},
                           {example_of_the_documentation, ~"at least 32 bytes that every node of the system is given"},
                           {example_of_the_guides, ~"the-new-secret-of-at-least-32-bytes-0001"},
                           {other_example_of_the_guides, ~"the-old-secret-of-at-least-32-bytes-0000"},
                           {variable_nothing_filled_in, ~"${SECRET_KEY_BASE_OF_THE_PRODUCTION_NODES}"},
                           {one_of_two, [?SECRET, binary:copy(~"changeme", 4)]},
                           {other_of_two, [binary:copy(~"s", 64), ?SECRET]}]].

%% What is kept of a secret is the keys made from it, and not the secret.
secret_is_not_kept_test() ->
    ok = nova_test_settings:set(secret_key_base, ?SECRET),
    try
        Kept = iolist_to_binary(io_lib:format("~0p", [persistent_term:get(nova_settings)])),
        ?assertEqual(nomatch, binary:match(Kept, ?SECRET)),
        ?assertMatch([<<_:32/binary>>], nova_secret:keys(csrf_token))
    after
        nova_test_settings:unset(secret_key_base)
    end.

%% A boot keeps what it checked. What is put in the environment afterwards
%% is not read until the next one.
value_is_the_one_of_the_boot_test() ->
    ok = nova_test_settings:set(max_sessions, 5),
    try
        application:set_env(nova, max_sessions, 0),
        ?assertEqual(5, nova_settings:value(max_sessions)),
        ?assertError({invalid_setting, max_sessions, 0}, nova_settings:validate()),
        ?assertEqual(5, nova_settings:value(max_sessions))
    after
        nova_test_settings:unset(max_sessions)
    end.
