-module(nova_settings_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").

-define(KEYS, [trusted_origins, secret_key_base, csrf_token, max_sessions, non_canonical_paths,
               plugins_before_security]).

%% The settings as a boot finds them with Env in the nova environment: the
%% value of every key, or what the boot is stopped with.
booted_with(Env) ->
    [application:set_env(nova, Key, Value) || {Key, Value} <- Env],
    try
        ok = nova_settings:validate(),
        #{Key => nova_settings:value(Key) || Key <- ?KEYS}
    catch
        error:Reason -> {stopped, Reason}
    after
        [application:unset_env(nova, Key) || {Key, _Value} <- Env],
        nova_settings:validate()
    end.

defaults_test() ->
    ?assertEqual(#{trusted_origins => same_host, secret_key_base => [], csrf_token => undefined,
                   max_sessions => 1000000, non_canonical_paths => reject, plugins_before_security => []},
                 booted_with([])).

values_that_can_be_used_test_() ->
    [?_assertMatch({Key, Value, #{Key := Kept}}, {Key, Value, booted_with([{Key, Value}])})
     || {Key, Value, Kept} <- [{non_canonical_paths, reject, reject},
                               {non_canonical_paths, resolve, resolve},
                               {trusted_origins, same_host, same_host},
                               {trusted_origins, same_origin, same_origin},
                               {trusted_origins, any, any},
                               {trusted_origins, [], []},
                               {trusted_origins, [~"https://example.com", "https://other.example.com"],
                                [~"https://example.com", ~"https://other.example.com"]},
                               {csrf_token, session, session},
                               {secret_key_base, ?SECRET, [nova_secret:derived(?SECRET)]},
                               {secret_key_base, [?SECRET, <<?SECRET/binary, "2">>],
                                [nova_secret:derived(?SECRET), nova_secret:derived(<<?SECRET/binary, "2">>)]},
                               {max_sessions, 1, 1},
                               {max_sessions, infinity, infinity},
                               {plugins_before_security, all, all},
                               {plugins_before_security, [nova_request_plugin], [nova_request_plugin]}]].

values_that_stop_the_boot_test_() ->
    [?_assertEqual({stopped, {invalid_setting, Key, Value}}, booted_with([{Key, Value}]))
     || {Key, Value} <- [{non_canonical_paths, allow},
                         {non_canonical_paths, true},
                         {trusted_origins, none},
                         {trusted_origins, true},
                         {trusted_origins, ~"https://example.com"},
                         {trusted_origins, [same_host]},
                         {trusted_origins, [[same_host]]},
                         {trusted_origins, nova_test_helper:improper_list([~"https://example.com"],
                                                                          ~"https://other.example.com")},
                         {trusted_origins, [nova_test_helper:improper_list([$h], $t)]},
                         {csrf_token, signed},
                         {csrf_token, true},
                         {max_sessions, 0},
                         {max_sessions, -1},
                         {max_sessions, many},
                         {max_sessions, 1.5},
                         {plugins_before_security, none},
                         {plugins_before_security, nova_request_plugin},
                         {plugins_before_security, [~"nova_request_plugin"]},
                         {plugins_before_security, nova_test_helper:improper_list([nova_request_plugin],
                                                                                  nova_csrf_plugin)},
                         {plugins_before_security, [all]},
                         {plugins_before_security, [nova_reqeust_plugin]},
                         {plugins_before_security, [nova_request_plugin, nova_reqeust_plugin]},
                         {plugins_before_security, [nova_cors_plugin]},
                         {plugins_before_security, [lists]}]].

csrf_settings_test_() ->
    Short = binary:copy(~"s", 31),
    Tokens = fun(#{csrf_token := Token, secret_key_base := Keys}) -> {Token, length(Keys)};
                (Stopped) -> Stopped
             end,
    [{lists:flatten(io_lib:format("~0p", [Env])), ?_assertEqual(Expected, Tokens(booted_with(Env)))}
     || {Env, Expected} <- [{[{csrf_token, stateless}, {secret_key_base, ?SECRET}], {stateless, 1}},
                            {[{csrf_token, session}, {secret_key_base, ?SECRET}], {session, 1}},
                            {[{csrf_token, stateless}], {stopped, {invalid_setting, secret_key_base, not_set}}},
                            {[{csrf_token, signed}, {secret_key_base, ?SECRET}],
                             {stopped, {invalid_setting, csrf_token, signed}}},
                            {[{secret_key_base, Short}], {stopped, {invalid_setting, secret_key_base, redacted}}},
                            {[{secret_key_base, [?SECRET, Short]}], {stopped, {invalid_setting, secret_key_base, redacted}}},
                            {[{secret_key_base, []}], {stopped, {invalid_setting, secret_key_base, redacted}}},
                            {[{secret_key_base, "a string of thirty-two characters or more"}],
                             {stopped, {invalid_setting, secret_key_base, redacted}}},
                            {[{csrf_token, session}, {secret_key_base, Short}],
                             {stopped, {invalid_setting, secret_key_base, redacted}}}]].

%% A signed token is for the session cookie of a client, which a node
%% without sessions does not hand out.
stateless_tokens_without_sessions_stop_the_boot_test_() ->
    Off = {use_sessions, false},
    [?_assertEqual({stopped, {invalid_setting, csrf_token, {requires, use_sessions}}},
                   booted_with([Off, {csrf_token, stateless}, {secret_key_base, ?SECRET}])),
     ?_assertMatch(#{csrf_token := undefined, secret_key_base := [_]}, booted_with([Off, {secret_key_base, ?SECRET}])),
     ?_assertMatch(#{csrf_token := session}, booted_with([Off, {csrf_token, session}])),
     ?_assertMatch(#{csrf_token := stateless},
                   booted_with([{use_sessions, true}, {csrf_token, stateless}, {secret_key_base, ?SECRET}]))].

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
