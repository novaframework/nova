-module(nova_settings_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").
-define(KEYS, [trusted_origins, secret_key_base, csrf_token, max_sessions, non_canonical_paths,
               plugins_before_security, server_grace, shutdown_delay, shutdown_drain_timeout]).

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
                   max_sessions => 1000000, non_canonical_paths => reject, plugins_before_security => [],
                   server_grace => 10000, shutdown_delay => 0, shutdown_drain_timeout => 15000},
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
                               {server_grace, 0, 0},
                               {server_grace, 60000, 60000},
                               {server_grace, 16#FFFFFFFF, 16#FFFFFFFF},
                               {server_grace, infinity, infinity},
                               {shutdown_delay, 0, 0},
                               {shutdown_delay, 5000, 5000},
                               {shutdown_delay, 16#FFFFFFFF, 16#FFFFFFFF},
                               {shutdown_drain_timeout, 0, 0},
                               {shutdown_drain_timeout, 200, 200},
                               {shutdown_drain_timeout, 16#FFFFFFFF, 16#FFFFFFFF},
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
                         {server_grace, -1},
                         {server_grace, 16#FFFFFFFF + 1},
                         {server_grace, "10s"},
                         {server_grace, 1.5},
                         {server_grace, never},
                         {shutdown_delay, -1},
                         {shutdown_delay, 16#FFFFFFFF + 1},
                         {shutdown_delay, infinity},
                         {shutdown_delay, "5000"},
                         {shutdown_delay, 0.5},
                         {shutdown_drain_timeout, -1},
                         {shutdown_drain_timeout, 16#FFFFFFFF + 1},
                         {shutdown_drain_timeout, infinity},
                         {shutdown_drain_timeout, "15s"},
                         {shutdown_drain_timeout, 1.5},
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

%% How long the node waits as it stops is read when it does, so what is
%% set while it runs applies. One that cannot be used then is not what the
%% node waits.
current_test_() ->
    Current = fun(Booted, Now) -> current(shutdown_drain_timeout, Booted, Now) end,
    Warned = fun(Events) -> [Value || {warning, {report, #{setting := shutdown_drain_timeout, value := Value,
                                                           used := 300}}} <- Events]
             end,
    [fun() ->
             {Value, Events} = Current(300, 200),
             ?assertEqual({200, []}, {Value, Warned(Events)})
     end,
     fun() ->
             {Value, Events} = Current(300, never),
             ?assertEqual({300, [never]}, {Value, Warned(Events)})
     end,
     fun() ->
             {Value, Events} = Current(300, -6000),
             ?assertEqual({300, [-6000]}, {Value, Warned(Events)})
     end,
     fun() ->
             application:unset_env(nova, shutdown_drain_timeout),
             ?assertEqual(15000, nova_settings:current(shutdown_drain_timeout))
     end,
     ?_assertMatch({2000, _}, current(shutdown_delay, 1000, 2000)),
     ?_assertMatch({1000, _}, current(shutdown_delay, 1000, infinity)),
     ?_assertMatch({1000, _}, current(shutdown_delay, 1000, "2000"))].

current(Key, Booted, Now) ->
    application:set_env(nova, Key, Booted),
    try
        ok = nova_settings:validate(),
        application:set_env(nova, Key, Now),
        nova_test_log:collect(fun() -> nova_settings:current(Key) end)
    after
        application:unset_env(nova, Key),
        nova_settings:validate()
    end.

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

%% What `base64:encode(crypto:strong_rand_bytes(48))`, `openssl rand -base64 48`
%% and `openssl rand -hex 32` give, and 32 bytes as they come.
secret_of_a_random_source_is_taken_test_() ->
    [{lists:flatten(io_lib:format("~0p", [Shape])),
      ?_assertMatch(#{secret_key_base := [_ | _]}, booted_with([{secret_key_base, Value}]))}
     || {Shape, Value} <- [{base64, ~"pT4mQ0fYxk8Hn2cWv7sJ1dLrA9uEoZb3KiG5yNqXwC6tRhVlMeSaUjOgBfDzIP+/"},
                           {hex, ~"9f2c41d7a0b3e85c6d1f7a924be03c58d6a1f09e7b34c2d85a60f1e39c7b4d20"},
                           {bytes, binary:decode_hex(~"9f2c41d7a0b3e85c6d1f7a924be03c58d6a1f09e7b34c2d85a60f1e39c7b4d20")},
                           {eight_bytes, ~"abcdefghabcdefghhgfedcbahgfedcba"},
                           {two, [?SECRET, ~"9f2c41d7a0b3e85c6d1f7a924be03c58"]}]].

%% A setting that was in no release and is gone stops the boot, and what is
%% raised and logged names the one that took its place and not the value.
removed_settings_stop_the_boot_test_() ->
    [{atom_to_list(Key),
      fun() ->
              application:set_env(nova, Key, Value),
              try
                  {Raised, Events} = nova_test_log:collect(fun() ->
                                                                   try nova_settings:validate()
                                                                   catch error:Reason -> {stopped, Reason}
                                                                   end
                                                           end),
                  ?assertEqual({stopped, {invalid_setting, Key, {replaced_by, Replacement}}}, Raised),
                  ?assertMatch([#{setting := Key, replaced_by := Replacement, msg := _}],
                               nova_test_log:errors(Events)),
                  [#{msg := Notice} = Report] = nova_test_log:errors(Events),
                  ?assertNotEqual(nomatch, binary:match(Notice, atom_to_binary(Replacement))),
                  ?assertNot(is_map_key(value, Report)),
                  ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~0p", [{Raised, Events}])),
                                                     ?SECRET))
              after
                  application:unset_env(nova, Key),
                  nova_settings:validate()
              end
      end}
     || {Key, Value, Replacement} <- removed()].

removed() ->
    [{plugins_after_security, [nova_request_plugin], plugins_before_security},
     {allowed_origins, [~"https://example.com"], trusted_origins},
     {csrf_secret, ?SECRET, secret_key_base}].

invalid_setting_is_logged_with_what_it_may_be_test() ->
    application:set_env(nova, non_canonical_paths, allow),
    try
        {_, Events} = nova_test_log:collect(fun() -> ?assertError(_, nova_settings:validate()) end),
        ?assertEqual([#{msg => nova_settings:expected(non_canonical_paths), setting => non_canonical_paths,
                        value => allow}],
                     nova_test_log:errors(Events))
    after
        nova_test_settings:unset(non_canonical_paths)
    end.

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

%% The bootstrap application has no say in these.
settings_are_read_from_nova_alone_test() ->
    application:set_env(nova, bootstrap_application, nova_settings_tests_app),
    [application:set_env(nova_settings_tests_app, Key, Value)
     || {Key, Value} <- [{max_sessions, 7}, {csrf_token, session}, {secret_key_base, ?SECRET},
                         {trusted_origins, any}, {non_canonical_paths, resolve}, {plugins_before_security, all}]],
    try
        ?assertEqual(booted_with([]), booted_with([{max_sessions, 1000000}]))
    after
        application:unset_env(nova, bootstrap_application),
        [application:unset_env(nova_settings_tests_app, Key) || Key <- ?KEYS]
    end.
