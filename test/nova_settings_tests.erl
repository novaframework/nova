-module(nova_settings_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").
-define(KEYS, [allowed_origins, csrf_secret, csrf_token, max_sessions, non_canonical_paths,
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
    ?assertEqual(#{allowed_origins => same_host, csrf_secret => [], csrf_token => undefined,
                   max_sessions => 1000000, non_canonical_paths => reject, plugins_before_security => []},
                 booted_with([])).

values_that_can_be_used_test_() ->
    [?_assertMatch({Key, Value, #{Key := Kept}}, {Key, Value, booted_with([{Key, Value}])})
     || {Key, Value, Kept} <- [{non_canonical_paths, reject, reject},
                               {non_canonical_paths, resolve, resolve},
                               {allowed_origins, same_host, same_host},
                               {allowed_origins, same_origin, same_origin},
                               {allowed_origins, any, any},
                               {allowed_origins, [], []},
                               {allowed_origins, [~"https://example.com", "https://other.example.com"],
                                [~"https://example.com", ~"https://other.example.com"]},
                               {csrf_token, session, session},
                               {csrf_secret, ?SECRET, [?SECRET]},
                               {csrf_secret, [?SECRET, <<?SECRET/binary, "2">>], [?SECRET, <<?SECRET/binary, "2">>]},
                               {max_sessions, 1, 1},
                               {max_sessions, infinity, infinity},
                               {plugins_before_security, all, all},
                               {plugins_before_security, [nova_request_plugin], [nova_request_plugin]}]].

values_that_stop_the_boot_test_() ->
    [?_assertEqual({stopped, {invalid_setting, Key, Value}}, booted_with([{Key, Value}]))
     || {Key, Value} <- [{non_canonical_paths, allow},
                         {non_canonical_paths, true},
                         {allowed_origins, none},
                         {allowed_origins, true},
                         {allowed_origins, ~"https://example.com"},
                         {allowed_origins, [same_host]},
                         {allowed_origins, [[same_host]]},
                         {allowed_origins, [~"https://example.com" | ~"https://other.example.com"]},
                         {allowed_origins, [[$h | $t]]},
                         {csrf_token, signed},
                         {csrf_token, true},
                         {max_sessions, 0},
                         {max_sessions, -1},
                         {max_sessions, many},
                         {max_sessions, 1.5},
                         {plugins_before_security, none},
                         {plugins_before_security, nova_request_plugin},
                         {plugins_before_security, [~"nova_request_plugin"]},
                         {plugins_before_security, [nova_request_plugin | nova_csrf_plugin]},
                         {plugins_before_security, [all]},
                         {plugins_before_security, [nova_reqeust_plugin]},
                         {plugins_before_security, [nova_request_plugin, nova_reqeust_plugin]},
                         {plugins_before_security, [nova_cors_plugin]},
                         {plugins_before_security, [lists]}]].

csrf_settings_test_() ->
    Short = binary:copy(~"s", 31),
    Tokens = fun(#{csrf_token := Token, csrf_secret := Secrets}) -> {Token, Secrets};
                (Stopped) -> Stopped
             end,
    [{lists:flatten(io_lib:format("~0p", [Env])), ?_assertEqual(Expected, Tokens(booted_with(Env)))}
     || {Env, Expected} <- [{[{csrf_token, stateless}, {csrf_secret, ?SECRET}], {stateless, [?SECRET]}},
                            {[{csrf_token, session}, {csrf_secret, ?SECRET}], {session, [?SECRET]}},
                            {[{csrf_token, stateless}], {stopped, {invalid_setting, csrf_secret, not_set}}},
                            {[{csrf_token, signed}, {csrf_secret, ?SECRET}],
                             {stopped, {invalid_setting, csrf_token, signed}}},
                            {[{csrf_secret, Short}], {stopped, {invalid_setting, csrf_secret, redacted}}},
                            {[{csrf_secret, [?SECRET, Short]}], {stopped, {invalid_setting, csrf_secret, redacted}}},
                            {[{csrf_secret, []}], {stopped, {invalid_setting, csrf_secret, redacted}}},
                            {[{csrf_secret, "a string of thirty-two characters or more"}],
                             {stopped, {invalid_setting, csrf_secret, redacted}}},
                            {[{csrf_token, session}, {csrf_secret, Short}],
                             {stopped, {invalid_setting, csrf_secret, redacted}}}]].

%% A value that is not a secret, or not a list of them, is refused whatever
%% it is, and what it holds is in nothing that is raised or logged.
secret_that_cannot_be_used_is_not_told_test_() ->
    Old = ~"the-secret-in-use-of-32-bytes-or-more",
    New = ~"the-secret-to-come-of-32-bytes-or-more",
    [{lists:flatten(io_lib:format("~0p", [Shape])),
      fun() ->
              application:set_env(nova, csrf_secret, Value),
              try
                  {Raised, Events} = nova_test_log:collect(fun() ->
                                                                   try nova_settings:validate()
                                                                   catch Class:Reason:Stacktrace ->
                                                                           {Class, Reason, Stacktrace}
                                                                   end
                                                           end),
                  ?assertMatch({error, {invalid_setting, csrf_secret, redacted}, _}, Raised),
                  ?assertMatch([#{setting := csrf_secret, value := redacted}], nova_test_log:errors(Events)),
                  Told = iolist_to_binary(io_lib:format("~0p", [{Raised, Events}])),
                  [?assertEqual({Secret, nomatch}, {Secret, binary:match(Told, Secret)}) || Secret <- [Old, New]]
              after
                  nova_test_settings:unset(csrf_secret)
              end
      end}
     || {Shape, Value} <- [{too_short, binary:part(Old, 0, 31)},
                           {improper_list, [New | Old]},
                           {improper_tail, [New, Old | New]},
                           {nested_list, [[New, Old]]},
                           {pair, {New, Old}},
                           {map, #{New => Old}},
                           {string, binary_to_list(Old)},
                           {one_too_short, [New, binary:part(Old, 0, 31)]},
                           {empty_list, []}]].

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
     || {Key, Value} <- [{max_sessions, 7}, {csrf_token, session}, {csrf_secret, ?SECRET},
                         {allowed_origins, any}, {non_canonical_paths, resolve}, {plugins_before_security, all}]],
    try
        ?assertEqual(booted_with([]), booted_with([{max_sessions, 1000000}]))
    after
        application:unset_env(nova, bootstrap_application),
        [application:unset_env(nova_settings_tests_app, Key) || Key <- ?KEYS]
    end.
