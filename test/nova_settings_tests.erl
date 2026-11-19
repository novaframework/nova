-module(nova_settings_tests).

-include_lib("eunit/include/eunit.hrl").

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
