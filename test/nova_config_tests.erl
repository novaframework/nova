-module(nova_config_tests).
-include_lib("eunit/include/eunit.hrl").

get_env_test_() ->
    {foreach,
     fun() ->
             Prev = application:get_env(nova, bootstrap_application),
             application:set_env(nova, bootstrap_application, nova_config_tests_app),
             Prev
     end,
     fun(Prev) ->
             application:unset_env(nova_config_tests_app, nova_config_test_key),
             application:unset_env(nova, nova_config_test_key),
             case Prev of
                 {ok, App} -> application:set_env(nova, bootstrap_application, App);
                 undefined -> application:unset_env(nova, bootstrap_application)
             end
     end,
     [
      fun default_when_unset/0,
      fun nova_env_when_bootstrap_unset/0,
      fun bootstrap_wins/0,
      fun no_bootstrap_application/0
     ]}.

default_when_unset() ->
    ?assertEqual(fallback, nova_config:get_env(nova_config_test_key, fallback)).

nova_env_when_bootstrap_unset() ->
    application:set_env(nova, nova_config_test_key, from_nova),
    ?assertEqual(from_nova, nova_config:get_env(nova_config_test_key, fallback)).

bootstrap_wins() ->
    application:set_env(nova, nova_config_test_key, from_nova),
    application:set_env(nova_config_tests_app, nova_config_test_key, from_app),
    ?assertEqual(from_app, nova_config:get_env(nova_config_test_key, fallback)).

no_bootstrap_application() ->
    application:unset_env(nova, bootstrap_application),
    application:set_env(nova, nova_config_test_key, from_nova),
    ?assertEqual(from_nova, nova_config:get_env(nova_config_test_key, fallback)).
