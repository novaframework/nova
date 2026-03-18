-module(nova_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    nova_test_helper:setup_nova_env().

cleanup(Prev) ->
    catch persistent_term:erase(nova_use_stacktrace),
    nova_test_helper:cleanup_nova_env(Prev).

nova_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun get_main_app_returns_nova/0,
      fun get_environment_returns_dev/0,
      fun get_env_returns_value/0,
      fun get_env_returns_default_when_missing/0,
      fun set_env_sets_value/0,
      fun set_env_error_when_no_main_app/0,
      fun use_stacktrace_true/0,
      fun use_stacktrace_false/0,
      fun format_stacktrace_basic/0,
      fun format_stacktrace_missing_info/0,
      fun detect_language_returns_erlang/0
     ]}.

get_main_app_returns_nova() ->
    ?assertEqual({ok, nova}, nova:get_main_app()).

get_environment_returns_dev() ->
    ?assertEqual(dev, nova:get_environment()).

get_env_returns_value() ->
    application:set_env(nova, test_param, <<"hello">>),
    ?assertEqual(<<"hello">>, nova:get_env(test_param, undefined)),
    application:unset_env(nova, test_param).

get_env_returns_default_when_missing() ->
    ?assertEqual(default_val, nova:get_env(nonexistent_param, default_val)).

set_env_sets_value() ->
    ok = nova:set_env(my_key, my_value),
    ?assertEqual(my_value, nova:get_env(my_key, undefined)),
    application:unset_env(nova, my_key).

set_env_error_when_no_main_app() ->
    application:unset_env(nova, bootstrap_application),
    ?assertEqual({error, main_app_not_found}, nova:set_env(key, val)),
    application:set_env(nova, bootstrap_application, nova).

use_stacktrace_true() ->
    nova:use_stacktrace(true),
    ?assertEqual(true, persistent_term:get(nova_use_stacktrace)).

use_stacktrace_false() ->
    persistent_term:put(nova_use_stacktrace, true),
    nova:use_stacktrace(false),
    ?assertEqual(undefined, persistent_term:get(nova_use_stacktrace, undefined)).

format_stacktrace_basic() ->
    Stack = [{my_mod, my_fun, 2, [{file, "my_mod.erl"}, {line, 42}]}],
    [Entry] = nova:format_stacktrace(Stack),
    ?assertEqual(my_mod, maps:get(module, Entry)),
    ?assertEqual(my_fun, maps:get(function, Entry)),
    ?assertEqual(2, maps:get(arity, Entry)),
    ?assertEqual("my_mod.erl", maps:get(file, Entry)),
    ?assertEqual(42, maps:get(line, Entry)).

format_stacktrace_missing_info() ->
    Stack = [{some_mod, some_fun, 0, []}],
    [Entry] = nova:format_stacktrace(Stack),
    ?assertEqual(some_mod, maps:get(module, Entry)),
    ?assertEqual(some_fun, maps:get(function, Entry)),
    ?assertEqual(0, maps:get(arity, Entry)),
    ?assertEqual(<<"unknown">>, maps:get(file, Entry)),
    ?assertEqual(-1, maps:get(line, Entry)).

detect_language_returns_erlang() ->
    ?assertEqual(erlang, nova:detect_language()).
