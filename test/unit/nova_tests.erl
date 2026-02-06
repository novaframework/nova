-module(nova_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

nova_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Get main app", fun test_get_main_app/0},
      {"Get main app - undefined", fun test_get_main_app_undefined/0},
      {"Get apps", fun test_get_apps/0},
      {"Get environment - default", fun test_get_environment_default/0},
      {"Get environment - custom", fun test_get_environment_custom/0},
      {"Get env - with main app", fun test_get_env_with_app/0},
      {"Get env - without main app", fun test_get_env_no_app/0},
      {"Set env - with main app", fun test_set_env_with_app/0},
      {"Set env - without main app", fun test_set_env_no_app/0},
      {"Use stacktrace - enable", fun test_use_stacktrace_enable/0},
      {"Use stacktrace - disable", fun test_use_stacktrace_disable/0},
      {"Format stacktrace", fun test_format_stacktrace/0}
     ]}.

setup() ->
    %% Clean up persistent_term
    catch persistent_term:erase(nova_use_stacktrace),
    ok.

cleanup(_) ->
    catch persistent_term:erase(nova_use_stacktrace),
    catch meck:unload(),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_get_main_app() ->
    %% Set a bootstrap application
    application:set_env(nova, bootstrap_application, test_app),

    Result = nova:get_main_app(),

    ?assertEqual({ok, test_app}, Result),

    %% Cleanup
    application:unset_env(nova, bootstrap_application).

test_get_main_app_undefined() ->
    %% Ensure no bootstrap application is set
    application:unset_env(nova, bootstrap_application),

    Result = nova:get_main_app(),

    ?assertEqual(undefined, Result).

test_get_apps() ->
    %% Mock nova_router:compiled_apps/0
    meck:new(nova_router, [passthrough]),
    meck:expect(nova_router, compiled_apps, fun() ->
                                                     [{app1, "/prefix1"}, {app2, "/prefix2"}]
                                             end),

    Result = nova:get_apps(),

    ?assertEqual([{app1, "/prefix1"}, {app2, "/prefix2"}], Result),

    meck:unload(nova_router).

test_get_environment_default() ->
    %% Ensure no environment is set
    application:unset_env(nova, environment),

    Result = nova:get_environment(),

    %% Should default to dev
    ?assertEqual(dev, Result).

test_get_environment_custom() ->
    %% Set custom environment
    application:set_env(nova, environment, production),

    Result = nova:get_environment(),

    ?assertEqual(production, Result),

    %% Cleanup
    application:unset_env(nova, environment).

test_get_env_with_app() ->
    %% Set bootstrap application
    application:set_env(nova, bootstrap_application, test_app),
    application:set_env(test_app, test_key, test_value),

    Result = nova:get_env(test_key, default_value),

    ?assertEqual(test_value, Result),

    %% Cleanup
    application:unset_env(nova, bootstrap_application),
    application:unset_env(test_app, test_key).

test_get_env_no_app() ->
    %% Ensure no bootstrap application
    application:unset_env(nova, bootstrap_application),

    Result = nova:get_env(test_key, default_value),

    %% Should return default
    ?assertEqual(default_value, Result).

test_set_env_with_app() ->
    %% Set bootstrap application
    application:set_env(nova, bootstrap_application, test_app),

    Result = nova:set_env(test_key, test_value),

    ?assertEqual(ok, Result),

    %% Verify the value was set
    ?assertEqual({ok, test_value}, application:get_env(test_app, test_key)),

    %% Cleanup
    application:unset_env(nova, bootstrap_application),
    application:unset_env(test_app, test_key).

test_set_env_no_app() ->
    %% Ensure no bootstrap application
    application:unset_env(nova, bootstrap_application),

    Result = nova:set_env(test_key, test_value),

    ?assertEqual({error, main_app_not_found}, Result).

test_use_stacktrace_enable() ->
    %% Enable stacktraces
    nova:use_stacktrace(true),

    %% Verify persistent_term was set
    ?assertEqual(true, persistent_term:get(nova_use_stacktrace)),

    %% Cleanup
    persistent_term:erase(nova_use_stacktrace).

test_use_stacktrace_disable() ->
    %% First enable, then disable
    persistent_term:put(nova_use_stacktrace, true),

    nova:use_stacktrace(false),

    %% Verify persistent_term was erased
    ?assertError(badarg, persistent_term:get(nova_use_stacktrace)).

test_format_stacktrace() ->
    %% Create a sample stacktrace
    Stacktrace = [
                  {module1, func1, 2, [{file, "src/module1.erl"}, {line, 42}]},
                  {module2, func2, 0, [{file, "src/module2.erl"}, {line, 99}]},
                  {module3, func3, 3, []}  % No file/line info
                 ],

    Result = nova:format_stacktrace(Stacktrace),

    ?assertEqual([
                  #{module => module1, function => func1, arity => 2,
                    file => "src/module1.erl", line => 42},
                  #{module => module2, function => func2, arity => 0,
                    file => "src/module2.erl", line => 99},
                  #{module => module3, function => func3, arity => 3,
                    file => <<"unknown">>, line => -1}
                 ], Result).
