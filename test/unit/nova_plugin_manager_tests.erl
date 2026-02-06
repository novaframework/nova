-module(nova_plugin_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

plugin_manager_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Add plugin with module", fun test_add_plugin_module/0},
      {"Add plugin with callback function", fun test_add_plugin_callback/0},
      {"Get state from existing plugin", fun test_get_state_existing/0},
      {"Get state from non-existent plugin", fun test_get_state_nonexistent/0},
      {"Set state for plugin", fun test_set_state/0},
      {"Set state for non-existent plugin", fun test_set_state_nonexistent/0},
      {"Duplicate plugin registration", fun test_duplicate_plugin/0},
      {"Get state with callback function", fun test_get_state_with_callback/0},
      {"Set state with callback function", fun test_set_state_with_callback/0}
     ]}.

setup() ->
    %% Mock nova_router:plugins() to return empty list
    meck:new(nova_router, [passthrough]),
    meck:expect(nova_router, plugins, fun() -> [] end),

    %% Start the plugin manager
    {ok, Pid} = nova_plugin_manager:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the plugin manager
    exit(Pid, normal),
    timer:sleep(50),

    %% Unload mocks
    catch meck:unload(nova_router),

    ok.

%%------------------------------------------------------------------------------
%% Helper Module for Testing
%%------------------------------------------------------------------------------

%% Create a simple test plugin module behavior
-record(plugin, {
                 module :: atom(),
                 name :: binary(),
                 version :: binary(),
                 state :: any()
                }).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_add_plugin_module() ->
    %% Mock a plugin module with plugin_info/0
    meck:new(test_plugin_module, [non_strict]),
    meck:expect(test_plugin_module, plugin_info, fun() ->
                                                          #{title => <<"Test Plugin">>,
                                                            version => <<"1.0.0">>}
                                                  end),

    %% Add the plugin
    ok = nova_plugin_manager:add_plugin(test_plugin_module),

    %% Wait for the cast to be processed
    timer:sleep(10),

    %% Verify the plugin state can be retrieved (should be undefined initially)
    Result = nova_plugin_manager:get_state(test_plugin_module),

    ?assertMatch({ok, _}, Result),

    meck:unload(test_plugin_module).

test_add_plugin_callback() ->
    %% Create a callback function from a mocked module
    meck:new(callback_plugin_module, [non_strict]),
    meck:expect(callback_plugin_module, plugin_info, fun() ->
                                                             #{title => <<"Callback Plugin">>,
                                                               version => <<"2.0.0">>}
                                                     end),

    Callback = fun callback_plugin_module:pre_request/4,

    %% Add the plugin via callback
    ok = nova_plugin_manager:add_plugin(Callback),

    timer:sleep(10),

    %% Verify the plugin was added
    Result = nova_plugin_manager:get_state(callback_plugin_module),

    ?assertMatch({ok, _}, Result),

    meck:unload(callback_plugin_module).

test_get_state_existing() ->
    %% Add a plugin first
    meck:new(state_test_plugin, [non_strict]),
    meck:expect(state_test_plugin, plugin_info, fun() ->
                                                        #{title => <<"State Test">>,
                                                          version => <<"1.0.0">>}
                                                end),

    ok = nova_plugin_manager:add_plugin(state_test_plugin),
    timer:sleep(10),

    %% Get the state
    {ok, State} = nova_plugin_manager:get_state(state_test_plugin),

    %% Initially should be undefined or empty map
    ?assert(State =:= undefined orelse is_map(State)),

    meck:unload(state_test_plugin).

test_get_state_nonexistent() ->
    %% Try to get state of a plugin that doesn't exist
    Result = nova_plugin_manager:get_state(nonexistent_plugin),

    ?assertEqual({error, not_found}, Result).

test_set_state() ->
    %% Add a plugin first
    meck:new(set_state_plugin, [non_strict]),
    meck:expect(set_state_plugin, plugin_info, fun() ->
                                                       #{title => <<"Set State Test">>,
                                                         version => <<"1.0.0">>}
                                               end),

    ok = nova_plugin_manager:add_plugin(set_state_plugin),
    timer:sleep(10),

    %% Set a new state
    NewState = #{counter => 42, data => <<"test">>},
    ok = nova_plugin_manager:set_state(set_state_plugin, NewState),

    %% Retrieve the state
    {ok, RetrievedState} = nova_plugin_manager:get_state(set_state_plugin),

    ?assertEqual(NewState, RetrievedState),

    meck:unload(set_state_plugin).

test_set_state_nonexistent() ->
    %% Try to set state for a plugin that doesn't exist
    Result = nova_plugin_manager:set_state(nonexistent_plugin, #{}),

    ?assertEqual({error, not_found}, Result).

test_duplicate_plugin() ->
    %% Add a plugin
    meck:new(dup_plugin, [non_strict]),
    meck:expect(dup_plugin, plugin_info, fun() ->
                                                 #{title => <<"Duplicate Plugin">>,
                                                   version => <<"1.0.0">>}
                                         end),

    ok = nova_plugin_manager:add_plugin(dup_plugin),
    timer:sleep(10),

    %% Try to add the same plugin again
    ok = nova_plugin_manager:add_plugin(dup_plugin),
    timer:sleep(10),

    %% Should still work without errors (duplicate is logged but ignored)
    ?assertMatch({ok, _}, nova_plugin_manager:get_state(dup_plugin)),

    meck:unload(dup_plugin).

test_get_state_with_callback() ->
    %% Add a plugin
    meck:new(callback_get_plugin, [non_strict]),
    meck:expect(callback_get_plugin, plugin_info, fun() ->
                                                          #{title => <<"Callback Get">>,
                                                            version => <<"1.0.0">>}
                                                  end),

    ok = nova_plugin_manager:add_plugin(callback_get_plugin),
    timer:sleep(10),

    %% Set some state
    TestState = #{value => 123},
    ok = nova_plugin_manager:set_state(callback_get_plugin, TestState),

    %% Get state using a callback function
    Callback = fun callback_get_plugin:pre_request/4,
    {ok, RetrievedState} = nova_plugin_manager:get_state(Callback),

    ?assertEqual(TestState, RetrievedState),

    meck:unload(callback_get_plugin).

test_set_state_with_callback() ->
    %% Add a plugin
    meck:new(callback_set_plugin, [non_strict]),
    meck:expect(callback_set_plugin, plugin_info, fun() ->
                                                          #{title => <<"Callback Set">>,
                                                            version => <<"1.0.0">>}
                                                  end),

    ok = nova_plugin_manager:add_plugin(callback_set_plugin),
    timer:sleep(10),

    %% Set state using a callback function
    Callback = fun callback_set_plugin:post_request/4,
    NewState = #{updated => true},
    ok = nova_plugin_manager:set_state(Callback, NewState),

    %% Verify the state was set
    {ok, RetrievedState} = nova_plugin_manager:get_state(callback_set_plugin),

    ?assertEqual(NewState, RetrievedState),

    meck:unload(callback_set_plugin).
