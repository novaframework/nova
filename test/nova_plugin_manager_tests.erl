-module(nova_plugin_manager_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    persistent_term:put(nova_plugins, []),
    {ok, Pid} = nova_plugin_manager:start_link(),
    %% Set up mock plugin module
    meck:new(test_plugin, [non_strict, no_link]),
    meck:expect(test_plugin, plugin_info, fun() ->
        #{title => <<"Test Plugin">>, version => <<"1.0.0">>}
    end),
    meck:expect(test_plugin, init, fun() -> #{initialized => true} end),
    meck:expect(test_plugin, stop, fun() -> ok end),
    %% Plugin without init
    meck:new(test_plugin_no_init, [non_strict, no_link]),
    meck:expect(test_plugin_no_init, plugin_info, fun() ->
        #{title => <<"No Init Plugin">>, version => <<"0.1.0">>}
    end),
    %% Plugin with old tuple format for plugin_info
    meck:new(test_plugin_old, [non_strict, no_link]),
    meck:expect(test_plugin_old, plugin_info, fun() ->
        {<<"Old Plugin">>, <<"2.0.0">>, <<"Author">>, <<"Desc">>, <<"URL">>}
    end),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    gen_server:stop(Pid),
    meck:unload(test_plugin),
    meck:unload(test_plugin_no_init),
    meck:unload(test_plugin_old),
    persistent_term:erase(nova_plugins),
    nova_test_helper:cleanup_nova_env(Prev).

%%====================================================================
%% add_plugin/1 with module
%%====================================================================

add_plugin_module_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin),
        timer:sleep(50),
        {ok, State} = nova_plugin_manager:get_state(test_plugin),
        ?assertEqual(#{initialized => true}, State)
    end}.

add_plugin_no_init_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin_no_init),
        timer:sleep(50),
        {ok, State} = nova_plugin_manager:get_state(test_plugin_no_init),
        ?assertEqual(#{}, State)
    end}.

add_plugin_old_format_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin_old),
        timer:sleep(50),
        ?assertMatch({ok, _}, nova_plugin_manager:get_state(test_plugin_old))
    end}.

add_plugin_duplicate_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin),
        timer:sleep(50),
        %% Adding again should return ok (already initialized)
        ok = nova_plugin_manager:add_plugin(test_plugin)
    end}.

%%====================================================================
%% add_plugin/1 with tuple
%%====================================================================

add_plugin_tuple_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin({pre_request, test_plugin, #{}}),
        timer:sleep(50),
        ?assertMatch({ok, _}, nova_plugin_manager:get_state(test_plugin))
    end}.

%%====================================================================
%% add_plugin/1 with function
%%====================================================================

add_plugin_function_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(fun test_plugin:plugin_info/0),
        timer:sleep(50),
        ?assertMatch({ok, _}, nova_plugin_manager:get_state(test_plugin))
    end}.

%%====================================================================
%% get_state/1
%%====================================================================

get_state_not_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual({error, not_found}, nova_plugin_manager:get_state(nonexistent_plugin))
    end}.

get_state_by_function_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin),
        timer:sleep(50),
        {ok, State} = nova_plugin_manager:get_state(fun test_plugin:plugin_info/0),
        ?assertEqual(#{initialized => true}, State)
    end}.

%%====================================================================
%% set_state/2
%%====================================================================

set_state_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin),
        timer:sleep(50),
        ok = nova_plugin_manager:set_state(test_plugin, #{new_state => true}),
        {ok, State} = nova_plugin_manager:get_state(test_plugin),
        ?assertEqual(#{new_state => true}, State)
    end}.

set_state_not_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual({error, not_found}, nova_plugin_manager:set_state(nonexistent_plugin, #{}))
    end}.

set_state_by_function_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin),
        timer:sleep(50),
        ok = nova_plugin_manager:set_state(fun test_plugin:plugin_info/0, #{via_fun => true}),
        {ok, State} = nova_plugin_manager:get_state(test_plugin),
        ?assertEqual(#{via_fun => true}, State)
    end}.

%%====================================================================
%% add_plugin/3
%%====================================================================

add_plugin_3_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_plugin_manager:add_plugin(test_plugin, <<"Custom Name">>, <<"3.0.0">>),
        timer:sleep(50),
        ?assertMatch({ok, _}, nova_plugin_manager:get_state(test_plugin))
    end}.

%%====================================================================
%% code_change/3
%%====================================================================

code_change_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual({ok, my_state}, nova_plugin_manager:code_change("1.0", my_state, []))
    end}.
