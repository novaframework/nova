-module(nova_sup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

%%====================================================================
%% resolve_nova_apps/1
%%====================================================================

%% Regression: the base clause reversed an accumulator the caller then kept
%% accumulating into, so even a flat list came back out of order - [a, b, c]
%% resolved to [c, a, b]. Any nova_apps list of two or more was affected, not
%% only nested ones.
resolve_flat_list_test() ->
    ?assertEqual([a, b, c], nova_sup:resolve_nova_apps([a, b, c])).

resolve_pair_test() ->
    ?assertEqual([a, b], nova_sup:resolve_nova_apps([a, b])).

resolve_empty_test() ->
    ?assertEqual([], nova_sup:resolve_nova_apps([])).

resolve_nested_keeps_order_test() ->
    with_env([{parent, nova_apps, [child]}], fun() ->
        ?assertEqual([child, parent, nova], nova_sup:resolve_nova_apps([parent, nova]))
    end).

resolve_nested_depth_first_test() ->
    with_env([{parent, nova_apps, [child]},
              {child, nova_apps, [grandchild]}], fun() ->
        ?assertEqual([grandchild, child, parent], nova_sup:resolve_nova_apps([parent]))
    end).

resolve_several_parents_test() ->
    with_env([{parent_a, nova_apps, [child_a]},
              {parent_b, nova_apps, [child_b]}], fun() ->
        ?assertEqual([child_a, parent_a, child_b, parent_b, nova],
                     nova_sup:resolve_nova_apps([parent_a, parent_b, nova]))
    end).

resolve_deduplicates_test() ->
    with_env([{parent_a, nova_apps, [shared]},
              {parent_b, nova_apps, [shared]}], fun() ->
        ?assertEqual([shared, parent_a, parent_b],
                     nova_sup:resolve_nova_apps([parent_a, parent_b]))
    end).

resolve_terminates_on_a_cycle_test() ->
    with_env([{a, nova_apps, [b]},
              {b, nova_apps, [a]}], fun() ->
        ?assertEqual([b, a], nova_sup:resolve_nova_apps([a]))
    end).

%% Regression: a sub-application given as {Name, Options} was looked up with
%% the whole tuple as the application name, so its own nova_apps were never
%% resolved.
resolve_nested_under_tuple_form_test() ->
    with_env([{parent, nova_apps, [child]}], fun() ->
        ?assertEqual([child, {parent, #{prefix => "/p"}}],
                     nova_sup:resolve_nova_apps([{parent, #{prefix => "/p"}}]))
    end).

resolve_tuple_form_is_deduplicated_by_name_test() ->
    ?assertEqual([{parent, #{}}],
                 nova_sup:resolve_nova_apps([{parent, #{}}, parent])).

%%====================================================================
%% compile_order/1
%%====================================================================

%% Regression: nova was compiled first, and because routes are first-wins its
%% default 404 always won, so an application's own status-code route was
%% silently ignored.
nova_is_compiled_last_test() ->
    with_env([{my_app, nova_apps, []}], fun() ->
        ?assertEqual([my_app, nova], nova_sup:compile_order(my_app))
    end).

nova_is_compiled_last_with_nested_apps_test() ->
    with_env([{my_app, nova_apps, [child]}], fun() ->
        ?assertEqual([child, my_app, nova], nova_sup:compile_order(my_app))
    end).

%%====================================================================
%% Status-code route precedence
%%
%% compile_order/1 only matters because of how the routing table resolves a
%% duplicate, so pin that too: whoever is compiled first keeps the route.
%%====================================================================

application_status_route_beats_nova_default_test_() ->
    {setup, fun setup_compile/0, fun cleanup_compile/1, fun() ->
        nova_router:compile([status_app, nova]),
        {ok, _Bindings, Value} = nova_router:lookup_url('_', 404, '_'),
        ?assertEqual(status_app, Value#nova_handler_value.app)
    end}.

nova_default_status_route_is_used_when_the_application_has_none_test_() ->
    {setup, fun setup_compile/0, fun cleanup_compile/1, fun() ->
        nova_router:compile([plain_app, nova]),
        {ok, _Bindings, Value} = nova_router:lookup_url('_', 404, '_'),
        ?assertEqual(nova, Value#nova_handler_value.app)
    end}.

%% The order the old code used, kept as an explicit statement of what the
%% regression looked like from the outside.
nova_first_hides_the_applications_status_route_test_() ->
    {setup, fun setup_compile/0, fun cleanup_compile/1, fun() ->
        nova_router:compile([nova, status_app]),
        {ok, _Bindings, Value} = nova_router:lookup_url('_', 404, '_'),
        ?assertEqual(nova, Value#nova_handler_value.app)
    end}.

setup_compile() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    persistent_term:put(nova_dispatch, routing_tree:new(#{use_strict => false, convert_to_binary => true})),
    persistent_term:put(nova_apps, []),
    persistent_term:put(nova_plugins, []),

    meck:new(status_app_router, [non_strict]),
    meck:expect(status_app_router, routes,
                fun(_Env) ->
                        [#{routes => [{404, fun(_Req) -> {status, 404} end, #{}}]}]
                end),

    meck:new(plain_app_router, [non_strict]),
    meck:expect(plain_app_router, routes,
                fun(_Env) ->
                        [#{routes => [{"/plain", fun(_Req) -> {status, 200} end, #{methods => [get]}}]}]
                end),
    Prev.

cleanup_compile(Prev) ->
    meck:unload(status_app_router),
    meck:unload(plain_app_router),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_apps),
    persistent_term:erase(nova_plugins),
    nova_test_helper:cleanup_nova_env(Prev).

%%====================================================================
%% Helpers
%%====================================================================

%% Set application environment keys for the duration of Fun, restoring
%% whatever was there before.
with_env(Vars, Fun) ->
    Saved = [{App, Key, application:get_env(App, Key)} || {App, Key, _Value} <- Vars],
    [application:set_env(App, Key, Value) || {App, Key, Value} <- Vars],
    try
        Fun()
    after
        [restore(App, Key, Previous) || {App, Key, Previous} <- Saved]
    end.

restore(App, Key, undefined)   -> application:unset_env(App, Key);
restore(App, Key, {ok, Value}) -> application:set_env(App, Key, Value).
