-module(nova_router_add_routes_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-export([log/2]).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = nova_routing_trie:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_apps, []),
    persistent_term:put(nova_plugins, []),
    Prev.

cleanup(Prev) ->
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_apps),
    persistent_term:erase(nova_plugins),
    nova_test_helper:cleanup_nova_env(Prev).

%% Regression: add_routes/2 crashed with {badkey, router_file}
%% because Options did not include router_file
add_routes_no_crash_test_() ->
    {setup, fun() -> setup() end, fun(S) -> cleanup(S) end, fun() ->
        Callback = fun(_) -> {json, #{ok => true}} end,
        Routes = [#{routes => [{"/test", Callback, #{methods => [get]}}]}],
        nova_router:add_routes(test_app, [Routes]),
        {ok, _, Value} = nova_router:lookup_url('_', <<"/test">>, <<"GET">>),
        ?assertMatch(#nova_handler_value{app = test_app}, Value)
    end}.

add_routes_registers_app_test_() ->
    {setup, fun() -> setup() end, fun(S) -> cleanup(S) end, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        Routes = [#{routes => [{"/x", Callback, #{methods => [get]}}]}],
        nova_router:add_routes(my_app, [Routes]),
        Apps = nova_router:compiled_apps(),
        ?assertMatch({my_app, _}, lists:keyfind(my_app, 1, Apps))
    end}.

%% The routing guide for 0.17.0 to 0.18.1 put plugin_strategy on the route
%% entry, where it is ignored; that is warned about once per application.
route_entry_plugin_strategy_is_warned_about_once_test_() ->
    {setup, fun() -> setup() end, fun(S) -> cleanup(S) end, fun() ->
        persistent_term:erase({nova_router, route_plugin_strategy_warned, strategy_app}),
        Callback = fun(_) -> {json, #{}} end,
        Routes = [#{plugin_strategy => global_first, routes => [{"/a", Callback, #{methods => [get]}}]},
                  #{plugin_strategy => local_only, routes => [{"/b", Callback, #{methods => [get]}}]}],
        ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
        try
            nova_router:add_routes(strategy_app, Routes),
            nova_router:add_routes(strategy_app, Routes),
            ?assertMatch([#{app := strategy_app, plugin_strategy := global_first}], logged()),
            nova_router:add_routes(other_app, [#{routes => [{"/c", Callback, #{methods => [get]}}]}]),
            ?assertEqual([], logged())
        after
            logger:remove_handler(?MODULE),
            persistent_term:erase({nova_router, route_plugin_strategy_warned, strategy_app})
        end
    end}.

log(#{msg := {report, #{msg := <<"plugin_strategy on a route entry", _/binary>>} = Report}},
    #{config := #{test_pid := Pid}}) ->
    Pid ! {?MODULE, Report},
    ok;
log(_Event, _Config) ->
    ok.

logged() ->
    receive
        {?MODULE, Report} -> [Report | logged()]
    after 50 ->
            []
    end.
