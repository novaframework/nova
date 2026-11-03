-module(nova_router_add_routes_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

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

%% guides/routing.md sets plugin_strategy on the route entry; it used to be
%% read only from the per-application options.
plugin_strategy_on_route_entry_test_() ->
    {setup,
     fun() ->
             Prev = setup(),
             application:set_env(nova, plugins, [{pre_request, nova_correlation_plugin, #{}}]),
             Prev
     end,
     fun(S) -> application:unset_env(nova, plugins), cleanup(S) end,
     fun() ->
             Callback = fun(_) -> {json, #{}} end,
             Routes = [#{plugin_strategy => global_first,
                         plugins => [{pre_request, nova_request_plugin, #{}}],
                         routes => [{"/both", Callback, #{methods => [get]}}]},
                       #{plugins => [{pre_request, nova_request_plugin, #{}}],
                         routes => [{"/local", Callback, #{methods => [get]}}]}],
             nova_router:add_routes(strategy_app, Routes),
             ?assertEqual([nova_correlation_plugin, nova_request_plugin], pre_request_modules(~"/both")),
             ?assertEqual([nova_request_plugin], pre_request_modules(~"/local"))
     end}.

pre_request_modules(Path) ->
    {ok, _, #nova_handler_value{plugins = Plugins}} = nova_router:lookup_url('_', Path, ~"GET"),
    case proplists:get_value(pre_request, Plugins, []) of
        PreRequest when is_list(PreRequest) ->
            [element(2, erlang:fun_info(Fun, module)) || {Fun, _Opts} <- PreRequest, is_function(Fun)]
    end.
