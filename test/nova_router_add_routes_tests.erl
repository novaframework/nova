-module(nova_router_add_routes_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
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
