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

route_template_test_() ->
    {setup, fun() -> setup() end, fun(S) -> cleanup(S) end, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        nova_router:add_routes(route_app,
                               #{prefix => "/api",
                                 plugins => [{pre_request, nova_request_plugin, #{}}],
                                 routes => [{"/users/:id", Callback, #{methods => [get]}},
                                            {"/socket", route_ws_mod, #{protocol => ws}},
                                            {404, Callback, #{}}]}),

        {ok, Req, _Env} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/api/users/7"), #{}),
        ?assertEqual(~"/api/users/:id", maps:get(nova_route, Req)),
        ?assertEqual([{pre_request, [{fun nova_request_plugin:pre_request/4, #{}}]}],
                     maps:get(plugins, Req)),

        {ok, WsReq, WsEnv} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/api/socket"), #{}),
        ?assertEqual(~"/api/socket", maps:get(nova_route, WsReq)),
        ?assertEqual(nova_ws_handler, maps:get(cowboy_handler, WsEnv)),
        ?assertEqual(false, lists:keyfind(nova_route, 1, maps:get(plugins, WsReq)))
    end}.

route_template_absent_on_404_and_405_test_() ->
    {setup, fun() -> setup() end, fun(S) -> cleanup(S) end, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        nova_router:add_routes(route_app, #{routes => [{"/only-get", Callback, #{methods => [get]}}]}),
        {ok, NotFound, _} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/missing"), #{}),
        ?assertNot(maps:is_key(nova_route, NotFound)),
        {ok, NotAllowed, _} = nova_router:execute(nova_test_helper:mock_req(~"POST", ~"/only-get"), #{}),
        ?assertEqual(405, maps:get(resp_status_code, NotAllowed)),
        ?assertNot(maps:is_key(nova_route, NotAllowed))
    end}.

route_template_record_arity_test() ->
    ?assertEqual(8, tuple_size(#nova_handler_value{})),
    ?assertEqual(6, tuple_size(#cowboy_handler_value{})).
