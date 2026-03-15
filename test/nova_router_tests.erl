-module(nova_router_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    %% Build a dispatch table with test routes
    Tree0 = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_Req) -> {json, #{ok => true}} end,
    Value = #nova_handler_value{
        app = test_app,
        callback = Callback,
        secure = false,
        plugins = [{pre_request, [{fun nova_request_plugin:pre_request/4, #{}}]}],
        extra_state = #{test => true}
    },
    Tree1 = routing_tree:insert('_', "/users", <<"GET">>, Value, Tree0),
    Tree2 = routing_tree:insert('_', "/users", <<"POST">>, Value#nova_handler_value{
        callback = fun(_Req) -> {json, 201, #{}, #{created => true}} end
    }, Tree1),
    %% Route with path params
    Tree3 = routing_tree:insert('_', "/users/:id", <<"GET">>, Value, Tree2),
    %% Pathinfo/wildcard route
    Tree4 = routing_tree:insert('_', "/static/[...]", '_',
        Value#nova_handler_value{
            callback = fun nova_file_controller:get_dir/1,
            extra_state = #{static => {priv_dir, test_app, "static"}}
        }, Tree3),
    %% Cowboy handler (WebSocket)
    CowboyValue = #cowboy_handler_value{
        app = test_app,
        handler = nova_ws_handler,
        arguments = #{module => test_ws_mod},
        plugins = [{pre_request, []}],
        secure = false
    },
    Tree5 = routing_tree:insert('_', "/ws", '_', CowboyValue, Tree4),
    %% Status code route (custom 404)
    ErrorCallback = fun(_Req) -> {status, 404, #{}, <<"Custom not found">>} end,
    ErrorValue = #nova_handler_value{
        app = test_app,
        callback = ErrorCallback,
        secure = false,
        plugins = [],
        extra_state = #{}
    },
    Tree6 = routing_tree:insert('_', 404, '_', ErrorValue, Tree5),
    persistent_term:put(nova_dispatch, Tree6),
    persistent_term:put(nova_apps, [{test_app, "/"}]),
    persistent_term:put(nova_plugins, []),
    {Prev, Callback}.

cleanup({Prev, _Callback}) ->
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_apps),
    persistent_term:erase(nova_plugins),
    nova_test_helper:cleanup_nova_env(Prev).

%%====================================================================
%% method_to_binary/1
%%====================================================================

method_to_binary_get_test() ->
    ?assertEqual(<<"GET">>, nova_router:method_to_binary(get)).

method_to_binary_post_test() ->
    ?assertEqual(<<"POST">>, nova_router:method_to_binary(post)).

method_to_binary_put_test() ->
    ?assertEqual(<<"PUT">>, nova_router:method_to_binary(put)).

method_to_binary_delete_test() ->
    ?assertEqual(<<"DELETE">>, nova_router:method_to_binary(delete)).

method_to_binary_options_test() ->
    ?assertEqual(<<"OPTIONS">>, nova_router:method_to_binary(options)).

method_to_binary_head_test() ->
    ?assertEqual(<<"HEAD">>, nova_router:method_to_binary(head)).

method_to_binary_connect_test() ->
    ?assertEqual(<<"CONNECT">>, nova_router:method_to_binary(connect)).

method_to_binary_trace_test() ->
    ?assertEqual(<<"TRACE">>, nova_router:method_to_binary(trace)).

method_to_binary_patch_test() ->
    ?assertEqual(<<"PATCH">>, nova_router:method_to_binary(patch)).

method_to_binary_wildcard_test() ->
    ?assertEqual('_', nova_router:method_to_binary('_')).

method_to_binary_unknown_atom_test() ->
    ?assertEqual('_', nova_router:method_to_binary(foobar)).

%%====================================================================
%% concat_strings/2
%%====================================================================

concat_strings_two_lists_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings("/api", "/users")).

concat_strings_empty_prefix_test() ->
    ?assertEqual("/users", nova_router:concat_strings("", "/users")).

concat_strings_empty_path_test() ->
    ?assertEqual("/api", nova_router:concat_strings("/api", "")).

concat_strings_both_empty_test() ->
    ?assertEqual("", nova_router:concat_strings("", "")).

concat_strings_binary_path1_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings(<<"/api">>, "/users")).

concat_strings_binary_path2_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings("/api", <<"/users">>)).

concat_strings_both_binary_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings(<<"/api">>, <<"/users">>)).

concat_strings_integer_path2_test() ->
    ?assertEqual(404, nova_router:concat_strings("/api", 404)).

%%====================================================================
%% execute/2
%%====================================================================

execute_found_route_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/users">>),
        Env = #{},
        {ok, Req1, Env1} = nova_router:execute(Req, Env),
        ?assertEqual(test_app, maps:get(app, Env1)),
        ?assertMatch(F when is_function(F), maps:get(callback, Env1)),
        ?assertEqual(false, maps:get(secure, Env1)),
        ?assertEqual(#{}, maps:get(controller_data, Env1)),
        ?assert(is_list(maps:get(plugins, Req1))),
        ?assertEqual(#{test => true}, maps:get(extra_state, Req1)),
        ?assertEqual(#{}, maps:get(bindings, Req1))
    end}.

execute_post_route_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"POST">>, <<"/users">>),
        {ok, _Req1, Env1} = nova_router:execute(Req, #{}),
        ?assertEqual(test_app, maps:get(app, Env1)),
        ?assertMatch(F when is_function(F), maps:get(callback, Env1))
    end}.

execute_path_params_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/users/42">>),
        {ok, Req1, Env1} = nova_router:execute(Req, #{}),
        ?assertEqual(test_app, maps:get(app, Env1)),
        Bindings = maps:get(bindings, Req1),
        ?assertEqual(<<"42">>, maps:get(<<"id">>, Bindings))
    end}.

execute_not_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/nonexistent">>),
        {ok, Req1, Env1} = nova_router:execute(Req, #{}),
        %% Should route to custom 404 handler since we registered one
        ?assertEqual(404, maps:get(resp_status_code, Req1)),
        ?assertMatch(F when is_function(F), maps:get(callback, Env1))
    end}.

execute_method_not_allowed_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"DELETE">>, <<"/users">>),
        {ok, Req1, Env1} = nova_router:execute(Req, #{}),
        %% Should get 405 with allow header
        ?assertEqual(405, maps:get(resp_status_code, Req1)),
        AllowHeader = maps:get(<<"allow">>, maps:get(resp_headers, Req1), undefined),
        ?assertNotEqual(undefined, AllowHeader),
        ?assertMatch(F when is_function(F), maps:get(callback, Env1))
    end}.

execute_cowboy_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        {ok, Req1, Env1} = nova_router:execute(Req, #{}),
        ?assertEqual(test_app, maps:get(app, Env1)),
        ?assertEqual(nova_ws_handler, maps:get(cowboy_handler, Env1)),
        ?assertEqual(#{module => test_ws_mod}, maps:get(arguments, Env1)),
        ?assertEqual(false, maps:get(secure, Env1)),
        ?assert(is_list(maps:get(plugins, Req1)))
    end}.

%%====================================================================
%% lookup_url/1,2,3
%%====================================================================

lookup_url_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, _Bindings, Value} = nova_router:lookup_url('_', <<"/users">>, <<"GET">>),
        ?assertMatch(#nova_handler_value{app = test_app}, Value)
    end}.

lookup_url_with_host_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, _Bindings, Value} = nova_router:lookup_url('_', <<"/users">>, <<"POST">>),
        ?assertMatch(#nova_handler_value{app = test_app}, Value)
    end}.

lookup_url_with_method_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, _Bindings, Value} = nova_router:lookup_url('_', <<"/users">>, <<"GET">>),
        ?assertMatch(#nova_handler_value{app = test_app}, Value)
    end}.

lookup_url_not_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:lookup_url('_', <<"/nope">>, <<"GET">>),
        ?assertMatch({error, not_found}, Result)
    end}.

lookup_url_path_params_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Bindings, _Value} = nova_router:lookup_url('_', <<"/users/99">>, <<"GET">>),
        ?assertEqual(<<"99">>, maps:get(<<"id">>, Bindings))
    end}.

lookup_url_method_not_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:lookup_url('_', <<"/users">>, <<"DELETE">>),
        ?assertMatch({error, comparator_not_found, _}, Result)
    end}.

%%====================================================================
%% render_status_page/2,3,5
%%====================================================================

render_status_page_custom_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1, Env1} = nova_router:render_status_page('_', 404, #{error => <<"test">>}, Req, #{}),
        ?assertEqual(404, maps:get(resp_status_code, Req1)),
        ?assertMatch(F when is_function(F), maps:get(callback, Env1)),
        ?assertEqual(test_app, maps:get(app, Env1)),
        ControllerData = maps:get(controller_data, Env1),
        ?assertEqual(404, maps:get(status, ControllerData)),
        ?assertEqual(#{error => <<"test">>}, maps:get(data, ControllerData))
    end}.

render_status_page_fallback_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        %% 500 has no custom handler, should fall back to nova_error_controller
        {ok, Req1, Env1} = nova_router:render_status_page('_', 500, #{}, Req, #{}),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertEqual(nova, maps:get(app, Env1)),
        ?assertEqual(fun nova_error_controller:status_code/1, maps:get(callback, Env1)),
        ?assertEqual(false, maps:get(secure, Env1))
    end}.

render_status_page_2_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1, _Env1} = nova_router:render_status_page(500, Req),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

render_status_page_3_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1, Env1} = nova_router:render_status_page(404, #{reason => test}, Req),
        ?assertEqual(404, maps:get(resp_status_code, Req1)),
        ControllerData = maps:get(controller_data, Env1),
        ?assertEqual(#{reason => test}, maps:get(data, ControllerData))
    end}.

%%====================================================================
%% compiled_apps/0
%%====================================================================

compiled_apps_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Apps = nova_router:compiled_apps(),
        ?assertEqual([{test_app, "/"}], Apps)
    end}.

%%====================================================================
%% plugins/0
%%====================================================================

plugins_empty_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Plugins = nova_router:plugins(),
        ?assertEqual([], Plugins)
    end}.

%%====================================================================
%% normalize_plugins/1
%%====================================================================

normalize_plugins_empty_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual([], nova_router:normalize_plugins([]))
    end}.

normalize_plugins_single_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:normalize_plugins([{pre_request, nova_request_plugin, #{}}]),
        ?assertMatch([{pre_request, [{_, #{}}]}], Result),
        %% Verify the plugin was added to the global store
        Plugins = persistent_term:get(nova_plugins),
        ?assert(lists:member(nova_request_plugin, Plugins))
    end}.

normalize_plugins_multiple_same_type_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:normalize_plugins([
            {pre_request, nova_request_plugin, #{}},
            {pre_request, nova_cors_plugin, #{}}
        ]),
        ?assertMatch([{pre_request, [_, _]}], Result)
    end}.

normalize_plugins_different_types_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:normalize_plugins([
            {pre_request, nova_request_plugin, #{}},
            {post_request, nova_cors_plugin, #{}}
        ]),
        ?assertEqual(2, length(Result)),
        ?assertMatch({pre_request, [_]}, lists:keyfind(pre_request, 1, Result)),
        ?assertMatch({post_request, [_]}, lists:keyfind(post_request, 1, Result))
    end}.

%%====================================================================
%% add_routes/2
%%====================================================================

add_routes_empty_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual(ok, nova_router:add_routes(my_app, []))
    end}.

add_routes_invalid_throws_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertThrow({error, {invalid_routes, my_app, not_a_list}},
                     nova_router:add_routes(my_app, not_a_list))
    end}.

%% NOTE: add_routes/2 has a bug — compile_paths requires router_file in Options
%% but add_routes creates Options = #{} without it. This crashes with badkey.
%% Skipping functional test until the bug is fixed.
add_routes_crashes_without_router_file_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        NewCallback = fun(_) -> {json, #{new => true}} end,
        Routes = [#{
            routes => [{"/new_route", NewCallback, #{methods => [get]}}]
        }],
        ?assertError({badkey, router_file},
                     nova_router:add_routes(new_app, [Routes]))
    end}.

%%====================================================================
%% routes/1 callback
%%====================================================================

routes_callback_test() ->
    Routes = nova_router:routes(dev),
    ?assertMatch([#{routes := [_, _]}], Routes),
    [#{routes := RouteList}] = Routes,
    ?assertEqual(2, length(RouteList)),
    [{404, Fun404, _}, {500, Fun500, _}] = RouteList,
    ?assert(is_function(Fun404, 1)),
    ?assert(is_function(Fun500, 1)).

%%====================================================================
%% add_plugin (internal, exported via TEST)
%%====================================================================

add_plugin_new_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        nova_router:add_plugin(test_plugin),
        Plugins = persistent_term:get(nova_plugins),
        ?assert(lists:member(test_plugin, Plugins))
    end}.

add_plugin_duplicate_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        nova_router:add_plugin(test_plugin),
        nova_router:add_plugin(test_plugin),
        Plugins = persistent_term:get(nova_plugins),
        %% Should only appear once (umerge deduplicates)
        ?assertEqual(1, length([P || P <- Plugins, P =:= test_plugin]))
    end}.

%%====================================================================
%% insert/5 (internal, exported via TEST)
%%====================================================================

insert_valid_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
        Value = #nova_handler_value{app = test_app, callback = fun(_) -> ok end},
        Tree1 = nova_router:insert('_', "/test", <<"GET">>, Value, Tree),
        {ok, _, V} = routing_tree:lookup('_', <<"/test">>, <<"GET">>, Tree1),
        ?assertMatch(#nova_handler_value{app = test_app}, V)
    end}.

%%====================================================================
%% apply_callback/3 (internal, exported via TEST)
%%====================================================================

apply_callback_existing_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        %% lists:reverse/1 exists
        Result = nova_router:apply_callback(lists, reverse, [[1, 2, 3]]),
        ?assertEqual([3, 2, 1], Result)
    end}.

apply_callback_missing_function_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:apply_callback(lists, nonexistent_function_xyz, []),
        ?assertEqual([], Result)
    end}.

apply_callback_missing_module_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:apply_callback(totally_fake_module_xyz, foo, []),
        ?assertEqual([], Result)
    end}.
