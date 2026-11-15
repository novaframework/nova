-module(nova_router_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-define(SETUP, fun setup/0).
-define(CLEANUP, fun cleanup/1).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    %% Build a dispatch table with test routes
    Tree0 = nova_routing_trie:new(#{strict => false}),
    Callback = fun(_Req) -> {json, #{ok => true}} end,
    Value = #nova_handler_value{
        app = test_app,
        callback = Callback,
        secure = false,
        plugins = [{pre_request, [{fun nova_request_plugin:pre_request/4, #{}}]}],
        extra_state = #{test => true}
    },
    {ok, Tree1} = nova_routing_trie:insert('_', "/users", <<"GET">>, Value, Tree0),
    {ok, Tree2} = nova_routing_trie:insert('_', "/users", <<"POST">>, Value#nova_handler_value{
        callback = fun(_Req) -> {json, 201, #{}, #{created => true}} end
    }, Tree1),
    %% Route with path params
    {ok, Tree3} = nova_routing_trie:insert('_', "/users/:id", <<"GET">>, Value, Tree2),
    %% Pathinfo/wildcard route
    {ok, Tree4} = nova_routing_trie:insert('_', "/static/[...]", '_',
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
    {ok, Tree5} = nova_routing_trie:insert('_', "/ws", '_', CowboyValue, Tree4),
    %% Status code route (custom 404)
    ErrorCallback = fun(_Req) -> {status, 404, #{}, <<"Custom not found">>} end,
    ErrorValue = #nova_handler_value{
        app = test_app,
        callback = ErrorCallback,
        secure = false,
        plugins = [],
        extra_state = #{}
    },
    {ok, Tree6} = nova_routing_trie:insert('_', 404, '_', ErrorValue, Tree5),
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
    ?assertThrow({error, {invalid_method, foobar}}, nova_router:method_to_binary(foobar)).

method_to_binary_accepts_every_spelling_test_() ->
    [?_assertEqual(<<"POST">>, nova_router:method_to_binary(Method))
     || Method <- ['POST', 'Post', <<"POST">>, "POST"]].

method_to_binary_keeps_the_case_of_a_binary_or_string_test_() ->
    [?_assertEqual(Expected, nova_router:method_to_binary(Method))
     || {Method, Expected} <- [{~"post", ~"post"}, {"post", ~"post"}, {~"Purge", ~"Purge"}, {"pots", ~"pots"}]].

method_to_binary_accepts_any_token_test_() ->
    [?_assertEqual(iolist_to_binary(Method), nova_router:method_to_binary(Method))
     || Method <- [~"PURGE", "PROPFIND", ~"QUERY", "M-SEARCH", ~"!#$%&'*+-.^_`|~09azAZ"]].

method_to_binary_rejects_what_is_not_a_method_test_() ->
    [?_assertThrow({error, {invalid_method, Method}}, nova_router:method_to_binary(Method))
     || Method <- [gett, update, purge, 'PROPFIND', <<"P O">>, <<>>, "", "GET,POST", ~"GET\r\n", ~"G/T",
                   <<"G", 233, "T">>, [$G, 233, $T], [$G, 16#D800], 1, {get}, [get]]].

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
        AllowHeader = maps:get(<<"allow">>, maps:get(resp_headers, Req1)),
        ?assertEqual([~"GET", ~"POST"], lists:sort(binary:split(iolist_to_binary(AllowHeader), ~", ", [global]))),
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

execute_keeps_the_dispatch_key_on_the_request_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Key = {nova_dispatch, test_app, 8081},
        persistent_term:put(Key, persistent_term:get(nova_dispatch)),
        try
            [begin
                 {ok, Req1, _Env1} = nova_router:execute(nova_test_helper:mock_req(Method, Path),
                                                         #{nova_dispatch_key => Key}),
                 ?assertEqual(Key, nova_router:dispatch_key(Req1), {Method, Path})
             end || {Method, Path} <- [{~"GET", ~"/users"}, {~"GET", ~"/static/a/b"}, {~"GET", ~"/ws"},
                                       {~"GET", ~"/nonexistent"}, {~"DELETE", ~"/users"}]]
        after
            persistent_term:erase(Key)
        end
    end}.

dispatch_key_of_a_request_outside_the_router_test() ->
    ?assertEqual(nova_dispatch, nova_router:dispatch_key(nova_test_helper:mock_req(~"GET", ~"/users"))).

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

%% A controller is called with the request and nothing else, so what a
%% caller hands to the status page has to be in it.
render_status_page_data_reaches_the_controller_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(~"GET", ~"/"),
        {ok, Custom, _} = nova_router:render_status_page('_', 404, #{error => ~"test"}, Req, #{}),
        ?assertEqual(#{status => 404, data => #{error => ~"test"}}, maps:get(controller_data, Custom)),
        {ok, Fallback, _} = nova_router:render_status_page(500, #{reason => test}, Req),
        ?assertEqual(#{status => 500, data => #{reason => test}}, maps:get(controller_data, Fallback))
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
%% The router module of an application
%%====================================================================

router_module_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     [{"an application without a router module stops the compile, and no atom is made for the name",
       fun() ->
               ?assertThrow({error, {router_module_not_found, nova_router_tests_no_such_app}},
                            nova_router:compile([nova_router_tests_no_such_app])),
               ?assertError(badarg, binary_to_existing_atom(<<"nova_router_tests_no_such_app", "_router">>))
       end},
      {"the router_module of the application environment is used as it is",
       fun() ->
               application:set_env(nova_router_tests_named_app, router_module, nova_test_mounted_router),
               try
                   nova_test_mounted_router:set_routes([]),
                   _Dispatch = nova_router:compile([nova_router_tests_named_app]),
                   ?assert(lists:keymember(nova_router_tests_named_app, 1, nova_router:compiled_apps()))
               after
                   application:unset_env(nova_router_tests_named_app, router_module)
               end
       end}]}.

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
%% normalize_plugins/2
%%====================================================================

normalize_plugins_empty_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual([], nova_router:normalize_plugins([], order([], [])))
    end}.

%% pre_request/4 waits for the security callback, and pre_security/4 of a
%% plugin that has it runs ahead of it.
normalize_plugins_single_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:normalize_plugins([{pre_request, nova_request_plugin, #{}}], order([], [])),
        ?assertEqual([{after_security, [{fun nova_request_plugin:pre_request/4, #{}}]},
                      {pre_request, [{fun nova_request_plugin:pre_security/4, #{}}]}], Result),
        %% Verify the plugin was added to the global store
        Plugins = persistent_term:get(nova_plugins),
        ?assert(lists:member(nova_request_plugin, Plugins))
    end}.

normalize_plugins_keep_their_order_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:normalize_plugins([
            {pre_request, nova_request_plugin, #{}},
            {pre_request, nova_cors_plugin, #{}},
            {pre_request, nova_csrf_plugin, #{}}
        ], order([], [])),
        ?assertEqual([{after_security, [{fun nova_request_plugin:pre_request/4, #{}},
                                        {fun nova_csrf_plugin:pre_request/4, #{}}]},
                      {pre_request, [{fun nova_request_plugin:pre_security/4, #{}},
                                     {fun nova_cors_plugin:pre_security/4, #{}},
                                     {fun nova_csrf_plugin:pre_security/4, #{}}]}], Result)
    end}.

normalize_plugins_different_types_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Result = nova_router:normalize_plugins([
            {pre_request, nova_csrf_plugin, #{}},
            {post_request, nova_cors_plugin, #{}}
        ], order([], [])),
        ?assertEqual([{after_security, [{fun nova_csrf_plugin:pre_request/4, #{}}]},
                      {post_request, [{fun nova_cors_plugin:post_request/4, #{}}]},
                      {pre_request, [{fun nova_csrf_plugin:pre_security/4, #{}}]}], Result)
    end}.

normalize_plugins_before_security_test_() ->
    Group = #{app => an_app, prefix => "/admin", router_file => "router.erl"},
    Plugins = [{pre_request, nova_request_plugin, #{}}, {pre_request, nova_cors_plugin, #{}},
               {post_request, nova_csrf_plugin, #{}}],
    [?_assertEqual([], nova_router:plugins_before_security(#{}, Plugins, Group)),
     ?_assertEqual(all, nova_router:plugins_before_security(#{plugins_before_security => all}, Plugins, Group)),
     ?_assertEqual(all, nova_router:plugins_before_security(#{plugins_before_security => all}, [], Group)),
     ?_assertEqual([], nova_router:plugins_before_security(#{plugins_before_security => []}, [], Group)),
     ?_assertEqual([nova_request_plugin],
                   nova_router:plugins_before_security(#{plugins_before_security => [nova_request_plugin]},
                                                       Plugins, Group)),
     [{lists:flatten(io_lib:format("~0p", [Value])),
       ?_assertThrow({error, {invalid_setting, plugins_before_security, Value}},
                     nova_router:plugins_before_security(#{plugins_before_security => Value}, Plugins, Group))}
      || Value <- [none, [~"nova_request_plugin"], [nova_request_plugin | another], [all],
                   [nova_reqeust_plugin], [nova_request_plugin, nova_reqeust_plugin], [nova_cors_plugin],
                   [nova_csrf_plugin], [nova_multipart_plugin]]]].

%% The one of the nova environment is for every route group, so it may name
%% a plugin this one does not have.
plugins_before_security_of_the_environment_test() ->
    ok = nova_test_settings:set(plugins_before_security, [nova_multipart_plugin]),
    try
        ?assertEqual([nova_multipart_plugin],
                     nova_router:plugins_before_security(#{}, [{pre_request, nova_request_plugin, #{}}],
                                                         #{app => an_app, prefix => "/", router_file => "router.erl"}))
    after
        nova_test_settings:unset(plugins_before_security)
    end.

plugins_before_security_that_is_refused_names_the_route_group_test() ->
    {_, Events} = nova_test_log:collect(
                    fun() ->
                            ?assertThrow({error, _},
                                         nova_router:plugins_before_security(
                                           #{plugins_before_security => [nova_reqeust_plugin]},
                                           [{pre_request, nova_request_plugin, #{}}],
                                           #{app => an_app, prefix => "/admin", router_file => "router.erl"}))
                    end),
    ?assertMatch([#{setting := plugins_before_security, value := [nova_reqeust_plugin], app := an_app,
                    prefix := "/admin", router_file := "router.erl"}],
                 nova_test_log:errors(Events)).

order(Before, After) ->
    #{before => Before, 'after' => After, secured => false}.


%%====================================================================
%% Route template
%%====================================================================

route_template_controller_and_ws_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        ok = nova_router:add_routes(route_app,
                                    #{prefix => "/api",
                                      plugins => [{pre_request, nova_request_plugin, #{}}],
                                      routes => [{"/users/:id", Callback, #{methods => [get]}},
                                                 {"/socket", route_ws_mod, #{protocol => ws}}]}),

        {ok, Req, _Env} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/api/users/7"), #{}),
        ?assertEqual(~"/api/users/:id", maps:get(nova_route, Req)),
        ?assertEqual([{after_security, [{fun nova_request_plugin:pre_request/4, #{}}]},
                      {pre_request, [{fun nova_request_plugin:pre_security/4, #{}}]}],
                     maps:get(plugins, Req)),

        {ok, WsReq, WsEnv} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/api/socket"), #{}),
        ?assertEqual(~"/api/socket", maps:get(nova_route, WsReq)),
        ?assertEqual([{after_security, [{fun nova_request_plugin:pre_request/4, #{}}]},
                      {pre_request, [{fun nova_request_plugin:pre_security/4, #{}}]}],
                     maps:get(plugins, WsReq)),
        ?assertEqual(#{module => route_ws_mod, app => route_app}, maps:get(arguments, WsEnv))
    end}.

route_template_static_dir_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = nova_router:add_routes(nova, #{prefix => "/p", plugins => [],
                                            routes => [{"/files/[...]", "static"}]}),
        {ok, Req, Env} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/p/files/a/b.txt"), #{}),
        ?assertEqual(~"/p/files/[...]", maps:get(nova_route, Req)),
        ?assertEqual([], maps:get(plugins, Req)),
        ?assertMatch(#{static := {priv_dir, nova, "static"}, pathinfo := [_ | _]}, maps:get(extra_state, Req)),
        ?assertEqual(fun nova_file_controller:get_dir/1, maps:get(callback, Env))
    end}.

route_template_absent_without_template_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Req, _} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/users"), #{}),
        ?assertNot(maps:is_key(nova_route, Req)),
        ?assertEqual([{pre_request, [{fun nova_request_plugin:pre_request/4, #{}}]}], maps:get(plugins, Req)),
        {ok, StaticReq, _} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/static/x"), #{}),
        ?assertNot(maps:is_key(nova_route, StaticReq))
    end}.

route_template_absent_on_404_and_405_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, NotFound, _} = nova_router:execute(nova_test_helper:mock_req(~"GET", ~"/missing"), #{}),
        ?assertNot(maps:is_key(nova_route, NotFound)),
        {ok, NotAllowed, _} = nova_router:execute(nova_test_helper:mock_req(~"DELETE", ~"/users"), #{}),
        ?assertEqual(405, maps:get(resp_status_code, NotAllowed)),
        ?assertNot(maps:is_key(nova_route, NotAllowed))
    end}.

route_template_record_arity_test() ->
    ?assertEqual(8, tuple_size(#nova_handler_value{})),
    ?assertEqual(6, tuple_size(#cowboy_handler_value{})).
