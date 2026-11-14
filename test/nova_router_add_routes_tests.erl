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

missing_static_path_is_named_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Routes = #{routes => [{"/assets", "no/such/directory"}]},
        ?assertThrow({error, {static_path_not_found, "no/such/directory"}},
                     nova_router:add_routes(nova, [Routes]))
    end}.

missing_static_path_is_logged_as_an_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        ok = nova_test_log:start(),
        ?assertThrow({error, {static_path_not_found, "no/such/directory"}},
                     nova_router:add_routes(nova, [#{routes => [{"/assets", "no/such/directory"}]}])),
        ?assertMatch([error], [Level || #{level := Level, msg := {report, #{local_path := "no/such/directory"}}}
                                            <- nova_test_log:stop()])
    end}.

static_directory_is_resolved_when_the_route_is_compiled_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Directory = code:lib_dir(nova),
        File = code:which(nova_router),
        nova_router:add_routes(nova, [#{routes => [{"/files/[...]", Directory}, {"/file", File}]}]),
        {ok, _, #nova_handler_value{extra_state = DirectoryState}} = nova_router:lookup_url('_', ~"/files", ~"GET"),
        ?assertMatch(#{static := {priv_dir, nova, Directory}, static_root := {Directory, {[_ | _] = Root, Root}}},
                     DirectoryState),
        {ok, _, #nova_handler_value{extra_state = FileState}} = nova_router:lookup_url('_', ~"/file", ~"GET"),
        ?assertEqual(#{static => {priv_file, nova, File}, options => #{}}, FileState)
    end}.

add_routes_registers_app_test_() ->
    {setup, fun() -> setup() end, fun(S) -> cleanup(S) end, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        Routes = [#{routes => [{"/x", Callback, #{methods => [get]}}]}],
        nova_router:add_routes(my_app, [Routes]),
        Apps = nova_router:compiled_apps(),
        ?assertMatch({my_app, _}, lists:keyfind(my_app, 1, Apps))
    end}.
