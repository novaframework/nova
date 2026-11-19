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
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Callback = fun(_) -> {json, #{ok => true}} end,
        Routes = [#{routes => [{"/test", Callback, #{methods => [get]}}]}],
        nova_router:add_routes(test_app, [Routes]),
        {ok, _, Value} = nova_router:lookup_url('_', <<"/test">>, <<"GET">>),
        ?assertMatch(#nova_handler_value{app = test_app}, Value)
    end}.

add_routes_registers_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        Routes = [#{routes => [{"/x", Callback, #{methods => [get]}}]}],
        nova_router:add_routes(my_app, [Routes]),
        Apps = nova_router:compiled_apps(),
        ?assertMatch({my_app, _}, lists:keyfind(my_app, 1, Apps))
    end}.

%% A security callback is declared with `security` on a route group.
%% Written on a single route, or as `secure` on the group, it is not read.
%% A callback there is refused, since the routes its author believes
%% protected would be open, and none of the routes is added.
misplaced_security_callback_is_refused_test_() ->
    Callback = fun(_) -> {json, #{}} end,
    Check = fun(_) -> false end,
    Group = fun(Options, Routes) ->
                    [maps:merge(#{prefix => "/admin", routes => [{"/open", Callback, #{methods => [get]}} | Routes]},
                                Options)]
            end,
    {setup, fun setup/0, fun cleanup/1,
     [fun() ->
              {Result, Events} =
                  nova_test_log:collect(fun() ->
                                                try nova_router:add_routes(misplaced_app, Routes)
                                                catch throw:Refusal -> Refusal
                                                end
                                        end),
              ?assertEqual({error, {misplaced_security, misplaced_app, Key, Path}}, Result),
              ?assertMatch([#{app := misplaced_app, key := Key, path := Path}], nova_test_log:errors(Events)),
              ?assertEqual({error, not_found}, nova_router:lookup_url('_', ~"/admin/open", ~"GET"))
      end || {Key, Path, Routes}
                 <- [{secure, "/admin", Group(#{secure => Check}, [])},
                     {security, "/admin/users",
                      Group(#{}, [{"/users", Callback, #{methods => [get], security => Check}}])},
                     {secure, "/admin/roles",
                      Group(#{}, [{"/roles", Callback, #{methods => [get], secure => Check}}])},
                     {security, "/admin/socket",
                      Group(#{}, [{"/socket", nova_router_add_routes_tests_ws, #{protocol => ws, security => Check}}])},
                     {secure, 404, Group(#{}, [{404, Callback, #{secure => Check}}])},
                     {secure, "/admin", Group(#{secure => true}, [])}]]}.

%% Written as false it asks for nothing, so the routes are added as before
%% and each misplaced key is named.
misplaced_security_key_that_is_false_is_warned_about_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Callback = fun(_) -> {json, #{}} end,
        Check = fun(_) -> false end,
        Routes = [#{prefix => "/admin", secure => false,
                    routes => [{"/users", Callback, #{methods => [get], security => false}},
                               {"/roles", Callback, #{methods => [get], secure => false}},
                               {404, Callback, #{secure => false}},
                               {"/open", Callback, #{methods => [get]}}]},
                  #{prefix => "/guarded", security => Check,
                    routes => [{"/page", Callback, #{methods => [get]}}]}],
        {ok, Events} = nova_test_log:collect(fun() -> nova_router:add_routes(misplaced_app, Routes) end),
        ?assertEqual([{secure, "/admin"}, {security, "/admin/users"}, {secure, "/admin/roles"}, {secure, 404}],
                     [{Key, Path} || {warning, {report, #{app := misplaced_app, key := Key, path := Path}}} <- Events]),
        ?assertMatch({ok, _, #nova_handler_value{secure = false}},
                     nova_router:lookup_url('_', ~"/admin/users", ~"GET")),
        ?assertMatch({ok, _, #nova_handler_value{secure = Check}},
                     nova_router:lookup_url('_', ~"/guarded/page", ~"GET"))
    end}.
