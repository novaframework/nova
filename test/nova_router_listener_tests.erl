%%% A route group that names its listeners, and routes that name their HTTP
%%% versions, in routing tables compiled without nova_sup.
-module(nova_router_listener_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-export([page/1]).

-define(KEY, {nova_dispatch, nova_router_listener_tests}).
-define(SERVING, #{name => admin, protocols => [http1, http2], websocket => [http1]}).

page(_Req) -> {status, 200}.

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Prev.

cleanup(Prev) ->
    ok = nova_router:delete_dispatch(?KEY),
    nova_test_helper:cleanup_nova_env(Prev).

compiled(Serving, Routes) ->
    _ = nova_router:compile_fresh([], ?KEY, Serving),
    nova_router:add_routes(test_app, Routes, ?KEY).

found(Path) ->
    element(1, nova_router:lookup_url('_', Path, ~"GET", ?KEY)).

group(Listener) ->
    #{prefix => "/admin", listener => Listener, routes => [{"/", fun ?MODULE:page/1, #{methods => [get]}}]}.

router_listener_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{"group is compiled into the table of a listener it names",
       fun() ->
               ok = compiled(?SERVING, [group(admin)]),
               ?assertEqual(ok, found(~"/admin/")),
               ?assertEqual([{served, test_app, "/admin", [admin]}], nova_router:bound_groups(?KEY))
       end},
      {"group is left out of the table of another listener",
       fun() ->
               ok = compiled(?SERVING, [group([public, internal])]),
               ?assertEqual(error, found(~"/admin/")),
               ?assertEqual([{skipped, test_app, "/admin", [public, internal]}], nova_router:bound_groups(?KEY))
       end},
      {"group is left out of a table whose listener is not known",
       fun() ->
               ok = compiled(undefined, [group(admin)]),
               ?assertEqual(error, found(~"/admin/"))
       end},
      {"listener of a group is a name or a list of names",
       fun() ->
               [?assertThrow({error, {invalid_route_listener, test_app, Value}}, compiled(?SERVING, [group(Value)]))
                || Value <- [[], "admin", [admin, ~"public"], ~"admin"]]
       end},
      {"group is noted once, and forgotten with its application",
       fun() ->
               ok = compiled(?SERVING, [group(admin), group(public)]),
               ok = nova_router:add_routes(test_app, [group(admin), group(public)], ?KEY),
               ?assertEqual([{served, test_app, "/admin", [admin]}, {skipped, test_app, "/admin", [public]}],
                            nova_router:bound_groups(?KEY)),
               ok = nova_router:remove_application(test_app, ?KEY),
               ?assertEqual([], nova_router:bound_groups(?KEY))
       end},
      {"table that is deleted forgets its listener and its groups",
       fun() ->
               ok = compiled(?SERVING, [group(admin)]),
               ok = nova_router:delete_dispatch(?KEY),
               ?assertEqual([], nova_router:bound_groups(?KEY)),
               _ = nova_router:compile_fresh([], ?KEY),
               ok = nova_router:add_routes(test_app, [group(admin)], ?KEY),
               ?assertEqual(error, found(~"/admin/"))
       end},
      {"versions of a route are those its listener has",
       fun() ->
               Route = {"/v", fun ?MODULE:page/1, #{methods => [get], http_versions => [http3, http2]}},
               ok = compiled(?SERVING, [#{routes => [Route]}]),
               {ok, _Bindings, #nova_handler_value{plugins = Plugins}} =
                   nova_router:lookup_url('_', ~"/v", ~"GET", ?KEY),
               ?assertMatch([{nova_route, ~"/v"}, {nova_http_versions, [http2]} | _], Plugins)
       end},
      {"websocket route needs a version the server takes websockets over",
       fun() ->
               Socket = {"/ws", some_ws_controller, #{protocol => ws, http_versions => [http2]}},
               ?assertThrow({error, {websocket_route_not_served, "/ws", admin}},
                            compiled(?SERVING, [#{routes => [Socket]}])),
               ?assertEqual(ok, compiled(?SERVING, [#{routes => [{"/ws", some_ws_controller, #{protocol => ws}}]}]))
       end}]}.
