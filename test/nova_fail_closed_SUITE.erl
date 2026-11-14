%%% Whatever goes wrong before the controller, the request must end without
%%% reaching what the route was protecting.
-module(nova_fail_closed_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         crashed_plugin_does_not_upgrade_websocket/1,
         crashed_plugin_does_not_reach_controller/1,
         crashed_plugin_does_not_upgrade_protected_websocket/1,
         failed_plugin_does_not_upgrade_protected_websocket/1,
         failed_plugin_does_not_reach_controller/1,
         crashed_plugin_does_not_reach_cowboy_handler/1,
         failed_plugin_does_not_reach_cowboy_handler/1,
         denied_request_does_not_reach_cowboy_handler/1,
         allowed_request_reaches_cowboy_handler/1,
         invalid_security_value_denies/1,
         crashing_security_callback_denies/1,
         unexpected_security_return_denies/1
        ]).

-export([pre_request/4, plugin_info/0]).

-export([init/2]).

-include_lib("nova/include/nova_router.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     crashed_plugin_does_not_upgrade_websocket,
     crashed_plugin_does_not_reach_controller,
     crashed_plugin_does_not_upgrade_protected_websocket,
     failed_plugin_does_not_upgrade_protected_websocket,
     failed_plugin_does_not_reach_controller,
     crashed_plugin_does_not_reach_cowboy_handler,
     failed_plugin_does_not_reach_cowboy_handler,
     denied_request_does_not_reach_cowboy_handler,
     allowed_request_reaches_cowboy_handler,
     invalid_security_value_denies,
     crashing_security_callback_denies,
     unexpected_security_return_denies
    ].

init_per_suite(Config) ->
    Routes = [{"/", fun nova_fail_closed_pages:page/1, #{methods => [get]}}],
    Host = [
            #{prefix => "/crashing-plugin",
              plugins => [{pre_request, ?MODULE, #{}}],
              routes => [{"/ws", nova_test_app_ws_controller, #{protocol => ws}} | Routes]},
            #{prefix => "/protected-crashing-plugin",
              security => fun nova_fail_closed_pages:deny/1,
              plugins => [{pre_request, ?MODULE, #{}}],
              routes => [{"/ws", nova_test_app_ws_controller, #{protocol => ws}}]},
            #{prefix => "/protected-failing-plugin",
              security => fun nova_fail_closed_pages:deny/1,
              plugins => [{pre_request, ?MODULE, #{return => {error, refused}}}],
              routes => [{"/ws", nova_test_app_ws_controller, #{protocol => ws}} | Routes]},
            #{prefix => "/crashing", security => fun nova_fail_closed_pages:crash/1, routes => Routes},
            #{prefix => "/unexpected", security => fun nova_fail_closed_pages:unexpected/1, routes => Routes}
           ],
    Port = nova_test_listener:start(#{host => Host}),
    persistent_term:put(?MODULE, counters:new(1, [])),
    ok = add_cowboy_handler_route("/cowboy/crashing-plugin", fun nova_fail_closed_pages:deny/1, #{}),
    ok = add_cowboy_handler_route("/cowboy/failing-plugin", fun nova_fail_closed_pages:deny/1, #{return => {error, refused}}),
    ok = add_cowboy_handler_route("/cowboy/denied", fun nova_fail_closed_pages:deny/1, #{return => ok}),
    ok = add_cowboy_handler_route("/cowboy/open", false, #{return => ok}),
    ok = add_route_with_invalid_security("/invalid/"),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    persistent_term:erase(?MODULE),
    nova_test_listener:stop().

%% The router has no route syntax for a plain Cowboy handler yet, so the
%% value such a route compiles to is put in the routing table directly.
add_cowboy_handler_route(Path, Secure, PluginOptions) ->
    Value = #cowboy_handler_value{app = nova_test_app,
                                  handler = ?MODULE,
                                  arguments = #{},
                                  plugins = [{pre_request, [{fun ?MODULE:pre_request/4, PluginOptions}]}],
                                  secure = Secure},
    {ok, Dispatch} = nova_routing_trie:insert('_', Path, '_', Value, persistent_term:get(nova_dispatch), #{}),
    persistent_term:put(nova_dispatch, Dispatch).

%% The router refuses to compile such a route, so this is one that got into
%% the routing table some other way.
add_route_with_invalid_security(Path) ->
    Value = #nova_handler_value{app = nova_test_app,
                                callback = fun nova_fail_closed_pages:page/1,
                                secure = {nova_fail_closed_pages, page}},
    {ok, Dispatch} = nova_routing_trie:insert('_', Path, '_', Value, persistent_term:get(nova_dispatch), #{}),
    persistent_term:put(nova_dispatch, Dispatch).

init(Req, State) ->
    counters:add(persistent_term:get(?MODULE), 1, 1),
    {ok, cowboy_req:reply(200, #{}, ~"handler", Req), State}.

handler_calls() ->
    counters:get(persistent_term:get(?MODULE), 1).

pre_request(Req, _Env, #{return := ok}, State) ->
    {ok, Req, State};
pre_request(_Req, _Env, #{return := Return}, _State) ->
    Return;
pre_request(_Req, _Env, _Options, _State) ->
    error(plugin_crashed).

plugin_info() ->
    #{title => ~"crashing plugin",
      version => ~"1.0.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team <info@novaframework.org>"],
      description => ~"Crashes in pre_request",
      options => []}.

crashed_plugin_does_not_upgrade_websocket(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/crashing-plugin/ws")).

crashed_plugin_does_not_reach_controller(Config) ->
    {Status, Body} = nova_test_listener:get(?config(port, Config), "/crashing-plugin/"),
    ?assertEqual(500, Status),
    ?assertNotEqual(~"page", Body).

crashed_plugin_does_not_upgrade_protected_websocket(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config),
                                                        "/protected-crashing-plugin/ws")).

failed_plugin_does_not_upgrade_protected_websocket(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config),
                                                        "/protected-failing-plugin/ws")).

failed_plugin_does_not_reach_controller(Config) ->
    {Status, Body} = nova_test_listener:get(?config(port, Config), "/protected-failing-plugin/"),
    ?assertEqual(500, Status),
    ?assertNotEqual(~"page", Body).

crashed_plugin_does_not_reach_cowboy_handler(Config) ->
    Before = handler_calls(),
    {Status, Body} = nova_test_listener:get(?config(port, Config), "/cowboy/crashing-plugin"),
    ?assertEqual(500, Status),
    ?assertNotEqual(~"handler", Body),
    ?assertEqual(Before, handler_calls()).

failed_plugin_does_not_reach_cowboy_handler(Config) ->
    Before = handler_calls(),
    {Status, Body} = nova_test_listener:get(?config(port, Config), "/cowboy/failing-plugin"),
    ?assertEqual(500, Status),
    ?assertNotEqual(~"handler", Body),
    ?assertEqual(Before, handler_calls()).

denied_request_does_not_reach_cowboy_handler(Config) ->
    Before = handler_calls(),
    ?assertMatch({401, _}, nova_test_listener:get(?config(port, Config), "/cowboy/denied")),
    ?assertEqual(Before, handler_calls()).

allowed_request_reaches_cowboy_handler(Config) ->
    Before = handler_calls(),
    ?assertEqual({200, ~"handler"}, nova_test_listener:get(?config(port, Config), "/cowboy/open")),
    ?assertEqual(Before + 1, handler_calls()).

invalid_security_value_denies(Config) ->
    ?assertMatch({401, _}, nova_test_listener:get(?config(port, Config), "/invalid/")).

crashing_security_callback_denies(Config) ->
    {Status, Body} = nova_test_listener:get(?config(port, Config), "/crashing/"),
    ?assertEqual(500, Status),
    ?assertNotEqual(~"page", Body).

unexpected_security_return_denies(Config) ->
    ?assertMatch({401, _}, nova_test_listener:get(?config(port, Config), "/unexpected/")).
