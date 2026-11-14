%%% Whatever goes wrong before the controller, the request must end without
%%% reaching what the route was protecting.
-module(nova_fail_closed_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         crashed_plugin_does_not_upgrade_websocket/1,
         crashed_plugin_does_not_reach_controller/1,
         invalid_security_value_denies/1,
         crashing_security_callback_denies/1,
         unexpected_security_return_denies/1
        ]).

-export([page/1, crash/1, unexpected/1, pre_request/4, plugin_info/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     crashed_plugin_does_not_upgrade_websocket,
     crashed_plugin_does_not_reach_controller,
     invalid_security_value_denies,
     crashing_security_callback_denies,
     unexpected_security_return_denies
    ].

init_per_suite(Config) ->
    Routes = [{"/", fun ?MODULE:page/1, #{methods => [get]}}],
    Host = [
            #{prefix => "/crashing-plugin",
              plugins => [{pre_request, ?MODULE, #{}}],
              routes => [{"/ws", nova_test_app_ws_controller, #{protocol => ws}} | Routes]},
            #{prefix => "/invalid", security => true, routes => Routes},
            #{prefix => "/crashing", security => fun ?MODULE:crash/1, routes => Routes},
            #{prefix => "/unexpected", security => fun ?MODULE:unexpected/1, routes => Routes}
           ],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

page(_Req) ->
    {status, 200, #{}, ~"page"}.

crash(_Req) ->
    error(security_callback_crashed).

unexpected(_Req) ->
    {ok, allowed}.

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

invalid_security_value_denies(Config) ->
    ?assertMatch({401, _}, nova_test_listener:get(?config(port, Config), "/invalid/")).

crashing_security_callback_denies(Config) ->
    {Status, Body} = nova_test_listener:get(?config(port, Config), "/crashing/"),
    ?assertEqual(500, Status),
    ?assertNotEqual(~"page", Body).

unexpected_security_return_denies(Config) ->
    ?assertMatch({401, _}, nova_test_listener:get(?config(port, Config), "/unexpected/")).
