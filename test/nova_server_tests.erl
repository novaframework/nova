-module(nova_server_tests).
-include_lib("eunit/include/eunit.hrl").

-export([execute/2]).

%%====================================================================
%% nova_server: the server module Nova uses, and execute/2 running a
%% pipeline of middlewares. This module is the middleware: it notes its
%% call on the request and stops once `stop_after' calls are made.
%%====================================================================

execute(#{calls := Calls} = Req, #{stop_after := StopAfter} = Env) ->
    Req1 = Req#{calls := Calls + 1},
    case Calls + 1 of
        StopAfter -> {stop, Req1};
        _ -> {ok, Req1, Env#{seen => Calls + 1}}
    end.

execute_runs_every_middleware_test() ->
    Env = #{nova_middlewares => [?MODULE, ?MODULE, ?MODULE], stop_after => 0},
    ?assertMatch({ok, #{calls := 3}, #{seen := 3}}, nova_server:execute(#{calls => 0}, Env)).

execute_stops_where_a_middleware_stops_test() ->
    Env = #{nova_middlewares => [?MODULE, ?MODULE, ?MODULE], stop_after => 2},
    ?assertEqual({stop, #{calls => 2}}, nova_server:execute(#{calls => 0}, Env)).

execute_without_middlewares_test() ->
    ?assertEqual({ok, #{}, #{nova_middlewares => []}},
                 nova_server:execute(#{}, #{nova_middlewares => []})).

middlewares_test() ->
    ?assertEqual([nova_router, nova_plugin_handler, nova_security_handler,
                  nova_handler, nova_plugin_handler],
                 nova_server:middlewares(#{})),
    ?assertEqual([my_mw], nova_server:middlewares(#{middleware_handlers => [my_mw]})).

module_test_() ->
    {setup,
     fun() -> application:unset_env(nova, server) end,
     fun(_) -> application:unset_env(nova, server) end,
     fun() ->
             ?assertEqual(nova_server_cowboy, nova_server:module()),
             application:set_env(nova, server, nova_fake_server),
             ?assertEqual(nova_fake_server, nova_server:module())
     end}.

of_req_test() ->
    ?assertEqual(nova_fake_server, nova_server:of_req(#{nova_server => nova_fake_server})),
    ?assertEqual(nova_server_cowboy, nova_server:of_req(#{})).
