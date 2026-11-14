%%% A websocket route from the upgrade it refuses to the plugins that run
%%% when the connection ends.
-module(nova_ws_lifecycle_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([refused_upgrade_is_a_500/1,
         refused_upgrade_logs_one_error/1,
         post_ws_connection_plugin_runs_without_terminate/1,
         post_ws_connection_plugin_runs_after_terminate/1,
         route_extra_state_reaches_the_controller/1,
         group_extra_state_reaches_the_controller/1,
         route_extra_state_wins_over_the_group/1,
         catch_all_route_has_its_template_and_segments/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(OBSERVER, nova_ws_lifecycle_observer).

all() ->
    [refused_upgrade_is_a_500,
     refused_upgrade_logs_one_error,
     post_ws_connection_plugin_runs_without_terminate,
     post_ws_connection_plugin_runs_after_terminate,
     route_extra_state_reaches_the_controller,
     group_extra_state_reaches_the_controller,
     route_extra_state_wins_over_the_group,
     catch_all_route_has_its_template_and_segments].

init_per_suite(Config) ->
    Closing = [{post_ws_connection, nova_ws_lifecycle_pages, #{}}],
    Routes = [#{prefix => "/refusing",
                plugins => [],
                routes => [{"/ws", nova_ws_lifecycle_pages, #{protocol => ws}}]},
              #{prefix => "/plain",
                plugins => Closing,
                routes => [{"/ws", nova_ws_lifecycle_pages, #{protocol => ws}}]},
              #{prefix => "/closing",
                plugins => Closing,
                routes => [{"/ws", nova_ws_lifecycle_closing_pages, #{protocol => ws}}]},
              #{prefix => "/route-state",
                plugins => [],
                routes => [{"/ws", nova_ws_lifecycle_pages, #{protocol => ws, extra_state => #{room => ~"lobby"}}}]},
              #{prefix => "/group-state",
                plugins => [],
                extra_state => #{room => ~"hall"},
                routes => [{"/ws", nova_ws_lifecycle_pages, #{protocol => ws}},
                           {"/own", nova_ws_lifecycle_pages, #{protocol => ws, extra_state => #{room => ~"attic"}}}]},
              #{prefix => "/rooms",
                plugins => [],
                routes => [{"/[...]", nova_ws_lifecycle_pages, #{protocol => ws, extra_state => #{room => ~"any"}}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

refused_upgrade_is_a_500(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/refusing/ws")).

refused_upgrade_logs_one_error(Config) ->
    {_Response, Events} =
        nova_test_log:collect(fun() ->
                                      nova_test_listener:websocket_handshake(?config(port, Config), "/refusing/ws")
                              end),
    ?assertMatch([#{msg := ~"Websocket handler returned unknown result"}], nova_test_log:errors(Events)).

post_ws_connection_plugin_runs_without_terminate(Config) ->
    register(?OBSERVER, self()),
    [~"ok"] = nova_test_listener:websocket_texts(?config(port, Config), "/plain/ws", ~"hello", 1),
    ?assertEqual([{post_ws_connection, nova_ws_lifecycle_pages}], observations(1)).

post_ws_connection_plugin_runs_after_terminate(Config) ->
    register(?OBSERVER, self()),
    [~"ok"] = nova_test_listener:websocket_texts(?config(port, Config), "/closing/ws", ~"hello", 1),
    ?assertEqual([terminate, {post_ws_connection, nova_ws_lifecycle_closing_pages}], observations(2)).

route_extra_state_reaches_the_controller(Config) ->
    ?assertEqual([~"lobby"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/route-state/ws", ~"room", 1)).

group_extra_state_reaches_the_controller(Config) ->
    ?assertEqual([~"hall"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/group-state/ws", ~"room", 1)).

route_extra_state_wins_over_the_group(Config) ->
    ?assertEqual([~"attic"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/group-state/own", ~"room", 1)).

catch_all_route_has_its_template_and_segments(Config) ->
    Port = ?config(port, Config),
    ?assertEqual([~"/rooms/[...]"], nova_test_listener:websocket_texts(Port, "/rooms/a/b", ~"route", 1)),
    ?assertEqual([~"a,b"], nova_test_listener:websocket_texts(Port, "/rooms/a/b", ~"pathinfo", 1)),
    ?assertEqual([~"any"], nova_test_listener:websocket_texts(Port, "/rooms/a/b", ~"room", 1)).

observations(0) ->
    [];
observations(Count) ->
    receive
        {observed, What} -> [What | observations(Count - 1)]
    after 2000 ->
        []
    end.
