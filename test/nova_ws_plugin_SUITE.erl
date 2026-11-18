%%% Websocket plugins declared on a route run for the frames of that route.
-module(nova_ws_plugin_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([pre_ws_request_plugin_runs_before_the_controller/1,
         post_ws_request_plugin_runs_after_the_controller/1,
         pre_ws_request_plugin_can_stop_the_connection/1,
         post_plugin_stop/1,
         route_without_plugins_is_unaffected/1]).

-export([pre_ws_request/3, post_ws_request/3, plugin_info/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [pre_ws_request_plugin_runs_before_the_controller,
     post_ws_request_plugin_runs_after_the_controller,
     pre_ws_request_plugin_can_stop_the_connection,
     post_plugin_stop,
     route_without_plugins_is_unaffected].

init_per_suite(Config) ->
    Routes = [#{prefix => "/pre",
                plugins => [{pre_ws_request, ?MODULE, #{mark => ~"pre plugin ran"}}],
                routes => [{"/ws", nova_ws_plugin_pages, #{protocol => ws}}]},
              #{prefix => "/post",
                plugins => [{post_ws_request, ?MODULE, #{}}],
                routes => [{"/ws", nova_ws_plugin_pages, #{protocol => ws}}]},
              #{prefix => "/stopping",
                plugins => [{pre_ws_request, ?MODULE, #{stop => true}}],
                routes => [{"/ws", nova_ws_plugin_pages, #{protocol => ws}}]},
              #{prefix => "/post-stopping",
                plugins => [{post_ws_request, ?MODULE, #{stop => true}}],
                routes => [{"/ws", nova_ws_plugin_pages, #{protocol => ws}}]},
              #{prefix => "/plain",
                plugins => [],
                routes => [{"/ws", nova_ws_plugin_pages, #{protocol => ws}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

pre_ws_request(_ControllerState, State, #{stop := true}) ->
    {stop, State};
pre_ws_request(ControllerState, State = #{controller_data := ControllerData}, #{mark := Mark}) ->
    {ok, ControllerState, State#{controller_data => ControllerData#{mark => Mark}}}.

post_ws_request(_ControllerState, State, #{stop := true}) ->
    {stop, State};
post_ws_request(ControllerState, State = #{commands := Commands}, _Options) ->
    {ok, ControllerState, State#{commands => Commands ++ [{text, ~"post plugin ran"}]}}.

plugin_info() ->
    #{title => ~"websocket test plugin",
      version => ~"1.0.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team <info@novaframework.org>"],
      description => ~"Marks the frames it sees",
      options => []}.

pre_ws_request_plugin_runs_before_the_controller(Config) ->
    ?assertEqual([~"pre plugin ran"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/pre/ws", ~"hello", 1)).

post_ws_request_plugin_runs_after_the_controller(Config) ->
    ?assertEqual([~"no plugin ran", ~"post plugin ran"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/post/ws", ~"hello", 2)).

pre_ws_request_plugin_can_stop_the_connection(Config) ->
    ?assertEqual([closed],
                 nova_test_listener:websocket_texts(?config(port, Config), "/stopping/ws", ~"hello", 1)).

%% The connection is closed, and nothing crashes with its state in the
%% reason: that has the request of the upgrade in it.
post_plugin_stop(Config) ->
    {Texts, Events} = nova_test_log:collect(
                        fun() ->
                                nova_test_listener:websocket_texts(?config(port, Config), "/post-stopping/ws",
                                                                   ~"hello", 1)
                        end),
    ?assertEqual([closed], Texts),
    ?assertEqual([], [Event || {error, _} = Event <- Events]),
    ?assertEqual([#{msg => ~"A plugin stopped the connection", phase => post_ws_request,
                    module => nova_ws_plugin_pages, function => websocket_handle}],
                 [Report || {warning, {report, Report}} <- Events]).

route_without_plugins_is_unaffected(Config) ->
    ?assertEqual([~"no plugin ran"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/plain/ws", ~"hello", 1)).
