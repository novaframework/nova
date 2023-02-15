-module(nova_ws_heartbeat_plugin).
-behaviour(nova_plugin).

-export([
         ws_init_request/3,
         pre_ws_request/3,
         plugin_info/0
        ]).


ws_init_request(_ControllerData, State, _Options) ->
    %% Set the controller to return a ping
    {ok, State#{commands => [ping]}}.

pre_ws_request(#{stage := websocket_handle, args := [pong], ws_handler_process := Pid}, State, Options) ->
    %% We've received a pong from the other end - let's make it count
    Timeout = maps:get(timeout, Options, 5000),
    erlang:send_after(Timeout, Pid, {?MODULE, send_ping}),
    {break, State};
pre_ws_request(#{stage := websocket_info, args := [{?MODULE, send_ping}]}, State, _Options) ->
    {break, State#{commands => [ping]}};
pre_ws_request(_ControllerState, State, _Options) ->
    {ok, State}.


plugin_info() ->
    {<<"Websocket heartbeat">>,
     <<"0.0.1">>,
     <<"Nova team <info@novaframework.org>">>,
     <<"Set up automatic heartbeat process">>,
     [
      {timeout, <<"How long between heartbeat-signals">>}
     ]}.
