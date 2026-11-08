-module(nova_test_app_ws_crash_controller).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(_ControllerData) ->
    erlang:error(deliberate_init_crash).

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
