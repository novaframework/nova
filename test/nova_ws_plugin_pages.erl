%%% The controllers of nova_ws_plugin_SUITE.
-module(nova_ws_plugin_pages).

-export([init/1, websocket_handle/2, websocket_info/2]).

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, _Text}, ControllerData) ->
    {reply, {text, maps:get(mark, ControllerData, ~"no plugin ran")}, ControllerData};
websocket_handle(_Frame, ControllerData) ->
    {ok, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.
