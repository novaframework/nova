%%% A websocket controller with terminate/3, for nova_ws_lifecycle_SUITE.
-module(nova_ws_lifecycle_closing_pages).

-export([init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle(_Frame, ControllerData) ->
    {reply, {text, ~"ok"}, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.

terminate(_Reason, _PartialReq, _ControllerData) ->
    nova_test_observer:observed(nova_ws_lifecycle_observer, terminate),
    ok.
