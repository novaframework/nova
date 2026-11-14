%%% The controllers of nova_ws_upgrade_SUITE.
-module(nova_ws_upgrade_pages).

-export([page/1, init/1, websocket_handle/2, websocket_info/2]).

-define(SMALL_FRAME, 16).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

init(ControllerData = #{req := #{path := ~"/small/ws"}}) ->
    {ok, ControllerData, #{max_frame_size => ?SMALL_FRAME}};
init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, Text}, ControllerData) ->
    {reply, {text, Text}, ControllerData};
websocket_handle(_Frame, ControllerData) ->
    {ok, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.
