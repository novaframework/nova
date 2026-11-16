-module(nova_test_app_ws_controller).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

init(Req) ->
    {ok, #{req => Req, received => 0}}.

websocket_init(State) ->
    {reply, {text, <<"connected">>}, State}.

websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State};
websocket_handle({text, <<"route">>}, State = #{req := #{req := Req}}) ->
    {reply, {text, maps:get(nova_route, Req, <<"absent">>)}, State};
websocket_handle({text, <<"crash">>} = Frame, State) ->
    erlang:error(deliberate_ws_crash, [Frame, State]);
websocket_handle({text, <<"close">>}, State) ->
    {stop, State};
websocket_handle({text, Message}, State = #{received := Received}) ->
    {reply, {text, <<"echo:", Message/binary>>}, State#{received => Received + 1}};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
