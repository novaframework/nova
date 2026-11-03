-module(nova_test_app_session_ws_controller).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

%% Runs in the request process, so the 101 carries the session cookie.
init(#{req := Req} = State) ->
    ok = nova_session:set(Req, ~"ws", ~"yes"),
    {ok, State}.

%% A process spawned with the upgrade request writes the session while the
%% connection process runs the websocket.
websocket_handle({text, ~"spawn-write"}, State = #{req := Req}) ->
    Self = self(),
    _ = spawn(fun() -> Self ! {spawned_write, nova_session:set(Req, ~"later", ~"yes")} end),
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({spawned_write, ok}, State) ->
    {reply, {text, ~"written"}, State};
websocket_info(_Other, State) ->
    {reply, {text, ~"stray"}, State}.
