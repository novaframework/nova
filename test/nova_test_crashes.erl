%%% A step of the pipeline, and a WebSocket handler, that crash with what
%%% they were given in the reason.
-module(nova_test_crashes).

-export([execute/2, websocket_handle/2, websocket_info/2]).

execute(Req, _Env) ->
    error({badmatch, Req}).

websocket_handle(_Frame, State) ->
    error({badmatch, State}).

websocket_info(Info, State) ->
    {Info, State}.
