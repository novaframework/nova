-module(test_controller).

%% Test controller for nova_handler tests
-export([
         init/1,
         init_crash/1,
         init_throw_status/1,
         init_websocket/1,
         terminate/3
        ]).

%% Successful controller
init(#{test_mode := success} = Req) ->
    {ok, Req#{result => success}, #{}}.

%% Controller that crashes
init_crash(_Req) ->
    error(simulated_crash).

%% Controller that throws status code exception
init_throw_status(_Req) ->
    throw({status, 500, #{}, <<"Internal error">>}).

%% WebSocket upgrade
init_websocket(Req) ->
    {cowboy_websocket, Req, #{}}.

%% Terminate callback
terminate(_Reason, _Req, _State) ->
    ok.
