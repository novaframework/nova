%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling websockets
%%% @end

-module(nova_ws_handler).

-export([
         init/2,
         terminate/3,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req, State = #{mod := Mod}) ->
    case Mod:init(Req) of
        {ok, Substate} ->
            {cowboy_websocket, Req, State#{substate => Substate}};
        Error ->
            ?ERROR("Websocket handler ~p returned unkown result ~p", [Mod, Error]),
            Req1 = cowboy_req:reply(500, Req),
            {ok, Req1, State}
    end.

websocket_init(State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_init, [], State).

websocket_handle(Frame, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_handle, [Frame], State).

websocket_info(Msg, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_info, [Msg], State).

terminate(Reason, PartialReq, State = #{mod := Mod}) ->
    handle_ws(Mod, terminate, [Reason, PartialReq], State),
    ok.



handle_ws(Mod, Func, Args, State = #{substate := Substate}) ->
    try
        case erlang:apply(Mod, Func, Args ++ [Substate]) of
            {reply, Frame, NewSubstate} ->
                {reply, Frame, State#{substate => NewSubstate}};
            {reply, Frame, NewSubstate, hibernate} ->
                {reply, Frame, State#{substate => NewSubstate}, hibernate};
            {ok, NewSubstate} ->
                {ok, State#{substate => NewSubstate}};
            {ok, NewSubstate, hibernate} ->
                {ok, State#{substate => NewSubstate}, hibernate};
            {stop, NewSubstate} ->
                {stop, State#{substate => NewSubstate}};
            ok ->
                {ok, State}
        end
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Websocket failed with ~p:~p.~nStacktrace:~n~p", [Type, Reason, Stacktrace]),
            Substate
    end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{substate => #{}}).


-ifdef(TEST).

-endif.
