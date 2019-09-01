%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling websockets
%%% @end
%%% Created :  1 Sep 2019 by Niclas Axelsson <niclas@burbas.se>

-module(nova_ws_controller).

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
% Type/Spec declarations  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type deprecated_call_result(State) :: {ok, State}
                                     | {ok, State, hibernate}
                                     | {reply, cow_ws:frame() | [cow_ws:frame()], State}
                                     | {reply, cow_ws:frame() | [cow_ws:frame()], State, hibernate}
                                     | {stop, State}.

-spec init(Req :: cowboy_req:req(), State :: map()) -> {ok, Req :: cowboy_req:req(), State :: map()} |
                                                       {cowboy_websocket, Req :: cowboy_req:req(), State :: map()}.
-spec websocket_init(State :: map()) -> deprecated_call_result(State) when State :: map().

-spec websocket_handle(ping | pong | {text | binary | ping | pong, binary()}, State)
                      -> deprecated_call_result(State) when State :: map().

-spec websocket_info(Msg :: any(), State) -> deprecated_call_result(State) when State :: map().

-spec terminate(Reason :: any(), PatialReq :: cowboy_req:req(), State :: map()) -> ok.

-spec handle_ws(Mod :: atom(), Func :: atom(), Args :: list(), State) ->
                       deprecated_call_result(State) when State :: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State = #{mod := Mod}) ->
    case nova_http_controller:check_security(Req, State) of
        true ->
            case Mod:init(Req) of
                {ok, Substate} ->
                    {cowboy_websocket, Req, State#{substate => Substate}};
                Error ->
                    ?ERROR("Websocket handler ~p returned unkown result ~p", [Mod, Error]),
                    Req1 = cowboy_req:reply(500, Req),
                    {ok, Req1, State}
            end;
        _ ->
            Req1 = cowboy_req:reply(401, Req),
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
                {stop, State#{substate => NewSubstate}}
        end
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
          ?ERROR("Websocket failed with ~p:~p.~nStacktrace:~n~p", [Type, Reason, Stacktrace]),
          Substate
     end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{substate => #{}}).


-ifdef(TEST).

-endif.
