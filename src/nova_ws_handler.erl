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

-type nova_ws_state() :: #{controller_data := map(),
                           mod := atom(),
                           subprotocols := [any()],
                           nova_handler := nova_ws_handler,
                           _ := _}.

-export_type([nova_ws_state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req, State) ->
    %% Call the http-handler in order to correctly handle potential plugins for the http-request
    {ok, PrePlugins} = nova_plugin:get_plugins(pre_ws_upgrade),
    case run_plugins(PrePlugins, pre_ws_upgrade, State#{req => Req}) of
        {ok, State0 = #{controller_data := ControllerData, mod := Mod}} ->
            ControllerData0 = ControllerData#{req => Req},
            case Mod:init(ControllerData0) of
                {ok, NewControllerData} ->
                    {cowboy_websocket, Req, State0#{controller_data => NewControllerData}};
                Error ->
                    ?ERROR("Websocket handler ~p returned unkown result ~p", [Mod, Error]),
                    Req1 = cowboy_req:reply(500, Req),
                    {ok, Req1, State0}
            end;
        Stop ->
            Stop
    end.

websocket_init(State = #{mod := Mod}) ->
    case erlang:function_exported(Mod, websocket_init, 1) of
        true ->
            handle_ws(Mod, websocket_init, [], State);
        _ ->
            {ok, State}
    end.

websocket_handle(Frame, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_handle, [Frame], State).

websocket_info(Msg, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_info, [Msg], State).

terminate(Reason, PartialReq, State = #{mod := Mod}) ->
    case erlang:function_exported(Mod, terminate, 3) of
        true ->
            handle_ws(Mod, terminate, [Reason, PartialReq], State);
        _ ->
            ok
    end.

handle_ws(Mod, Func, Args, State = #{controller_data := _ControllerData}) ->
    {ok, PrePlugins} = nova_plugin:get_plugins(pre_ws_request),
    case run_plugins(PrePlugins, pre_ws_request, State) of
        {ok, State0} ->
            invoke_controller(Mod, Func, Args, State0);
        Stop ->
            Stop
    end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{controller_data => #{}}).

invoke_controller(Mod, Func, Args, State = #{controller_data := ControllerData}) ->
    try
        ControllerResult =
            case erlang:apply(Mod, Func, Args ++ [ControllerData]) of
                {reply, Frame, NewControllerData} ->
                    {reply, Frame, State#{controller_data => NewControllerData}};
                {reply, Frame, NewControllerData, hibernate} ->
                    {reply, Frame, State#{controller_data => NewControllerData}, hibernate};
                {ok, NewControllerData} ->
                    {ok, State#{controller_data => NewControllerData}};
                {ok, NewControllerData, hibernate} ->
                    {ok, State#{controller_data => NewControllerData}, hibernate};
                {stop, NewControllerData} ->
                    {stop, State#{controller_data => NewControllerData}};
                ok ->
                    {ok, State}
            end,
        case ControllerResult of
            {stop, _} = Stop ->
                Stop;
            {_, State0} ->
                {ok, PostPlugins} = nova_plugin:get_plugins(post_ws_request),
                run_plugins(PostPlugins, post_ws_request, State0);
            _ ->
                ControllerResult
        end
    catch
        Type:Reasons:Stacktrace ->
            ?ERROR("Websocket failed with ~p:~p.~nStacktrace:~n~p", [Type, Reasons, Stacktrace]),
            {stop, State}
    end.


run_plugins([], _Callback, State) ->
    {ok, State};
run_plugins([{_Prio, #{id := Id, module := Module, options := Options}}|Tl], Callback, State) ->
    try Module:Callback(State, Options) of
        {ok, State0} ->
            run_plugins(Tl, Callback, State0);
        %% Stop indicates that we want the entire pipe of plugins/controller to be stopped.
        {stop, State0} ->
            {stop, State0};
        %% Break is used to signal that we are stopping further executing of plugins within the same Callback
        {break, State0} ->
            {ok, State0};
        {error, Reason} ->
            ?ERROR("Plugin (~p:~p/2) with id: ~p returned error with reason ~p", [Module, Callback, Id, Reason]),
            {stop, State}
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Plugin with id: ~p failed in execution. Type: ~p Reason: ~p~nStacktrace:~n~p",
                   [Id, Type, Reason, Stacktrace]),
            {stop, State}
    end.


-ifdef(TEST).

-endif.
