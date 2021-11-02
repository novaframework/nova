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
                           _ := _}.

-export_type([nova_ws_state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req = #{method := _Method, plugins := Plugins}, State = #{module := Module}) ->
    %% Call the http-handler in order to correctly handle potential plugins for the http-request
    ControllerData = maps:get(controller_data, State, #{}),
    State0 = State#{controller_data => ControllerData,
                    mod => maps:get(mod, State, undefined),
                    plugins => Plugins},
    upgrade_ws(Module, Req, State0, ControllerData).


upgrade_ws(Module, Req, State, ControllerData) ->
    case Module:init(ControllerData) of
        {ok, NewControllerData} ->
            {cowboy_websocket, Req, State#{controller_data => NewControllerData}};
        Error ->
            ?ERROR("Websocket handler ~p returned unkown result ~p", [Module, Error]),
            nova_router:render_status_page(500, Req)
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

handle_ws(Mod, Func, Args, State = #{controller_data := _ControllerData, plugins := Plugins}) ->
    PrePlugins = proplists:get_value(pre_ws_request, Plugins, []),
    case run_plugins(PrePlugins, pre_ws_request, State) of
        {ok, State0} ->
            invoke_controller(Mod, Func, Args, State0);
        Stop ->
            Stop
    end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{controller_data => #{}}).

invoke_controller(Mod, Func, Args, State = #{controller_data := ControllerData, plugins := Plugins}) ->
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
                PostPlugins = proplists:get_value(post_ws_request, Plugins, []),
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
run_plugins([{Module, Options}|Tl], Callback, State) ->
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
            ?ERROR("Plugin (~p:~p/2) returned error with reason ~p", [Module, Callback, Reason]),
            {stop, State}
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Plugin failed in execution. Type: ~p Reason: ~p~nStacktrace:~n~p",
                   [Type, Reason, Stacktrace]),
            {stop, State}
    end.



-ifdef(TEST).

-endif.
