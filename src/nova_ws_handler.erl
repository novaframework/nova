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

-include_lib("kernel/include/logger.hrl").

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
    State0 = State#{mod => Module,
                    plugins => Plugins},
    ControllerData2 = ControllerData#{req => Req},
    upgrade_ws(Module, Req, State0, ControllerData2).


upgrade_ws(Module, Req, State, ControllerData) ->
    case Module:init(ControllerData) of
        {ok, NewControllerData} ->
            {cowboy_websocket, Req, State#{controller_data => NewControllerData}};
        Error ->
            logger:error(#{msg => "Websocket handler returned unkown result", handler => Module, returned => Error}),
            nova_router:render_status_page(500, Req)
    end.

websocket_init(State = #{mod := Mod}) ->
    case erlang:function_exported(Mod, websocket_init, 1) of
        true ->
            ControllerData = maps:get(controller_data, State, #{}),
            handle_ws(Mod,
                      websocket_init,
                      [],
                      State#{controller_data => ControllerData#{ws_handler_process => self()}});
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
    ControllerState = #{module => Mod,
                        function => Func,
                        arguments => Args},
    %% First run the pre-plugins and if everything goes alright we continue
    case run_plugins(PrePlugins, pre_ws_request, ControllerState, State) of
        {ok, State0} ->
            case invoke_controller(Mod, Func, Args, State0) of
                {stop, _StopReason} = S ->
                    S;
                Result ->
                    %% Run the post-plugins
                    PostPlugins = proplists:get_value(post_ws_request, Plugins, []),
                    run_plugins(PostPlugins, post_ws_request, ControllerState, Result)
            end;
        {stop, _} = Stop ->
            Stop
    end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{controller_data => #{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


invoke_controller(Mod, Func, Args, State = #{controller_data := ControllerData}) ->
    try erlang:apply(Mod, Func, Args ++ [ControllerData]) of
        RetObj ->
            case nova_handlers:get_handler(ws) of
                {ok, Callback} ->
                    Callback(RetObj, State);
                {error, not_found} ->
                    logger:error(#{msg => "Websocket handler not found. Check that a handler is
                                           registred on handle 'ws'",
                                   controller => Mod, function => Func, return => RetObj}),
                    {stop, State}
            end
    catch Class:Reason:Stacktrace ->
            logger:error(#{msg => "Controller crashed", class => Class,
                           reason => Reason, stacktrace => Stacktrace}),
            {stop, State}
    end.


run_plugins([], _Callback, #{controller_result := {stop, _} = Signal}, _State) ->
    Signal;
run_plugins([], _Callback, _ControllerState, State) ->
    {ok, State};
run_plugins([{Module, Options}|Tl], Callback, ControllerState, State) ->
    try erlang:apply(Module, Callback, [ControllerState, State, Options]) of
        {ok, ControllerState0, State0} ->
            run_plugins(Tl, Callback, ControllerState0, State0);
        %% Stop indicates that we want the entire pipe of plugins/controller to be stopped.
        {stop, State0} ->
            {stop, State0};
        %% Break is used to signal that we are stopping further executing of plugins within the same Callback
        {break, State0} ->
            {ok, State0};
        {error, Reason} ->
            logger:error(#{msg => "Plugin returned error", plugin => Module, function => Callback, reason => Reason}),
            {stop, State}
    catch
        Class:Reason:Stacktrace ->
            logger:error(#{msg => "Plugin crashed", class => Class, reason => Reason, stacktrace => Stacktrace}),
            {stop, State}
    end.



-ifdef(TEST).

-endif.
