%%% Author: Niclas Axelsson <niclas@burbas.se>

-module(nova_ws_handler).
-moduledoc false.

-export([
         init/2,
         terminate/3,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

-include_lib("kernel/include/logger.hrl").

-type nova_ws_state() :: #{controller_data := map(),
                           mod := atom(),
                           _ => _}.

-export_type([nova_ws_state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req = #{method := _Method, plugins := Plugins}, State = #{module := Module}) ->
    %% Call the http-handler in order to correctly handle potential plugins for the http-request
    ControllerData = maps:get(controller_data, State, #{}),
    State0 = State#{mod => Module,
                    plugins => Plugins,
                    route => maps:get(nova_route, Req, undefined)},
    ControllerData2 = ControllerData#{req => Req},
    upgrade_ws(Module, Req, State0, ControllerData2).

upgrade_ws(Module, Req, State, ControllerData) ->
    case Module:init(ControllerData) of
        {ok, NewControllerData} ->
            {cowboy_websocket, Req, State#{controller_data => NewControllerData}};
        {ok, NewControllerData, WebsocketOptions} when is_map(WebsocketOptions) ->
            {cowboy_websocket, Req, State#{controller_data => NewControllerData}, WebsocketOptions};
        Error ->
            ?LOG_ERROR(#{msg => ~"Websocket handler returned unknown result", handler => Module,
                         returned => nova_redact:term(Error)}),
            %% nova_handler renders the 500 status page for this
            {status, 500, Req#{crash_info => #{class => error, reason => unsupported_websocket_init_result}}, State}
    end.

websocket_init(State = #{mod := Mod}) ->
    %% Inject the websocket process into the state
    ControllerData = maps:get(controller_data, State, #{}),
    NewState = start_connection(State#{controller_data => ControllerData#{ws_handler_process => self()}}),

    case erlang:function_exported(Mod, websocket_init, 1) of
        true ->
            handle_ws(Mod, websocket_init, [], NewState);
        _ ->
            {ok, NewState}
    end.

websocket_handle(Frame, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_handle, [Frame], State).

websocket_info(Msg, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_info, [Msg], State).

terminate(Reason, PartialReq, State) ->
    ok = stop_connection(Reason, State),
    terminate_controller(Reason, PartialReq, State).

terminate_controller(Reason, PartialReq, State = #{controller_data := ControllerData, mod := Mod,
                                                   plugins := Plugins}) ->
    case erlang:function_exported(Mod, terminate, 3) of
        true ->
            try
                Mod:terminate(Reason, PartialReq, ControllerData)
            catch
                Class:TerminateReason:Stacktrace ->
                    ?LOG_ERROR(#{msg => ~"Controller crashed", class => Class,
                                 reason => nova_redact:term(TerminateReason),
                                 stacktrace => nova_redact:stacktrace(Stacktrace)})
            end;
        false ->
            ok
    end,
    TerminatePlugins = proplists:get_value(post_ws_connection, Plugins, []),
    ControllerState = #{module => Mod,
                        function => terminate,
                        arguments => [Reason, PartialReq, State]},
    _ = run_plugins(TerminatePlugins, post_ws_connection, ControllerState, State),
    ok;
terminate_controller(Reason, PartialReq, State) ->
    ?LOG_ERROR(#{msg => ~"Terminate called", reason => nova_redact:term(Reason),
                 partial_req => nova_redact:term(PartialReq), state => nova_redact:term(State)}),
    ok.


handle_ws(Mod, Func, Args, State = #{controller_data := _ControllerData, plugins := Plugins}) ->
    PrePlugins = proplists:get_value(pre_ws_request, Plugins, []),
    ControllerState = #{module => Mod,
                        function => Func,
                        arguments => Args},
    %% First run the pre-plugins and if everything goes alright we continue
    State0 = State#{commands => []},
    case run_plugins(PrePlugins, pre_ws_request, ControllerState, State0) of
        {ok, State1} ->
            case invoke_controller(Mod, Func, Args, State1) of
                {stop, _StopReason} = S ->
                    S;
                State2 ->
                    %% Run the post-plugins
                    PostPlugins = proplists:get_value(post_ws_request, Plugins, []),
                    {ok, State3 = #{commands := Cmds}} = run_plugins(PostPlugins, post_ws_request,
                                                                     ControllerState, State2),
                    %% Remove the commands from the map
                    State4 = maps:remove(commands, State3),

                    %% Check if we are hibernating
                    case maps:get(hibernate, State4, false) of
                        false ->
                            {Cmds, State4};
                        _ ->
                            {Cmds, maps:remove(hibernate, State4), hibernate}
                    end
            end;
        {stop, _} = Stop ->
            ?LOG_WARNING(#{msg => ~"Got stop signal", signal => nova_redact:term(Stop)}),
            Stop
    end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{controller_data => #{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_connection(State) ->
    State#{nova_telemetry => nova_telemetry:start([nova, websocket, connection], connection_metadata(State))}.

stop_connection(Reason, #{nova_telemetry := Span}) ->
    %% Only the tag: {remote, Code, Payload} carries what the client sent.
    nova_telemetry:stop(Span, #{reason => nova_telemetry:reason_tag(Reason)});
stop_connection(_Reason, _State) ->
    ok.

connection_metadata(State) ->
    #{module => maps:get(mod, State, undefined),
      app => maps:get(app, State, undefined),
      route => maps:get(route, State, undefined)}.

invoke_controller(Mod, Func, Args, State = #{controller_data := ControllerData}) ->
    Span = nova_telemetry:start([nova, websocket, handle], (connection_metadata(State))#{callback => Func}),
    try erlang:apply(Mod, Func, Args ++ [ControllerData]) of
        RetObj ->
            nova_telemetry:stop(Span, #{}),
            handle_result(Mod, Func, RetObj, State)
    catch
        Class:Reason:Stacktrace ->
            nova_telemetry:exception(Span, Class, Reason, Stacktrace),
            ?LOG_ERROR(#{msg => <<"Controller crashed">>, class => Class,
                           reason => nova_redact:term(Reason),
                           stacktrace => nova_redact:stacktrace(Stacktrace)}),
            {stop, State}
    end.

handle_result(Mod, Func, RetObj, State) ->
    case nova_handlers:get_handler(ws) of
        {ok, Callback} ->
            try
                Callback(RetObj, State)
            catch
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR(#{msg => ~"Websocket handler failed on what the controller returned",
                                 controller => Mod, function => Func, class => Class,
                                 reason => nova_redact:term(Reason),
                                 stacktrace => nova_redact:stacktrace(Stacktrace)}),
                    {stop, State}
            end;
        {error, not_found} ->
            ?LOG_ERROR(#{msg => ~"Websocket handler not found. Check that a handler is registered on handle 'ws'",
                           controller => Mod, function => Func, return => nova_redact:term(RetObj)}),
            {stop, State}
    end.


run_plugins([], _Callback, #{controller_result := {stop, _} = Signal}, _State) ->
    Signal;
run_plugins([], _Callback, _ControllerState, State) ->
    {ok, State};
run_plugins([{Plugin, Options}|Tl], Callback, ControllerState, State) when is_function(Plugin) ->
    {module, Module} = erlang:fun_info(Plugin, module),
    run_plugins([{Module, Options}|Tl], Callback, ControllerState, State);
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
            ?LOG_ERROR(#{msg => ~"Plugin returned error", plugin => Module, function => Callback, reason => nova_redact:term(Reason)}),
            {stop, State}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{msg => ~"Plugin crashed", class => Class, reason => nova_redact:term(Reason),
                         stacktrace => nova_redact:stacktrace(Stacktrace)}),
            {stop, State}
    end.



