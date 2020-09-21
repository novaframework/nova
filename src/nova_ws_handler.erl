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
init(Req, State = #{mod := Mod, controller_data := ControllerData}) ->
    %% Call the http-handler in order to correctly handle potential plugins for the http-request
    ControllerData0 = ControllerData#{req => Req},
    case Mod:init(ControllerData0) of
        {ok, NewControllerData} ->
            {cowboy_websocket, Req, State#{controller_data => NewControllerData}};
        Error ->
            ?ERROR("Websocket handler ~p returned unkown result ~p", [Mod, Error]),
            Req1 = cowboy_req:reply(500, Req),
            {ok, Req1, State}
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
    case pre_process(State, PrePlugins) of
        {break, State0} ->
            invoke_controller(Mod, Func, Args, State0);
        {stop, State0} ->
            {stop, State0};
        {error, Reason} ->
            ?ERROR("Websocket plugin (~p:~p) stopped with error ~p", [Mod, Func, Reason]),
            {stop, State};
        State0 ->
            invoke_controller(Mod, Func, Args, State0)
    end;
handle_ws(Mod, Func, Args, State) ->
    handle_ws(Mod, Func, Args, State#{controller_data => #{}}).

pre_process(State, []) ->
    State;
pre_process(State, [{_, #{module := PluginMod, options := Options}}|T]) ->
    case PluginMod:pre_ws_request(State, Options) of
        {ok, State0} ->
            pre_process(State0, T);
        Error ->
            Error
    end;
pre_process(State, [_|T]) ->
    pre_process(State, T).


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
            _ ->
                {ok, PostPlugins} = nova_plugin:get_plugins(post_ws_request),
                run_post_plugin(ControllerResult, PostPlugins)
        end
    catch
        Type:Reasons:Stacktrace ->
            ?ERROR("Websocket failed with ~p:~p.~nStacktrace:~n~p", [Type, Reasons, Stacktrace]),
            {stop, State}
    end.

run_post_plugin(ControllerResult, []) ->
    ControllerResult;
run_post_plugin(ControllerResult, PostPlugins) ->
    Hibernate = element(size(ControllerResult), ControllerResult) == hibernate,
    case post_process(ControllerResult, Hibernate, PostPlugins) of
        %% Returning {ok, ...} is the same as {break, ...}
        {OkOrBreake, Frames, State0, Options} when OkOrBreake == reply orelse
                                                   OkOrBreake == break ->
            %% There must be a nicer way to achive the following case but I can't figure it out now.
            %% TODO! Rewrite into something better
            case maps:get(hibernate, Options, false) of
                true ->
                    {reply, Frames, State0, hibernate};
                _ ->
                    {reply, Frames, State0}
            end;
        {OkOrBreake, State0, Options} when OkOrBreake == ok orelse
                                           OkOrBreake == break ->
            %% TODO! Rewrite this case aswell.
            case maps:get(hibernate, Options, false) of
                true ->
                    {ok, State0, hibernate};
                _ ->
                    {ok, State0}
            end;
        {stop, State0} ->
            {stop, State0};
        {error, Reason} ->
            %% Just output the error message. Maybe include information about which plugin that
            %% returned the error?
            ?ERROR("Post-plugin exited with reason ~p. Closing connection.", [Reason]),
            {stop, no_state}
    end.

post_process(ControllerResult, _, []) ->
    ControllerResult;
post_process(ControllerResult, Hibernate, [{_, #{module := PluginMod, options := Options}} | T]) ->
    Options0 = case Hibernate of
                   true ->
                       Options#{hibernate => true};
                   _ ->
                       Options
               end,
    Result = case size(ControllerResult) of
                 3 ->
                     PluginMod:post_ws_request(ControllerResult, Options0);
                 _ ->
                     {ControlCode, State} = ControllerResult,
                     PluginMod:post_ws_request({ControlCode, [], State}, Options0)
             end,
    case Result of
        X when element(1, X) == ok orelse
               element(1, X) == reply ->
            post_process(X, Hibernate, T);
        Error ->
            Error
    end.

-ifdef(TEST).

-endif.
