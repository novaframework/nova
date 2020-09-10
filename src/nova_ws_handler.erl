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

-type nova_ws_state() :: #{controller_data := any(),
                           mod := atom(),
                           subprotocols := [any()],
                           nova_handler := nova_ws_handler,
                           _ := _}.

-export_type([nova_ws_state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req, State = #{mod := Mod}) ->
    case Mod:init(Req) of
        {ok, ControllerData} ->
            {cowboy_websocket, Req, State#{controller_data => ControllerData}};
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



handle_ws(Mod, Func, Args, State = #{controller_data := _ControllerData}) ->
    {ok, PrePlugins} = nova_plugin:get_plugins(pre_ws_request),
    case lists_run_while(fun({ok, State0}) ->
                                 {true, State0};
                            (X) ->
                                 X
                         end,
                         fun({_, #{module := PluginMod, options := Options}}, Ack) ->
                                 PluginMod:pre_ws_request(Ack, Options)
                         end, State, PrePlugins) of
        {Cont, State0} when Cont == ok orelse
                            Cont == break ->
            invoke_controller(Mod, Func, Args, State0);
        {stop, State0} ->
            {stop, State0};
        {error, Reason} ->
            ?ERROR("Websocket plugin (~p:~p) stopped with error ~p", [Mod, Func, Reason]),
            {stop, State}
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
            _ ->
                run_post_plugin(ControllerResult)
        end
    catch
        Type:Reasons:Stacktrace ->
            ?ERROR("Websocket failed with ~p:~p.~nStacktrace:~n~p", [Type, Reasons, Stacktrace]),
            {stop, State}
    end.

run_post_plugin(ControllerResult) ->

    %% Invoke the post plugins
    {ok, PostPlugins} = nova_plugin:get_plugins(post_ws_request),
    Hibernate = element(size(ControllerResult), ControllerResult) == hibernate,

    case lists_run_while(fun(X) when element(1, X) == ok orelse
                                     element(1, X) == reply ->
                                 {true, X};
                            (Y) ->
                                 Y
                         end,
                         fun({_, #{module := PluginMod, options := Options}}, Ack) ->
                                 Options0 = case Hibernate of
                                                true -> Options#{hibernate => true};
                                                _ -> Options
                                            end,
                                 case size(Ack) of
                                     3 ->
                                         PluginMod:post_ws_request(Ack, Options0);
                                     _ ->
                                         {ControlCode, State} = Ack,
                                         PluginMod:post_ws_request({ControlCode, [], State}, Options0)
                                 end
                         end, ControllerResult, PostPlugins) of
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




lists_run_while(_Cond, _Func, Ack, []) -> Ack;
lists_run_while(Cond, Func, Ack, [E|Tl]) ->
    Ack0 = Func(E, Ack),
    case Cond(Ack0) of
        {true, Ack1} ->
            lists_run_while(Cond, Func, Ack1, Tl);
        Error ->
            Error
    end.


-ifdef(TEST).

-endif.
