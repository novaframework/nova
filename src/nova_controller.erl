%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2018 by Niclas Axelsson <niclas@burbas.se>

-module(nova_controller).

-export([
         init/2,
         terminate/3
        ]).


%% Websocket specific callbacks
-export([
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

-include_lib("nova/include/nova.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State = #{secure := false}) -> dispatch(Req, State);
init(Req, State = #{secure := {Mod, Func}}) ->
    case Mod:Func(Req) of
        true ->
            dispatch(Req, State);
        _ ->
            Req1 = cowboy_req:reply(401, Req),
            {ok, Req1, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.


websocket_init(State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_init, [], State).

websocket_handle(Frame, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_handle, [Frame], State).

websocket_info(Msg, State = #{mod := Mod}) ->
    handle_ws(Mod, websocket_info, [Msg], State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_ws(Mod, Func, Args, State = #{substate := Substate}) ->
    try
        case erlang:apply(Mod, Func, Args ++ [Substate]) of
            {reply, Frame, NewSubstate} ->
                {reply, Frame, State#{substate => NewSubstate}};
            {reply, Frame, NewSubstate, hibernate} ->
                {reply, Frame, State#{substate => NewSubstate}, hibernate};
            {ok, NewSubstate, hibernate} ->
                {ok, State#{substate => NewSubstate}, hibernate};
            {stop, NewSubstate} ->
                {stop, State#{substate => NewSubstate}}
        end
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
          ?ERROR("Websocket failed with ~p:~p.~nStacktrace:~n~p", [Type, Reason, Stacktrace]),
          Substate
    end.

dispatch(Req, State = #{protocol := ws}) ->
    ReqProtocols = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req),

    case maps:get(subprotocols, State, []) of
        [] when ReqProtocols == undefined ->
            %% We have not specified any procols. Client have not requested specific protocol so we accept.
            {cowboy_websocket, Req, State};
        _Subprotocols when ReqProtocols == undefined->
            %% The client have not requested any specific protocol but we requre one. Return 400.
            Req1 = cowboy_req:reply(400, Req),
            {ok, Req1, State};
        Subprotocols ->
            %% Find out if we support the subprotocol
            case find_subprotocol_match(Subprotocols, ReqProtocols, Req) of
                {ok, Req1} ->
                    {cowboy_websocket, Req1, State};
                _ ->
                    Req1 = cowboy_req:reply(400, Req),
                    {ok, Req1, State}
            end
    end;
dispatch(Req, State = #{protocol := http, mod := Mod,
                        func := Func, methods := '_'}) ->
    handle(Mod, Func, Req, State);
dispatch(Req = #{method := ReqMethod}, State = #{protocol := http, mod := Mod,
                                                 func := Func, methods := Methods}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            handle(Mod, Func, Req, State);
        false ->
            Req1 = cowboy_req:reply(404, Req),
            {ok, Req1, State}
    end;
dispatch(Req, State) ->
    %% Display a nicer page
    Req1 = cowboy_req:reply(404, Req),
    {ok, Req1, State}.




handle(Mod, Fun, Req, State) ->
    try Mod:Fun(Req) of
        RetObj ->
            handle1(RetObj, {Mod, Fun}, Req, State)
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
          ?ERROR("Controller failed with ~p:~p.~nStacktrace:~n~p", [Type, Reason, Stacktrace])
    end.

handle1(RetObj, {Mod, Fun}, Req = #{method := Method}, State) ->
    case RetObj of
	{json, JSON} ->
            EncodedJSON = jsone:encode(JSON, [undefined_as_null]),
	    StatusCode = case Method of
			     <<"POST">> -> 201;
			     _ -> 200
			 end,
            Req1 = cowboy_req:reply(StatusCode, #{
                                                  <<"content-type">> => <<"application/json">>
                                                 }, EncodedJSON, Req),
            {ok, Req1, State};
        {json, StatusCode, Headers, JSON} ->
            EncodedJSON = jsone:encode(JSON, [undefined_as_null]),
            Req1 = cowboy_req:reply(StatusCode,
				    maps:merge(#{<<"content-type">> =>
						    <<"application/json">>},
                                              Headers),
				    EncodedJSON,
				    Req),
            {ok, Req1, State};
        {ok, Variables} ->
            %% Derive the view from module
            ViewNameAtom = get_view_name(Mod),
            handle_view(ViewNameAtom, Variables, #{}, Req, State);
        {ok, Variables, Options} ->
            View =
                case maps:get(view, Options, undefined) of
                    undefined ->
                        ViewName = atom_to_list(Mod) ++ "_dtl",
                        list_to_atom(ViewName);
                    CustomView ->
                        CustomView
                end,
            handle_view(View, Variables, Options, Req, State);
        {status, Status} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, #{}, Req),
            {ok, Req1, State};
        {status, Status, Headers} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, Headers, Req),
            {ok, Req1, State};
        {cowboy_req, CowboyReq} ->
            {ok, CowboyReq, State};
        {extern_handler, Module, Function, Payload} ->
            try Module:Function(Payload, Req) of
                {external_handler, _, _, _} ->
                    ?ERROR("Infinite loop detected"),
                    Req1 = cowboy_req:reply(500, #{}, Req),
                    {ok, Req1, State};
                RetObject ->
                    handle1(RetObject, {Mod, Fun}, Req, State)
            catch
                _:_ ->
                    ?WARNING("External handler (~p:~p) failed", [Module, Function])
            end;
        Other ->
            ?WARNING("Unsupported return value from controller ~p:~p/1. Returned: ~p", [Mod, Fun, Other]),
            Req1 = cowboy_req:reply(500, #{}, Req),
            {ok, Req1, State}
    end.

handle_view(View, Variables, Options, Req, State) ->
    {ok, HTML} = render_dtl(View, Variables, []),
    Headers =
        case maps:get(headers, Options, undefined) of
            undefined ->
                #{<<"content-type">> => <<"text/html">>};
            UserHeaders ->
                UserHeaders
        end,
    StatusCode = maps:get(status_code, Options, 200),
    Req1 = cowboy_req:reply(StatusCode, Headers, HTML, Req),
    {ok, Req1, State}.


render_dtl(View, Variables, Options) ->
    case code:is_loaded(View) of
        false ->
            %% Cast a warning since the module could not be found
            ?WARNING("Could not render ~p cause it's not loaded.", [View]);
        _ ->
            View:render(Variables, Options)
    end.


get_view_name(Mod) when is_atom(Mod) ->
    StrName = get_view_name(erlang:atom_to_list(Mod)),
    erlang:list_to_atom(StrName);
get_view_name([$_, $c, $o, $n, $t, $r, $o, $l, $l, $e, $r]) ->
    "_dtl";
get_view_name([H|T]) ->
    [H|get_view_name(T)].


find_subprotocol_match([], _, _) -> not_found;
find_subprotocol_match([SupportedProtocol|Tl], ReqProtocols, Req) ->
    case lists:keymember(SupportedProtocol, 1, ReqProtocols) of
        true ->
            Req1 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                                              SupportedProtocol, Req),
            {ok, Req1};
        _ ->
            find_subprotocol_match(Tl, ReqProtocols, Req)
    end.
