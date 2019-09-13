%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling http requests
%%% @end

-module(nova_http_controller).

-export([
         check_security/2,
         init/2,
         terminate/3
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



-spec check_security(Req :: cowboy_req:req(), State :: map()) -> boolean() |
                                                                 {redirect, Route :: binary()} |
                                                                 {cowboy_req, Req1 :: cowboy_req:req()}.
-spec init(Req :: cowboy_req:req(), State :: map()) -> boolean() |
                                                       deprecated_call_result(State) when State :: map().
-spec terminate(Reason :: any(), Req :: cowboy_req:req(), State :: map()) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%% If a security module/func is defined in the route of the request that
%% module/func will be called. If the result of that is false it will
%% return a 401 to the requester. Otherwise it will continue to handle
%% the request.
%% @end
%%--------------------------------------------------------------------
check_security(_, #{secure := false}) -> true;
check_security(Req, #{secure := {Mod, Func}}) -> Mod:Func(Req).


%%--------------------------------------------------------------------
%% @doc
%% Callback function from cowboy. Checks if the user is allowed to see
%% this page (See check_security/2 function). If true then the handling
%% of the request will continue. If not it will either redirect the user,
%% return a 401 http staus or return a cowboy request object.
%% @end
%%--------------------------------------------------------------------
init(Req, State) ->
    case check_security(Req, State) of
        true ->
            dispatch(Req, State);
        false ->
            Req1 = cowboy_req:reply(401, Req),
            {ok, Req1, State};
        {redirect, Route} ->
            Req1 = cowboy_req:reply(
                     302,
                     #{<<"Location">> => list_to_binary(Route)},
                     Req
                    ),
            {ok, Req1, State};
        {cowboy_req, Req1} ->
            {ok, Req1, State}
    end.

%% TODO! Fix a proper terminate function
terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
dispatch(Req, State = #{mod := Mod, func := Func, methods := '_'}) ->
    handle(Mod, Func, Req, State);
dispatch(Req = #{method := ReqMethod}, State = #{mod := Mod, func := Func, methods := Methods}) ->
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
            handle1(RetObj, Mod, Fun, Req, State)
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
          ?ERROR("Controller (~p:~p/1) failed with ~p:~p.~nStacktrace:~n~p", [Mod, Fun, Type, Reason, Stacktrace])
    end.

handle1(RetObj, Mod, Fun, Req = #{method := Method}, State) ->
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
                    CustomView when is_atom(CustomView) ->
                        ViewName = atom_to_list(CustomView) ++ "_dtl",
                        list_to_atom(ViewName);
                    CustomView ->
                        list_to_atom(CustomView ++ "_dtl")
                end,
            handle_view(View, Variables, Options, Req, State);
        {status, Status} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, #{}, Req),
            {ok, Req1, State};
        {status, Status, Headers} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, Headers, Req),
            {ok, Req1, State};
        {redirect, Route} ->
            Req1 = cowboy_req:reply(
                     302,
                     #{<<"Location">> => list_to_binary(Route)},
                     Req
                    ),
            {ok, Req1, State};
        {cowboy_req, CowboyReq} ->
            {ok, CowboyReq, State};
        {external_handler, Module, Payload} ->
            try Module:handle(Payload, Req) of
                {external_handler, _, _, _} ->
                    ?ERROR("Infinite loop detected"),
                    Req1 = cowboy_req:reply(500, #{}, Req),
                    {ok, Req1, State};
                RetObject ->
                    handle1(RetObject, Mod, Fun, Req, State)
            catch
                ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                    ?WARNING("External handler (~p:~p) failed. ~p:~p. Stacktrace: ~p", [Module, handle, Type, Reason, Stacktrace])
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eunit functions         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-endif.
