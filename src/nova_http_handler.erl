%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling http requests
%%% @end

-module(nova_http_handler).

-export([
         init/2,
         handle/4
        ]).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%% Callback function from nova_handler. This is the initial call where
%% all the logic is handed.
%% @end
%%--------------------------------------------------------------------
init(Req, State = #{mod := Mod, func := Func, methods := '_'}) ->
    handle(Mod, Func, Req, State);
init(Req = #{method := ReqMethod}, State = #{mod := Mod, func := Func, methods := Methods}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            handle(Mod, Func, Req, State);
        false ->
            Req1 = nova_router:status_page(405, Req),
            {ok, Req1, State}
    end;
init(Req, State) ->
    Req1 = nova_router:status_page(404, Req),
    {ok, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(Mod, Fun, Req, State) ->
    ?LOG(debug, "Handling request for ~p:~p", [Mod, Fun]),
    Args =
        case maps:get(auth_data, State, undefined) of
            undefined ->
                [Req];
            SecObject ->
                [Req, SecObject]
        end,
    try erlang:apply(Mod, Fun, Args) of
        RetObj ->
            handle1(RetObj, Mod, Fun, Req, State)
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
          ?ERROR("Controller (~p:~p/1) failed with ~p:~p.~nStacktrace:~n~p", [Mod, Fun, Type, Reason, Stacktrace]),
          Req1 = nova_router:status_page(404, Req),
          {ok, Req1, State}
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
            Req1 = nova_router:status_page(Status, Req),
            {ok, Req1, State};
        {status, Status, _Headers} when is_integer(Status) ->
            %% TODO! Include headers
            Req1 = nova_router:status_page(Status, Req),
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
        Other ->
            Signature = erlang:element(1, Other),
            case nova_router:get_handler(Signature) of
                #{module := Handler} ->
                    try Handler:handle(Other) of
                        {continue, Result} ->
                            %% Recurse. Maybe we need something else?
                            handle1(Result, Mod, Fun, Req, State);
                        Result ->
                            Result
                    catch
                        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                        erlang:raise(Type, Reason, Stacktrace)
                    end;
                _ ->
                    erlang:throw({unknown_handler_type, Signature})
            end
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
            ?ERROR("Could not render ~p cause it's not loaded.", [View]);
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
