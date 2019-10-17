%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling http requests
%%% @end

-module(nova_http_handler).

-export([
         execute/2
        ]).

-behaviour(cowboy_middleware).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%% Callback function from cowboy. Checks if the user is allowed to see
%% this page (See check_security/2 function). If true then the handling
%% of the request will continue. If not it will either redirect the user,
%% return a 401 http staus or return a cowboy request object.
%% @end
%%--------------------------------------------------------------------
execute(Req, Env = #{handler_opts := #{mod := Mod, func := Func, methods := '_'}}) ->
    handle(Mod, Func, Req, Env);
execute(Req = #{method := ReqMethod}, Env = #{handler_opts := #{mod := Mod, func := Func, methods := Methods}}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            handle(Mod, Func, Req, Env);
        false ->
            Req1 = nova_router:status_page(401, Req),
            {stop, Req1}
    end;
execute(Req, Env = #{handler := nova_ws_controller}) ->
    %% This is a websocket request
    nova_ws_controller:init(Req, Env);
execute(Req, Env) ->
    %% Display a nicer page
    Req1 = cowboy_req:reply(404, Req),
    {stop, Req1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(Mod, Fun, Req, Env = #{handler_opts := State}) ->
    Args =
        case maps:get(auth_data, State, undefined) of
            undefined ->
                [Req];
            SecObject ->
                [Req, SecObject]
        end,
    try erlang:apply(Mod, Fun, Args) of
        RetObj ->
            handle1(RetObj, Mod, Fun, Req, Env)
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
          ?ERROR("Controller (~p:~p/1) failed with ~p:~p.~nStacktrace:~n~p", [Mod, Fun, Type, Reason, Stacktrace]),
          Req1 = nova_router:status_page(404, Req),
          {ok, Req1, Env}
    end.

handle1(RetObj, Mod, Fun, Req = #{method := Method}, Env = #{handler_opts := State}) ->
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
            {ok, Req1, Env};
        {json, StatusCode, Headers, JSON} ->
            EncodedJSON = jsone:encode(JSON, [undefined_as_null]),
            Req1 = cowboy_req:reply(StatusCode,
				    maps:merge(#{<<"content-type">> =>
						    <<"application/json">>},
                                              Headers),
				    EncodedJSON,
				    Req),
            {ok, Req1, Env};
        {ok, Variables} ->
            %% Derive the view from module
            ViewNameAtom = get_view_name(Mod),
            handle_view(ViewNameAtom, Variables, #{}, Req, Env);
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
            handle_view(View, Variables, Options, Req, Env);
        {status, Status} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, #{}, Req),
            {ok, Req1, Env};
        {status, Status, Headers} when is_integer(Status) ->
            Req1 = cowboy_req:reply(Status, Headers, Req),
            {ok, Req1, Env};
        {redirect, Route} ->
            Req1 = cowboy_req:reply(
                     302,
                     #{<<"Location">> => list_to_binary(Route)},
                     Req
                    ),
            {stop, Req1};
        {cowboy_req, CowboyReq} ->
            {ok, CowboyReq, Env#{handler_opts := CowboyReq}};
        {websocket, WebsocketState} ->
            cowboy_websocket:upgrade(Req, Env, nova_ws_controller, WebsocketState);
        {websocket, WebsocketState, Options} ->
            cowboy_websocket:upgrade(Req, Env, nova_ws_controller, WebsocketState, Options);
        Other ->
            Signature = erlang:element(1, Other),
            Handlers = maps:get(handlers, State, []),
            case proplists:get_value(Signature, Handlers) of
                #{module := Handler} ->
                    try Handler:handle(Other) of
                        Result ->
                            %% Recurse. Maybe we need something else?
                            handle1(Result, Mod, Fun, Req, Env)
                    catch
                        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                        erlang:raise(Type, Reason, Stacktrace)
                    end;
                _ ->
                    erlang:throw({unknown_return_type, Other})
            end
    end.

handle_view(View, Variables, Options, Req, Env) ->
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
    {ok, Req1, Env}.


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
