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
-type method() :: '_' | binary(). %% Can not have binary literals in specs
-type nova_http_state() :: #{mod := atom(),
                             func := atom(),
                             methods := [method()] | method(),
                             _ => _}.
-export_type([nova_http_state/0]).


%%--------------------------------------------------------------------
%% @doc
%% Callback function from nova_handler. This is the initial call where
%% all the logic is handed.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), State :: nova_http_state()) ->
                  {ok, Req1 :: cowboy_req:req(), State0 :: nova_http_state()}.
init(Req, State = #{mod := Mod, func := Func, methods := '_'}) ->
    {ok, StatusCode, Headers, Body, State0} = handle(Mod, Func, Req, State),
    Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
    {ok, Req1, State0};
init(Req = #{method := ReqMethod}, State = #{mod := Mod, func := Func, methods := Methods}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            {ok, StatusCode, Headers, Body, State0} = handle(Mod, Func, Req, State),
            Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
            {ok, Req1, State0};
        false ->
            {ok, StatusCode, Headers, Body, _} = nova_router:status_page(405, Req),
            Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
            {ok, Req1, State}
    end;
init(Req, State) ->
    {ok, StatusCode, Headers, Body, _} = nova_router:status_page(404, Req),
    Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
    {ok, Req1, State}.


%%--------------------------------------------------------------------
%% @doc
%% This function is exposed mostly cause we need to call it from nova_router.
%% It returns the raw handle request instead of the aggregated result returned in init/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(Mod :: atom(), Fun :: atom(), Req :: cowboy_req:req(), State :: nova_http_state()) ->
                    {ok, StatusCode :: integer(), Headers :: cowboy:http_headers(), Body :: binary(), State0 :: nova_http_state()}.
handle(Mod, Fun, Req, State) ->
    ?DEBUG("Handling request for ~p:~p", [Mod, Fun]),
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
          nova_router:status_page(404, Req)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle1(RetObj, Mod, Fun, Req = #{method := Method}, State) ->
    case RetObj of
	{json, JSON} ->
            EncodedJSON = json:encode(JSON, [binary, {maps, true}]),
	    StatusCode = case Method of
			     <<"POST">> -> 201;
			     _ -> 200
			 end,
            Headers = #{<<"content-type">> => <<"application/json">>},
            {ok, StatusCode, Headers, EncodedJSON, State};
        {json, StatusCode, Headers, JSON} ->
            EncodedJSON = json:encode(JSON, [binary, {maps, true}]),
            Headers0 = maps:merge(#{<<"content-type">> => <<"application/json">>}, Headers),
            {ok, StatusCode, Headers0, EncodedJSON, State};
        {ok, Variables} ->
            %% Derive the view from module
            ViewNameAtom = get_view_name(Mod),
            handle_view(ViewNameAtom, Variables, #{}, State);
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
            handle_view(View, Variables, Options, State);
        {status, Status} when is_integer(Status) ->
            nova_router:status_page(Status, Req);
        {status, Status, ExtraHeaders} when is_integer(Status) ->
            {ok, _StatusCode, StatusHeaders, StatusBody, _} = nova_router:status_page(Status, Req),
            Headers0 = maps:merge(ExtraHeaders, StatusHeaders),
            {ok, Status, Headers0, StatusBody, State};
        {redirect, Route} ->
            Headers = #{<<"Location">> => list_to_binary(Route)},
            {ok, 302, Headers, <<>>, State};
        {cowboy_req, CowboyReq} ->
            {ok, CowboyReq, State};
        Other ->
            ?ERROR("Unknown return object ~p returned from module: ~p function: ~p", [Other, Mod, Fun]),
            erlang:throw({unknown_handler_type, Other})
    end.

handle_view(View, Variables, Options, State) ->
    {ok, HTML} = render_dtl(View, Variables, []),
    Headers =
        case maps:get(headers, Options, undefined) of
            undefined ->
                #{<<"content-type">> => <<"text/html">>};
            UserHeaders ->
                UserHeaders
        end,
    StatusCode = maps:get(status_code, Options, 200),
    {ok, StatusCode, Headers, HTML, State}.

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
