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


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


dispatch(Req = #{method := ReqMethod},
         State = #{mod := Mod, func := Func, method := Method}) when Method == '_' orelse
                                                                     Method == ReqMethod ->
    handle(Mod, Func, Req, State);
dispatch(Req, State) ->
    Req1 = cowboy_req:reply(404, Req),
    {ok, Req1, State}.




handle(Mod, Fun, Req = #{method := Method}, State) ->
    case Mod:Fun(Req) of
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
        Other ->
            logger:info("Unsupported return value from controller ~p:~p/1. Returned: ~p", [Mod, Fun, Other]),
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
            logger:warning("Could not render ~p cause it's not loaded.", [View]);
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
