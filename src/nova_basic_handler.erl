-module(nova_basic_handler).
-export([
         handle_json/4,
         handle_ok/4,
         handle_status/4,
         handle_redirect/4,
         handle_cowboy_req/4
        ]).

-include_lib("nova/include/nova.hrl").

handle_json({json, JSON}, _, #{method := Method}, State) ->
    EncodedJSON = json:encode(JSON, [binary, maps]),
    StatusCode = case Method of
                     <<"POST">> -> 201;
                     _ -> 200
                 end,
    Headers = #{<<"content-type">> => <<"application/json">>},
    {ok, StatusCode, Headers, EncodedJSON, State};
handle_json({json, StatusCode, Headers, JSON}, _, _Req, State) ->
    EncodedJSON = json:encode(JSON, [binary, {maps, true}]),
    Headers0 = maps:merge(#{<<"content-type">> => <<"application/json">>}, Headers),
    {ok, StatusCode, Headers0, EncodedJSON, State}.


handle_ok({ok, Variables}, {Mod, _Func}, _Req, State) ->
    %% Derive the view from module
    ViewNameAtom = get_view_name(Mod),
    handle_view(ViewNameAtom, Variables, #{}, State);
handle_ok({ok, Variables, Options}, {Mod, _Func}, _Req, State) ->
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
    handle_view(View, Variables, Options, State).


handle_status({status, Status}, _ModFun, Req, _State) ->
    nova_router:status_page(Status, Req);
handle_status({status, Status, ExtraHeaders}, _ModFun, Req, State) ->
    {ok, _StatusCode, StatusHeaders, StatusBody, _} = nova_router:status_page(Status, Req),
    Headers0 = maps:merge(ExtraHeaders, StatusHeaders),
    {ok, Status, Headers0, StatusBody, State}.


handle_redirect({redirect, Route}, _ModFun, _Req, State) ->
    Headers = #{<<"Location">> => list_to_binary(Route)},
    {ok, 302, Headers, <<>>, State}.

handle_cowboy_req({cowboy_req, CowboyReq}, _ModFun, _Req, State) ->
    {ok, CowboyReq, State}.



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



%%%===================================================================
%%% Internal functions
%%%===================================================================


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
