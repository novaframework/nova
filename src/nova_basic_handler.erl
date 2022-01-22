-module(nova_basic_handler).
-export([
         handle_json/3,
         handle_ok/3,
         handle_status/3,
         handle_redirect/3,
         handle_sendfile/3,
         handle_websocket/3,
         handle_ws/2
        ]).

-include_lib("kernel/include/logger.hrl").

-type mod_fun() :: {Module :: atom(), Function :: atom()} | undefined.
-type erlydtl_vars() :: map() | [{Key :: atom() | binary() | string(), Value :: any()}].



%%--------------------------------------------------------------------
%% @doc
%% Handler for JSON. It takes two different return objects:
%%
%%   {json, JSON :: map()} returns the JSON encoded to the user.
%%     If the operation was a POST the HTTP-status code will be 201, otherwise
%%     200.
%%
%%   {json, StatusCode :: integer(), Headers :: map(), JSON :: map()} Same
%%     operation as the above except you can set custom status code and custom
%%     headers.
%% @end
%%--------------------------------------------------------------------
-spec handle_json({json, JSON :: map()} | {json, StatusCode :: integer(), Headers :: map(), JSON :: map()},
                  ModFun :: mod_fun(), Req :: cowboy_req:req()) -> {ok, State :: cowboy_req:req()} |
                                                                   {Module :: atom(), Req :: cowboy_req:req()}.
handle_json({json, StatusCode, Headers, JSON}, _ModFun, Req) ->
    JsonLib = nova:get_env(json_lib, thoas),
    EncodedJSON = erlang:apply(JsonLib, encode, [JSON]),
    Headers0 = maps:merge(#{<<"content-type">> => <<"application/json">>}, Headers),
    Req0 = cowboy_req:set_resp_headers(Headers0, Req),
    Req1 = cowboy_req:set_resp_body(EncodedJSON, Req0),
    Req2 = Req1#{resp_status_code => StatusCode},
    {ok, Req2};
handle_json({json, JSON}, ModFun, Req = #{method := Method}) ->
    case Method of
        <<"POST">> ->
            handle_json({json, 201, #{}, JSON}, ModFun, Req);
        _ ->
            handle_json({json, 200, #{}, JSON}, ModFun, Req)
    end.



%%--------------------------------------------------------------------
%% @doc
%% Handler for regular views. This will render a template with given variables.
%% If not another view is specified in options a view that corresponds to the controller will be
%% rendered.
%%
%%
%% -module(my_first_controller).
%% -compile(export_all).
%%
%% my_function(_Req) ->
%%    {ok, []}.
%%
%% The example above will then render the view named 'app_main.dtl'
%%
%% Options can be specified as follows:
%%
%% - view - Specifies if another view should be rendered instead of default one
%%
%% - headers - Custom headers
%% @end
%%--------------------------------------------------------------------
-spec handle_ok({ok, Variables :: erlydtl_vars()} | {ok, Variables :: erlydtl_vars(), Options :: map()},
                ModFun :: mod_fun(), Req :: cowboy_req:req()) -> {ok, cowboy_req:req()}.
handle_ok({ok, Variables}, {Mod, _Func}, Req) ->
    %% Derive the view from module
    ViewNameAtom = get_view_name(Mod),
    handle_view(ViewNameAtom, Variables, #{}, Req);
handle_ok({ok, Variables, Options}, {Mod, _Func}, Req) ->
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
    handle_view(View, Variables, Options, Req).


%%--------------------------------------------------------------------
%% @doc
%% Handler for returning http status codes. There's three different ways one can
%% return status code. The most basic case is {status, Status} where Status is
%% the code that should be returned.
%%
%% If there's a need for additional headers to be sent along with the http code one can specify
%% a third argument that is a map with header-fields.
%%
%% One can also send in a body as a fourth argument in the tuple. It can either be a binary or
%% a map. If it's a map it will be concidered a JSON-structure and encoded.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_status({status, StatusCode :: integer()} |
                    {status, StatusCode :: integer(), ExtraHeaders :: map()} |
                    {status, StatusCode :: integer(), ExtraHeaders :: map(), Body :: binary() | map()},
                    ModFun :: mod_fun(), Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()}.
handle_status({status, Status, ExtraHeaders, JSON}, _ModFun, Req) when is_map(JSON) ->
    %% We do not need to render a status page since we just return a JSON structure
    JsonLib = nova:get_env(json_lib, thoas),
    Headers0 = maps:merge(#{<<"content-type">> => <<"application/json">>}, ExtraHeaders),
    Req0 = cowboy_req:set_resp_headers(Headers0, Req),
    Req1 = Req0#{resp_status_code => Status},
    JSONStr = erlang:apply(JsonLib, encode, [JSON]),
    Req2 = cowboy_req:set_resp_body(JSONStr, Req1),
    {ok, Req2};
handle_status({status, Status, ExtraHeaders, Body}, _ModFun, Req) ->
    %% Body is a binary - just send it out
    Req0 = cowboy_req:set_resp_headers(ExtraHeaders, Req),
    Req1 = Req0#{resp_status_code => Status},
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {ok, Req2};
handle_status({status, Status, ExtraHeaders}, _ModFun, Req) ->
    Req0 = cowboy_req:set_resp_headers(ExtraHeaders, Req),
    Req1 = Req0#{resp_status_code => Status},
    {ok, Req2, _Env} =  nova_router:render_status_page(Status, #{}, Req1),
    {ok, Req2};
handle_status({status, Status}, ModFun, State) when is_integer(Status) ->
    handle_status({status, Status, #{}}, ModFun, State).


%%--------------------------------------------------------------------
%% @doc
%% Handles redirects. This will return a 302-status code with a location given
%% by the user. Something like {redirect, "/login"} will send a
%% 302 with location set to "/login"
%% @end
%%-----------------------------------------------------------------
-spec handle_redirect({redirect, Route :: list()|binary()}, ModFun :: mod_fun(),
                      Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()}.
handle_redirect({redirect, Route}, ModFun, Req) when is_list(Route) ->
    handle_redirect({redirect, list_to_binary(Route)}, ModFun, Req);
handle_redirect({redirect, Route}, _ModFun, Req) ->
    Headers = #{<<"Location">> => Route},
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    Req1 = Req0#{resp_status_code => 302},
    {ok, Req1}.

%%--------------------------------------------------------------------
%% @doc
%% Handles sendfile.
%% @end
%%-----------------------------------------------------------------
-spec handle_sendfile({sendfile, StatusCode :: integer(), Headers :: map(), {Offset :: integer(),
                                                                             Length :: integer(),
                                                                             Path :: list()}, Mime :: binary()},
                  ModFun :: mod_fun(), Req) -> {ok, Req} when Req :: cowboy_req:req().
handle_sendfile({sendfile, StatusCode, Headers, {Offset, Length, Path}, Mime}, _ModFun, Req) ->
    Headers0 = maps:merge(#{<<"content-type">> => Mime}, Headers),
    Req0 = cowboy_req:set_resp_headers(Headers0, Req),
    Req1 = cowboy_req:set_resp_body({sendfile, Offset, Length, Path}, Req0),
    Req2 = Req1#{resp_status_code => StatusCode},
    {ok, Req2}.


%%--------------------------------------------------------------------
%% @doc
%% Handles upgrading to websocket
%% @end
%%-----------------------------------------------------------------
-spec handle_websocket({websocket, ControllerData :: any()}, ModFun :: mod_fun(), Req :: cowboy_req:req()) ->
                              {ok, Req :: cowboy_req:req()}.
handle_websocket({websocket, ControllerData}, {Module, _Fun}, Req) ->
    case Module:init(ControllerData) of
        {ok, NewControllerData} ->
            {cowboy_websocket, Req#{controller_data => NewControllerData}, #{}};
        Error ->
            logger:error(#{msg => "Handler returned unsupported result", handler => Module, return_obj => Error}),
            %% Render 500
            {ok, Req}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles basic websocket operations. This is a special handler in regards to
%% arguments. Handlers for websocket only takes two arguments; What the controller
%% returned and the state. And the handler should return what cowboy expects.
%%
%% Example of a valid return value is `{reply, Frame, State}`
%% @end
%%-----------------------------------------------------------------
handle_ws({reply, Frame, NewControllerData}, State = #{commands := Commands}) ->
    State#{controller_data => NewControllerData,
           commands => [Frame|Commands]};
handle_ws({reply, Frame, NewControllerData, hibernate}, State = #{commands := Commands}) ->
    State#{controller_data => NewControllerData,
           commands => [Frame|Commands],
           hibernate => true};
handle_ws({ok, NewControllerData}, State) ->
    State#{controller_data => NewControllerData};
handle_ws({ok, NewControllerData, hibernate}, State) ->
    State#{controller_data => NewControllerData,
          hibernate => true};
handle_ws({stop, NewControllerData}, State) ->
    {stop, State#{controller_data => NewControllerData}};
handle_ws(ok, State) ->
    State.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_view(View, Variables, Options, Req) ->
    {ok, HTML} = render_dtl(View, Variables, []),
    Headers =
        case maps:get(headers, Options, undefined) of
            undefined ->
                #{<<"content-type">> => <<"text/html">>};
            UserHeaders ->
                UserHeaders
        end,
    StatusCode = maps:get(status_code, Options, 200),
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    Req1 = cowboy_req:set_resp_body(HTML, Req0),
    Req2 = Req1#{resp_status_code => StatusCode},
    {ok, Req2}.

render_dtl(View, Variables, Options) ->
    case code:is_loaded(View) of
        false ->
            case code:load_file(View) of
                {error, Reason} ->
                    %% Cast a warning since the module could not be found
                    logger:error(#{msg => "Nova could not render template", template => View, reason => Reason}),
                    throw({error, template_not_found});
                _ ->
                    View:render(Variables, Options)
            end;
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
