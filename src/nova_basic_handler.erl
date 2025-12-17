-module(nova_basic_handler).
-export([
         handle_json/3,
         handle_ok/3,
         handle_view/3,
         handle_status/3,
         handle_redirect/3,
         handle_sendfile/3,
         handle_websocket/3,
         handle_ws/2
        ]).

-include_lib("kernel/include/logger.hrl").

-type erlydtl_vars() :: map() | [{Key :: atom() | binary() | string(), Value :: any()}].



%%--------------------------------------------------------------------
%% @doc
%% Handler for JSON. It can take one of three different return objects:
%%
%%   {json, JSON :: map()} returns the JSON encoded to the user.
%%     If the operation was a POST the HTTP-status code will be 201, otherwise
%%     200.
%%
%%   {json, StatusCode :: integer(), Headers :: map(), JSON :: map()} Same
%%     operation as the above except you can set custom status code and custom
%%     headers.
%%
%%   {json, StatusCode :: integer(), Headers :: map(), Req0 :: cowboy_req:req(), JSON :: map()}
%%     This is the same as the above but you can also return the request object. This is particularly
%%     useful if you want to set cookies or other headers that are not supported by the handler.
%% @end
%%--------------------------------------------------------------------
-spec handle_json({json, JSON :: map()} | {json, StatusCode :: integer(), Headers :: map(), JSON :: map()} |
                  {json, StatusCode :: integer(), Headers :: map(), JSON :: map(), Req0 :: cowboy_req:req()},
                  Callback :: function(), Req :: cowboy_req:req()) -> {ok, State :: cowboy_req:req()}.
handle_json({json, StatusCode, Headers, Req0, JSON}, Callback, _Req) ->
    handle_json({json, StatusCode, Headers, JSON}, Callback, Req0);
handle_json({json, StatusCode, Headers, JSON}, _Callback, Req) ->
    JsonLib = nova:get_env(json_lib, thoas),
    EncodedJSON = JsonLib:encode(JSON),
    Headers0 = maps:merge(#{<<"content-type">> => <<"application/json">>}, Headers),
    Req0 = cowboy_req:set_resp_headers(Headers0, Req),
    Req1 = cowboy_req:set_resp_body(EncodedJSON, Req0),
    Req2 = Req1#{resp_status_code => StatusCode},
    {ok, Req2};
handle_json({json, JSON}, Callback, Req = #{method := Method}) ->
    case Method of
        <<"POST">> ->
            handle_json({json, 201, #{}, JSON}, Callback, Req);
        _ ->
            handle_json({json, 200, #{}, JSON}, Callback, Req)
    end.



%%--------------------------------------------------------------------
%% @doc
%% Handler for regular views. This will render a template with given variables.
%% If not another view is specified in options a view that corresponds to the controller will be
%% rendered. The first element of the returned tuple could be either ok or view - they are
%% identical in their functionality.
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
%% The tuple can have three different forms:
%%
%% {ok, Variables} - This will render the default view with the given variables
%%
%% {ok, Variables, Options} - This will render the default view with the given variables and options
%%   Options can be specified as follows:
%%
%%   - view - Specifies if another view should be rendered instead of default one
%%
%%   - headers - Custom headers
%%
%% {ok, Variables, Options, Req0} - This will render the default view with the given variables and options
%%   but also takes a request object. This is useful if you want to set cookies or other headers that are not
%%   supported by the handler.
%% @end
%%--------------------------------------------------------------------
-spec handle_ok({ok, Variables :: erlydtl_vars()} | {ok, Variables :: erlydtl_vars(), Options :: map()},
                Callback :: function(), Req :: cowboy_req:req()) -> {ok, cowboy_req:req()}.
handle_ok({ok, Variables}, Callback, Req) ->
    %% Derive the view from module
    {module, Module} = erlang:fun_info(Callback, module),
    ViewNameAtom = get_view_name(Module),
    handle_view(ViewNameAtom, Variables, #{}, Req);
handle_ok({ok, Variables, Options}, Callback, Req) ->
    {module, Module} = erlang:fun_info(Callback, module),
    View =
        case maps:get(view, Options, undefined) of
            undefined ->
                get_view_name(Module);
            CustomView when is_atom(CustomView) ->
                ViewName = atom_to_list(CustomView) ++ "_dtl",
                list_to_atom(ViewName);
            CustomView ->
                list_to_atom(CustomView ++ "_dtl")
        end,
    handle_view(View, Variables, Options, Req);
handle_ok({ok, Variables, Options, Req0}, Callback, _Req) ->
    handle_ok({ok, Variables, Options}, Callback, Req0).



%%--------------------------------------------------------------------
%% @doc
%% Handler for regular views and uses the ok-handler. For more info see
%% handle_ok/3.
%% @end
%%--------------------------------------------------------------------
handle_view({view, Variables}, Callback, Req) ->
    handle_ok({ok, Variables}, Callback, Req);
handle_view({view, Variables, Options}, Callback, Req) ->
    handle_ok({ok, Variables, Options}, Callback, Req);
handle_view({view, Variables, Options, Req0}, Callback, _Req) ->
    handle_ok({ok, Variables, Options}, Callback, Req0).

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
%% a map. If it's a map it will be considered a JSON-structure and encoded.
%%
%% And if you want to modulate the request object you can send it in as the fifth argument.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_status({status, StatusCode :: integer()} |
                    {status, StatusCode :: integer(), ExtraHeaders :: map()} |
                    {status, StatusCode :: integer(), ExtraHeaders :: map(), Body :: binary() | map()},
                    Callback :: function(), Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()}.
handle_status({status, Status, ExtraHeaders, JSON, Req0}, Callback, _Req) ->
    handle_status({status, Status, ExtraHeaders, JSON}, Callback, Req0);
handle_status({status, Status, ExtraHeaders, JSON}, _Callback, Req) when is_map(JSON) ->
    %% We do not need to render a status page since we just return a JSON structure
    JsonLib = nova:get_env(json_lib, thoas),
    Headers0 = maps:merge(#{<<"content-type">> => <<"application/json">>}, ExtraHeaders),
    Req0 = cowboy_req:set_resp_headers(Headers0, Req),
    Req1 = Req0#{resp_status_code => Status},
    JSONStr = JsonLib:encode(JSON),
    Req2 = cowboy_req:set_resp_body(JSONStr, Req1),
    {ok, Req2};
handle_status({status, Status, ExtraHeaders, Body}, _Callback, Req) ->
    %% Body is a binary - just send it out
    Req0 = cowboy_req:set_resp_headers(ExtraHeaders, Req),
    Req1 = Req0#{resp_status_code => Status},
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {ok, Req2};
handle_status({status, Status, ExtraHeaders}, _Callback, Req) ->
    Req0 = cowboy_req:set_resp_headers(ExtraHeaders, Req),
    Req1 = Req0#{resp_status_code => Status},
    {ok, Req2, _Env} =  nova_router:render_status_page(Status, #{}, Req1),
    {ok, Req2};
handle_status({status, Status}, Callback, State) when is_integer(Status) ->
    handle_status({status, Status, #{}}, Callback, State).


%%--------------------------------------------------------------------
%% @doc
%% Handles redirects. This will return a 302-status code with a location given
%% by the user. Something like {redirect, "/login"} will send a
%% 302 with location set to "/login"
%%
%% Optionally you can attach the request object as the third argument. This is useful
%% if you want to set cookies or other headers that are not supported by the handler.
%% @end
%%-----------------------------------------------------------------
-spec handle_redirect({redirect, Route :: list()|binary()}, Callback :: function(),
                      Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()}.
handle_redirect({redirect, Route, Req0}, Callback, _Req) ->
    handle_redirect({redirect, Route}, Callback, Req0);
handle_redirect({redirect, Route}, Callback, Req) when is_list(Route) ->
    handle_redirect({redirect, list_to_binary(Route)}, Callback, Req);
handle_redirect({redirect, Route}, _Callback, Req) ->
    Headers = #{<<"location">> => Route},
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    Req1 = Req0#{resp_status_code => 302},
    {ok, Req1}.

%%--------------------------------------------------------------------
%% @doc
%% Handles sendfile.
%%
%% The tuple have the following structure:
%% {sendfile, StatusCode, Headers, {Offset, Length, Path}, Mime}
%%
%% - sendfile is the atom that tells the handler to send a file.
%%
%% - StatusCode is the HTTP status code that should be returned.
%%
%% - Headers is a map with headers that should be sent along with the file.
%%
%% - Offset is the offset in the file where the sending should start.
%%
%% - Length is the length of the file that should be sent.
%%
%% - Path is the path to the file that should be sent.
%%
%% - Mime is the mime-type of the file.
%%
%%
%% Optionally a sixth element can be added to the tuple. This is the request object. This is useful
%% if you want to set cookies or other headers that are not supported by the handler.
%% @end
%%-----------------------------------------------------------------
-spec handle_sendfile({sendfile, StatusCode :: integer(), Headers :: map(), {Offset :: integer(),
                                                                             Length :: integer(),
                                                                             Path :: list()}, Mime :: binary()},
                  Callback :: function(), Req) -> {ok, Req} when Req :: cowboy_req:req().
handle_sendfile({sendfile, StatusCode, Headers, FileInfo, Mime, Req0}, Callback, _Req) ->
    handle_sendfile({sendfile, StatusCode, Headers, FileInfo, Mime}, Callback, Req0);
handle_sendfile({sendfile, StatusCode, Headers, {Offset, Length, Path}, Mime}, _Callback, Req) ->
    Headers0 = maps:merge(#{<<"content-type">> => Mime}, Headers),
    Req0 = cowboy_req:set_resp_headers(Headers0, Req),
    Req1 = cowboy_req:set_resp_body({sendfile, Offset, Length, Path}, Req0),
    Req2 = Req1#{resp_status_code => StatusCode},
    {ok, Req2}.


%%--------------------------------------------------------------------
%% @doc
%% Handles upgrading to websocket. This is a special handler in regards to
%% arguments. The tuple-object for websocket only takes two arguments; What the initial state
%% of the websocket handler should be and the request object.
%% @end
%%-----------------------------------------------------------------
-spec handle_websocket({websocket, ControllerData :: any()}, Callback :: function(), Req :: cowboy_req:req()) ->
                              {ok, Req :: cowboy_req:req()}.
handle_websocket({websocket, ControllerData, Req0}, Callback, _Req) ->
    handle_websocket({websocket, ControllerData}, Callback, Req0);
handle_websocket({websocket, ControllerData}, Callback, Req) ->
    {module, Module} = erlang:fun_info(Callback, module),
    case Module:init(ControllerData) of
        {ok, NewControllerData} ->
            {cowboy_websocket, Req#{controller_data => NewControllerData}, #{}};
        Error ->
            ?LOG_ERROR(#{msg => <<"Handler returned unsupported result">>, handler => Module, return_obj => Error}),
            %% Render 500
            {ok, Req}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles basic websocket operations. This is a special handler in regards to
%% arguments. Handlers for websocket only takes two arguments; What the controller
%% returned and the state. And the handler should return what cowboy expects.
%%
%% Example of a valid return value is {reply, Frame, State}
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
    %% Erlang's code server will auto-load modules on first call
    %% Try to render and catch cases where module or function doesn't exist
    try View:render(Variables, Options) of
        Result -> Result
    catch
        error:undef:Stacktrace ->
            %% Check if the error is specifically from View:render/2
            case Stacktrace of
                [{View, render, 2, _} | _] ->
                    %% Module doesn't exist or render/2 not exported
                    ?LOG_ERROR(#{msg => <<"Nova could not render template">>, 
                                template => View, reason => module_not_found}),
                    throw({404, {template_not_found, View}});
                _ ->
                    %% undef error from somewhere else, re-raise it
                    erlang:raise(error, undef, Stacktrace)
            end
    end.


get_view_name({Mod, _Opts}) -> get_view_name(Mod);
get_view_name(Mod) when is_atom(Mod) ->
    StrName = get_view_name(erlang:atom_to_list(Mod)),
    erlang:list_to_atom(StrName);
get_view_name([$_, $c, $o, $n, $t, $r, $o, $l, $l, $e, $r]) ->
    "_dtl";
get_view_name([H|T]) ->
    [H|get_view_name(T)].
