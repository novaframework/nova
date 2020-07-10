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
-type nova_http_state() :: #{mod := atom(),
                             func := atom(),
                             methods := [binary()] | '_',
                             _ => _}.
-export_type([nova_http_state/0]).


%%--------------------------------------------------------------------
%% @doc
%% Callback function from nova_handler. This is the initial call where
%% all the logic is handed.
%%
%% @TODO This function needs to be refactored soon. It's way to cluttered
%% and I really dislike how the case's are nested.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), State :: nova_http_state()) ->
                  {ok, Req0 :: cowboy_req:req(), State0 :: nova_http_state()}.
init(Req, State) ->
    case run_plugins(pre_request, Req, State) of
        {ok, Req0, State0} ->
            %% Call the controller
            {ok, Req1, State1} = invoke_controller(Req0, State0),
            %% Invoke post_request plugins
            case run_plugins(post_request, Req1, State1) of
                {error, Req2} ->
                    {ok, Req2, State1};
                Reply ->
                    Reply
            end;
        {error, Req0} ->
            {ok, Req0, State}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% First checks so that method of the request is supported by the controller.
%% If not a 405 page is returned. '_' is used as a catch-all when defining the route.
%% @end
%%--------------------------------------------------------------------
%% Methods for this path is defined as a 'catch_all' so just continue executing
invoke_controller(Req, State = #{mod := Mod, func := Func, methods := '_'}) ->
    {ok, StatusCode, Headers, Body, State0} = handle(Mod, Func, Req, State),
    Req1 = cowboy_req:reply(StatusCode, Headers, Body, Req),
    {ok, Req1, State0};

%% We have a list of methods we allow, so check if they match the one requested before continuing
invoke_controller(Req = #{method := ReqMethod}, State = #{methods := Methods}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            invoke_controller(Req, State#{methods := '_'});
        false ->
            Req0 = render_page(405, Req),
            {ok, Req0, State}
    end.


%%--------------------------------------------------------------------
%% @doc
%% This function is exposed mostly cause we need to call it from nova_router.
%% It returns the raw handle request instead of the aggregated result returned in init/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(Mod :: atom(), Fun :: atom(), Req :: cowboy_req:req(), State :: nova_http_state()) ->
                    {ok, StatusCode :: integer(), Headers :: cowboy:http_headers(), Body :: binary(),
                     State0 :: nova_http_state()}.
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
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    Callback(RetObj, {Mod, Fun}, Req, State);
                _ ->
                    ?ERROR("Unknown return object ~p returned from module: ~p function: ~p", [RetObj, Mod, Fun]),
                    {ok, render_page(500, Req, {Mod, Fun, 1, "Unknown return object ~p returned from module: ~s:~s/1", [RetObj, Mod, Fun]}), State}
            end
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Controller (~p:~p/1) failed with ~p:~p.~nStacktrace:~n~p",
                   [Mod, Fun, Type, Reason, Stacktrace]),
            {ok, render_page(500, Req, {Mod, Fun, 1, Type, Reason, Stacktrace}), State}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% Checks if a page is defined in nova_router for the given status code.
%% If it is that page will be rendered and a new request will be returned
%% with the rendered body. Otherwise a request with a blank body will be returned.
%% @end
%%--------------------------------------------------------------------
-spec render_page(StatusCode :: integer(), Req :: cowboy_req:req()) -> cowboy_req:req().
render_page(StatusCode, Req) ->
    {ok, StatusCode, Headers, Body, _} =
        case nova_router:status_page(StatusCode, Req) of
            {error, _} ->
                {ok, StatusCode, #{}, <<>>, Req};
            FoundPage ->
                FoundPage
        end,
    cowboy_req:reply(StatusCode, Headers, Body, Req).


%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% Renders a page with additional arguments. The returning cowboy request
%% does always contain a body.
%% @end
%%--------------------------------------------------------------------
-spec render_page(StatusCode :: integer(), Req :: cowboy_req:req(), {Module :: atom(), Function :: atom(), Arity :: integer(), Format :: list(), Args :: list()} |
                  {Module :: atom(), Function :: atom(), Arity :: integer(), Type :: atom(), Reason :: atom(), Stacktrace :: any()}) ->
                         cowboy_req:req().
render_page(500, Req, {Module, Function, Arity, Format, Args}) ->
    {ok, _, Headers, HTML, _} =
        case nova_router:status_page(500, Req) of
            {error, not_found} ->
                Msg = lists:flatten(io_lib:format(Format, Args)),
                DevMode = nova:get_env(dev_mode, false),
                {ok, HTML0} = nova_internal_error_dtl:render([{module, Module},
                                                              {function, Function},
                                                              {arity, Arity},
                                                              {message, Msg},
                                                              {dev_mode, DevMode}], []),
                {ok, 500, #{}, HTML0, undefined};
            Page ->
                Page
        end,
    cowboy_req:reply(500, Headers, HTML, Req);
render_page(500, Req, {Module, Function, Arity, Type, Reason, Stacktrace}) ->
    {ok, _, Headers, HTML, _} =
        case nova_router:status_page(500, Req) of
            {error, not_found} ->
                DevMode = nova:get_env(dev_mode, false),
                {ok, HTML0} = nova_internal_error_dtl:render([{module, Module},
                                                              {function, Function},
                                                              {arity, Arity},
                                                              {type, Type},
                                                              {reason, Reason},
                                                              {stacktrace, Stacktrace},
                                                              {dev_mode, DevMode}], []),
                {ok, 500, #{}, HTML0, undefined};
            Page ->
                Page
        end,
    cowboy_req:reply(500, Headers, HTML, Req).


%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% Run a list of plugins with a given callback function. Each plugin is called
%% with Req and State in its argument. It can either return {ok, Req0, State0} which
%% will run the next plugin in the list until it's empty and the result is returned.
%% If {stop, Req, State} is returned it will stop executing the plugin-chain and return
%% normally. If {error, Reason} is returned a 500-status page will be rendered and returned.
%% @end
%%--------------------------------------------------------------------
run_plugins(ReqType, Req, State) when ReqType == pre_request orelse
                                      ReqType == post_request ->
    {ok, Plugins} = nova_plugin:get_plugins(ReqType, http),
    run_plugins(Plugins, ReqType, Req, State).

-spec run_plugins(Plugins :: list(), CallbackFun :: pre_request | post_request, Req :: cowboy_req:req(), State :: nova_http_state()) ->
                         {ok, Req0 :: cowboy_req:req(), State0 :: nova_http_state()} |
                         {error, Req0 :: cowboy_req:req()}.
run_plugins([], _CallbackFun, Req, State) ->
    {ok, Req, State};
run_plugins([Plugin|Tl], CallbackFun, Req, State) when CallbackFun == pre_request orelse
                                                       CallbackFun == post_request ->
    try Plugin:CallbackFun(Req, State) of
        {ok, Req0, State0} ->
            run_plugins(Tl, CallbackFun, Req0, State0);
        {stop, Req0, State0} ->
            {ok, Req0, State0};
        {error, Reason} ->
            ?ERROR("Pre handler returned error with reason ~p", [Reason]),
            Req0 = render_page(500, Req, {?MODULE, run_plugins, 4, "Error when running pre-plugins. Plugin ~p:~p/2 exited with reason: ~p",
                                          [Plugin, CallbackFun, Reason]}),
            {error, Req0}
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Pre-handler failed in execution. Type: ~p Reason: ~p~nStacktrace:~n~p", [Type, Reason, Stacktrace]),
            Req0 = render_page(500, Req, {?MODULE, run_plugins, 4, "Error when running pre-plugins. One plugin exited with reason: ~p", [Reason]}),
            {error, Req0}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eunit functions         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-endif.
