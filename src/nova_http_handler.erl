%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Callback controller for handling http requests
%%% @end

-module(nova_http_handler).

-export([
         init/2,
         handle/3
        ]).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type controller_data() :: #{auth_data => term(),
                             _ => _}.
-export_type([controller_data/0]).

-type nova_http_state() :: #{mod := atom(),
                             func := atom(),
                             methods := [binary()] | '_',
                             %% Intermediate data blow
                             resp_status := integer(),
                             req := cowboy_req:req(),
                             %% This structure is sent to the controller
                             controller_data := controller_data(),
                             _ := _}.
-export_type([nova_http_state/0]).


%%--------------------------------------------------------------------
%% @doc
%% Callback function from nova_handler. This is the initial call where
%% all the logic is handed.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), State :: nova_http_state()) ->
                  {ok, Req0 :: cowboy_req:req(), State0 :: nova_http_state()}.
init(Req, State) ->
    State0 = State#{resp_status => 200, req => Req},

    {ok, PrePlugins} = nova_plugin:get_plugins(pre_request, http),
    #{req := Req0, resp_status := StatusCode} =
        case run_plugins(PrePlugins, pre_request, State0) of
            {ok, State1} ->
                %% Call the controller
                {ok, State2} = invoke_controller(State1),
                %% Invoke post_request plugins
                {ok, PostPlugins} = nova_plugin:get_plugins(post_request, http),
                {_, State3} = run_plugins(PostPlugins, post_request, State2),
                State3;
            {stop, State1} ->
                State1
        end,
    %% Here we send out the response to the client
    Req1 = cowboy_req:reply(StatusCode, Req0),
    %% We return the initial state since we don't want to have the intermediate things stored in cowboy
    {ok, Req1, State}.

%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% First checks so that method of the request is supported by the controller.
%% If not a 405 page is returned. '_' is used as a catch-all when defining the route.
%% @end
%%--------------------------------------------------------------------
%% Methods for this path is defined as a 'catch_all' so just continue executing
-spec invoke_controller(State :: nova_http_state()) -> {ok, State0 :: nova_http_state()}.
invoke_controller(State = #{mod := Mod, func := Func, methods := '_'}) ->
    handle(Mod, Func, State);
%% We have a list of methods we allow, so check if they match the one requested before continuing
invoke_controller(State = #{req := #{method := ReqMethod}, methods := Methods}) ->
    case lists:any(fun(X) -> X == ReqMethod end, Methods) of
        true ->
            invoke_controller(State#{methods := '_'});
        false ->
            render_page(405, State)
    end.


%%--------------------------------------------------------------------
%% @doc
%% This function is exposed mostly cause we need to call it from nova_router.
%% It returns the raw handle request instead of the aggregated result returned in init/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(Mod :: atom(), Fun :: atom(), State :: nova_http_state()) ->
                    {ok, State0 :: nova_http_state()}.
handle(Mod, Fun, State = #{req := Req, controller_data := ControllerData}) ->
    ?DEBUG("Handling request for ~p:~p", [Mod, Fun]),
    try Mod:Fun(ControllerData#{req => Req}) of
        RetObj ->
            case nova_handlers:get_handler(element(1, RetObj)) of
                {ok, Callback} ->
                    {ok, StatusCode, Headers, Body, State0} = Callback(RetObj, {Mod, Fun}, State),
                    Req0 = set_resp(Headers, Body, Req),
                    {ok, State0#{req => Req0, resp_status => StatusCode}};
                _ ->
                    ?ERROR("Unknown return object ~p returned from module: ~p function: ~p", [RetObj, Mod, Fun]),
                    render_page(500, State, {Mod, Fun, 1, "Unknown return object ~p returned from module: ~s:~s/1",
                                             [RetObj, Mod, Fun]})
            end
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Controller (~p:~p/1) failed with ~p:~p.~nStacktrace:~n~p",
                   [Mod, Fun, Type, Reason, Stacktrace]),
            render_page(500, State, {Mod, Fun, 1, Type, Reason, Stacktrace})
    end.


%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% Checks if a page is defined in nova_router for the given status code.
%% If it is that page will be rendered and a new request will be returned
%% with the rendered body. Otherwise a request with a blank body will be returned.
%% @end
%%--------------------------------------------------------------------
-spec render_page(StatusCode :: integer(), State :: nova_http_state()) -> {ok, State0 :: nova_http_state()}.
render_page(StatusCode, State = #{req := Req}) ->
    {ok, StatusCode, Headers, Body, State0} =
        case nova_router:status_page(StatusCode, State) of
            {error, _} ->
                {ok, StatusCode, #{}, <<>>, State};
            FoundPage ->
                FoundPage
        end,
    Req0 = set_resp(Headers, Body, maps:get(req, State0, Req)),
    {ok, State0#{resp_status => StatusCode, req => Req0}}.


%%--------------------------------------------------------------------
%% @doc
%% @hidden
%% Renders a page with additional arguments. The returning cowboy request
%% does always contain a body.
%% @end
%%--------------------------------------------------------------------
-spec render_page(StatusCode :: integer(), State :: nova_http_state(),
                  {Module :: atom(), Function :: atom(), Arity :: integer(), Format :: list(), Args :: list()} |
                  {Module :: atom(), Function :: atom(), Arity :: integer(), Type :: atom(),
                   Reason :: atom(), Stacktrace :: any()}) ->
                         {ok, State0 :: nova_http_state()}.
render_page(500, State = #{req := Req}, {Module, Function, Arity, Format, Args}) ->
    {ok, StatusCode, Headers, Body, State0} =
        case nova_router:status_page(500, Req) of
            {error, not_found} ->
                Msg = lists:flatten(io_lib:format(Format, Args)),
                DevMode = nova:get_env(dev_mode, false),
                {ok, HTML} = nova_internal_error_dtl:render([{module, Module},
                                                             {function, Function},
                                                             {arity, Arity},
                                                             {message, Msg},
                                                             {dev_mode, DevMode}], []),
                {ok, 500, #{}, HTML, State};
            FoundPage ->
                FoundPage
        end,
    Req0 = set_resp(Headers, Body, maps:get(req, State0, Req)),
    {ok, State0#{req => Req0, resp_status => StatusCode}};
render_page(500, State = #{req := Req}, {Module, Function, Arity, Type, Reason, Stacktrace}) ->
    {ok, StatusCode, Headers, Body, State0} =
        case nova_router:status_page(500, State) of
            {error, not_found} ->
                DevMode = nova:get_env(dev_mode, false),
                {ok, HTML} = nova_internal_error_dtl:render([{module, Module},
                                                             {function, Function},
                                                             {arity, Arity},
                                                             {type, Type},
                                                             {reason, Reason},
                                                             {stacktrace, Stacktrace},
                                                             {dev_mode, DevMode}], []),
                {ok, 500, #{}, HTML, State};
            Page ->
                Page
        end,
    Req0 = set_resp(Headers, Body, maps:get(req, State0, Req)),
    {ok, State0#{req => Req0, resp_status => StatusCode}}.

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
-spec run_plugins(Plugins :: list(), CallbackFun :: atom(), State :: nova_http_state()) ->
                         {ok, State0 :: nova_http_state()} |
                         {stop, State0 :: nova_http_state()}.
run_plugins([], _Callback, State) ->
    {ok, State};
run_plugins([{Plugin, Options}|Tl], Callback, State) ->
    try Plugin:Callback(State, Options) of
        {ok, State0} ->
            run_plugins(Tl, Callback, State0);
        %% Stop indicates that we want the entire pipe of plugins/controller to be stopped.
        {stop, State0} ->
            {stop, State0};
        %% Break is used to signal that we are stopping further executing of plugins within the same Callback
        {break, State0} ->
            {ok, State0};
        {error, Reason} ->
            ?ERROR("Plugin returned error with reason ~p", [Reason]),
            Msg = "Error when running plugins. Plugin ~p:~p/2 exited with reason: ~p",
            {ok, State0} = render_page(500, State, {?MODULE, run_plugins, 3, Msg, [Plugin, pre_request, Reason]}),
            {stop, State0}
    catch
        Type:Reason:Stacktrace ->
            ?ERROR("Plugin failed in execution. Type: ~p Reason: ~p~nStacktrace:~n~p", [Type, Reason, Stacktrace]),
            Msg =  "Error when running plugins. One plugin exited with reason: ~p",
            {ok, State0} = render_page(500, State, {?MODULE, run_plugins, 3, Msg, [Reason]}),
            {stop, State0}
    end.

set_resp(Headers, Body, Req) ->
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    cowboy_req:set_resp_body(Body, Req0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eunit functions         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-endif.
