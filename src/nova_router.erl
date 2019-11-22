%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Nova router is responsible for handling all incoming request and
%%% routes them to the correct controller.
%%% @end
-module(nova_router).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         get_status_page/2,
         get_main_app/0,
         process_routefile/2,
         add_route/4,
         add_route/5,
         add_route/6,
         apply_routes/0,
         remove_route/2,
         get_routes/0,
         get_route_info/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-define(SERVER, ?MODULE).

-record(state, {
                main_app :: atom(),
                listener = nova_listener :: atom(),
                dispatch_table = [] :: list()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_status_page(Error, Req) ->
    cowboy_req:reply(Error, #{}, Req).

get_main_app() ->
    gen_server:call(?SERVER, get_main_app).

process_routefile(App, Routefile) ->
    gen_server:cast(?SERVER, {process_routes, App, Routefile}).

add_route(App, CallbackInfo, Route, Security) ->
    add_route(App, CallbackInfo, '_', Route, Security).

add_route(App, CallbackInfo, Host, Route, Security) ->
    add_route(App, CallbackInfo, Host, Route, Security, #{}).

add_route(App, CallbackInfo, Host, Route, Security, Options) ->
    gen_server:cast(?SERVER, {add_route, App, CallbackInfo, Host, Route, Security, Options}).

add_static(App, Path, Host, Route) ->
    gen_server:cast(?SERVER, {add_static, App, Path, Host, Route}).

apply_routes() ->
    gen_server:cast(?SERVER, apply_routes).

remove_route(Host, Route) ->
    gen_server:cast(?SERVER, {remove_route, Host, Route}).

get_routes() ->
    gen_server:call(?SERVER, get_routes).

get_route_info(Route) ->
    gen_server:call(?SERVER, {get_route_info, Route}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Apps =
        case application:get_env(nova_applications) of
            undefined -> [];
            {ok, AppsList} -> AppsList
        end,
    lists:foreach(fun load_app_route/1, Apps),
    {ok, MainApp} = application:get_application(),
    process_flag(trap_exit, true),
    {ok, #state{main_app = MainApp}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_main_app, _From, State = #state{main_app = Application}) ->
    {reply, {ok, Application}, State};
handle_call(get_routes, _From, State = #state{dispatch_table = DT}) ->
    {reply, {ok, DT}, State};
handle_call({get_route_info, Route}, _From, State = #state{dispatch_table = DT}) ->
    case lists:keyfind(Route, 1, DT) of
        false ->
            %% Not found
            {reply, {error, not_found}, State};
        Route ->
            {reply, {ok, Route}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({process_routes, App, Routefile}, State) ->
    {ok, HostRoutes} = file:consult(Routefile),
    process_routes(App, HostRoutes),
    apply_routes(),
    {noreply, State};

handle_cast({remove_route, Host, Route}, State = #state{dispatch_table = DT}) ->
    case proplists:get_value(Host, DT) of
        undefined ->
            ?WARNING("Could not remove route: ~p for host: ~p. Host not found!", [Route, Host]),
            {noreply, State};
        {Host, Routes} ->
            NewRoutes = proplists:delete(Route, Routes),
            case NewRoutes of
                Routes ->
                    ?WARNING("Could not remove route: ~p for host: ~p. Route not found!", [Route, Host]),
                    {noreply, State};
                _ ->
                    ?INFO("Removed route: ~p from host: ~p", [Route, Host]),
                    DT2 = proplists:delete(Host, DT),
                    NewDT = [{Host, NewRoutes}|DT2],
                    {noreply, State#state{dispatch_table = NewDT}}
            end
    end;

handle_cast(apply_routes, State = #state{dispatch_table = DT,
                                         listener = Listener}) ->
    ?INFO("Applying routes. ~p", [DT]),
    Dispatch = cowboy_router:compile(DT),
    cowboy:set_env(Listener, dispatch, Dispatch),
    {noreply, State};

handle_cast({add_static, App, Path, Host, Route}, State = #state{dispatch_table = DT}) ->
    RouteInfo = {Route, cowboy_static, {priv_dir, App, Path}},
    NewDT =
        case proplists:get_value(Host, DT) of
            undefined ->
                [{Host, [RouteInfo]}|DT];
            Routes ->
                [{Host, [RouteInfo|Routes]}|proplists:delete(Host, DT)]
        end,
    {noreply, State#state{dispatch_table = NewDT}};
handle_cast({add_route, App, CallbackInfo, Host, Route, Secure, Options}, State = #state{dispatch_table = DT}) ->
    InitialState = #{app => App,
                     secure => Secure
                    },
    RouteInfo =
        case maps:get(protocol, Options, http) of
            http ->
                {Module, Func} = CallbackInfo,
                HttpState = InitialState#{mod => Module,
                                          func => Func,
                                          methods => get_methods(Options),
                                          nova_handler => nova_http_handler},
                {Route, nova_http_handler, HttpState};
            ws ->
                SubProtocols = maps:get(subprotocols, Options, []),
                WSState = InitialState#{mod => CallbackInfo,
                                        subprotocols => SubProtocols,
                                        nova_handler => nova_ws_handler},
                {Route, nova_ws_handler, WSState};
            Other ->
                logger:info("No protocol matches ~p", [Other]),
                erlang:throw({unknown_protocol, Other})
        end,

    NewDT =
        case proplists:get_value(Host, DT) of
            undefined ->
                [{Host, [RouteInfo]}|DT];
            Routes ->
                [{Host, [RouteInfo|Routes]}|proplists:delete(Host, DT)]
        end,

    {noreply, State#state{dispatch_table = NewDT}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_app_route(#{name := AppName, routes_file := RouteFile}) ->
    case code:lib_dir(AppName) of
        {error, bad_name} ->
            ?WARNING("Could not find the application ~p. Check your config and rerun the application", [AppName]),
            ok;
        Filepath ->
            RouteFilePath = filename:join([Filepath, RouteFile]),
            process_routefile(AppName, RouteFilePath)
    end;
load_app_route(RouteInfo = #{name := AppName}) ->
    Routename = "priv/" ++ erlang:atom_to_list(AppName) ++ ".routes.erl",
    load_app_route(RouteInfo#{routes_file => Routename}).

process_routes(_, []) ->
    ok;
process_routes(App, [RoutesLine|T]) ->
    Prefix = maps:get(prefix, RoutesLine, ""),
    Host = maps:get(host, RoutesLine, '_'),
    Routes = maps:get(routes, RoutesLine, []),
    Statics = maps:get(statics, RoutesLine, []),
    Secure = maps:get(security, RoutesLine, false),
    add_routes(App, Host, Prefix, Secure, Routes),
    add_statics(App, Host, Prefix, Statics),
    process_routes(App, T).


add_routes(_, _, _, _, []) ->
    ok;
add_routes(App, Host, Prefix, Secure, [{Route, CallbackInfo} | T]) ->
    add_route(App, CallbackInfo, Host, Prefix ++ Route, Secure),
    add_routes(App, Host, Prefix, Secure, T);
add_routes(App, Host, Prefix, Secure, [{Route, CallbackInfo, Options} | T]) when is_map(Options) ->
    add_route(App, CallbackInfo, Host, Prefix ++ Route, Secure, Options),
    add_routes(App, Host, Prefix, Secure, T);
add_routes(App, Host, Prefix, Secure, [{Route, Module, Function} | T]) ->
    add_route(App, {Module, Function}, Host, Prefix ++ Route, Secure),
    add_routes(App, Host, Prefix, Secure, T);
add_routes(App, Host, Prefix, Secure, [{Route, Module, Function, Options} | T]) ->
    add_route(App, {Module, Function}, Host, Prefix ++ Route, Secure, Options),
    add_routes(App, Host, Prefix, Secure, T).

add_statics(_, _, _, []) ->
    ok;
add_statics(App, Host, Prefix, [{Route, Path} | T]) ->
    add_static(App, Path, Host, Prefix ++ Route),
    add_statics(App, Host, Prefix, T).


get_methods(#{methods := M}) when is_list(M) ->
    Res = lists:map(fun(get)  -> <<"GET">>;
                       (post) -> <<"POST">>;
                       (put) -> <<"PUT">>;
                       (delete) -> <<"DELETE">>;
                       (_) -> throw(unknown_method)
                    end, M),
    case length(Res) of
        4 -> '_';
        _ -> Res
    end;
get_methods(#{methods := M}) ->
    get_methods(#{methods => [M]});
get_methods(_) ->
    '_'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eunit functions         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
process_empty_route_test_() ->
    ?_assert(ok =:= process_routes(test, [])).

methods_test_() ->
    [
     ?_assertMatch([<<"GET">>], get_methods(#{methods => [get]})),
     ?_assertMatch([<<"POST">>], get_methods(#{methods => [post]})),
     ?_assertMatch([<<"PUT">>], get_methods(#{methods => [put]})),
     ?_assertMatch([<<"DELETE">>], get_methods(#{methods => [delete]})),
     ?_assertMatch([<<"GET">>, <<"PUT">>], get_methods(#{methods => [get, put]}))
    ].

methods_wildcard_test_() ->
    [?_assertException(throw, unknown_method, get_methods(#{methods => [something_else]})),
     ?_assertMatch('_', get_methods(undefined)),
     ?_assertMatch('_', get_methods(#{methods => [get,put,delete,post]}))
    ].


-endif.
