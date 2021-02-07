%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% <i>Nova router</i> is in charge of all routing in Nova. When a nova application
%%% is started its routing-file is read and processed by the router. You should
%%% not use this module directly if you're not certain of what you are doing.
%%%
%%% A basic routing file could look something like this;
%%% <code title="priv/my_demo_app.routes.erl">
%%% #{prefix => "",
%%%   security => false,
%%%   routes => [
%%%              {"/", {hello_world_controller, index}},
%%%              {"/blog", {my_blog_controller, index}},
%%%              {404, {error_controller, not_found}}
%%%             ],
%%%   statics => [
%%%               {"/assets/[...]", "assets"}
%%%              ]
%%%  }.
%%% </code>
%%% Lets go through it in sections.
%%%
%%% <icode>prefix</icode> - This tells us if the routes should have a common prefix (Eg "/v1" or similar)
%%%
%%% <icode>security</icode> - If this is a tuple of type <icode>{Module, Function}</icode> Nova will try and call
%%% that function each time a page from the <i>routes</i>-section is visited. That function could return either
%%% true or false, telling nova if the user should be allowed to visit the page or not.
%%%
%%% <icode>routes</icode> - Describes the routes inside this block. (An application can have several of these maps
%%% defined)
%%%
%%% <icode>statics</icode> - Routes for all the static assets
%%%
%%% <h3>Routing more in detail</h3>
%%% There's three different ways of describing a route, depending on what kind of protocol (http, websocket) or what
%%% options you have.
%%%
%%% <icode>{Route, {Module, Function}}</icode> - This is a regular route used for HTTP.
%%% @end
%%% @todo Extend the introduction to how routing works.
%%% @end
%%% Created : 17 Nov 2019 by Niclas Axelsson <niclas@burbasconsulting.com>
%%% Updated : 28 Dec 2020 by Niclas Axelsson <niclas@burbasconsulting.com>
%%%-------------------------------------------------------------------
-module(nova_router).

-behaviour(gen_server).

-callback parse_routefile(Appname :: atom()) -> any().

%% API
-export([
         start_link/1,
         get_app_info/1,
         add_route/2,
         apply_routes/0,
         status_page/2
        ]).

%% Utility functions
-export([
         parse_routefile/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

-include("include/nova.hrl").
-include("nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type route_info() :: #{application := atom(),
                        prefix := string(),
                        host := atom() | string(),
                        security := false | {atom(), atom()},
                        _ => _}.
-export_type([route_info/0]).

-type route() :: {Route :: string(), {Module :: atom(), Function :: atom()}} |
                 {Route :: string(), {Module :: atom(), Function :: atom()}, Options :: map()} |
                 {Route :: string(), Module :: atom(), Function :: atom()} |
                 {Route :: string(), CallbackInfo :: atom(), Options :: map()} |
                 {StatusCode :: integer(), {Module :: atom(), Function :: atom()}} |
                 {Route :: string(), Filename :: string()}.
-export_type([route/0]).


-type app_info() :: #{name := atom(),
                      prefix := string(),
                      host := atom() | string(),
                      security := false | {atom(), atom()},
                      routes := [route()]}.
-export_type([app_info/0]).



-define(SERVER, ?MODULE).

-record(state, {
                worker_pid :: pid() | undefined,
                main_app :: atom(),
                apps :: #{AppName :: atom() => app_info()} | #{},
                route_table :: #{binary() => list()},
                static_route_table :: #{StatusCode :: integer() => {Mod :: atom(), Func :: atom()}}
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(BootstrapApp :: atom()) -> {ok, Pid :: pid()} |
                                            {error, Error :: {already_started, pid()}} |
                                            {error, Error :: term()} |
                                            ignore.
start_link(BootstrapApp) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, BootstrapApp, []).



%%--------------------------------------------------------------------
%% @doc
%% Tries and fetch a status page from the routing table. If a page was
%% found it will be returned to the requester. If not an error will be
%% returned.
%% @end
%%--------------------------------------------------------------------
-spec status_page(StatusCode :: integer(), NovaHttpState :: nova_http_handler:nova_http_state()) ->
                         {ok, NovaHttpState0 :: nova_http_handler:nova_http_state()} |
                                           {error, not_found}.
status_page(StatusCode, NovaHttpState) when is_integer(StatusCode) ->
    gen_server:call(?SERVER, {status_page, StatusCode, NovaHttpState}).


%%--------------------------------------------------------------------
%% @doc
%% Returns information about a specified app. This information contains
%% route-prefix, host, security rules etc.
%% @end
%%--------------------------------------------------------------------
-spec get_app_info(App :: atom()) -> {ok, app_info()} | {error, not_found}.
get_app_info(App) ->
    gen_server:call(?SERVER, {get_app_prefix, App}).

%%--------------------------------------------------------------------
%% @doc
%% Applying all routes read by nova_router
%% @end
%%--------------------------------------------------------------------
-spec apply_routes() -> ok.
apply_routes() ->
    gen_server:cast(?SERVER, apply_routes).

%%--------------------------------------------------------------------
%% @doc
%% Collects all the routes for the application and it's included apps.
%% Starts by using the application marked as 'bootstrap_application'
%% from the config.
%% @end
%%--------------------------------------------------------------------
-spec collect_routes() -> ok.
collect_routes() ->
    gen_server:cast(?SERVER, collect_routes).

%%--------------------------------------------------------------------
%% @doc
%% Add a single route to nova.
%% @end
%%--------------------------------------------------------------------
-spec add_route(RouteInfo :: route_info(), Route :: route()) -> ok.
add_route(RouteInfo, Route = {_, FileOrDir}) when is_list(FileOrDir) ->
    gen_server:cast(?SERVER, {add_static, RouteInfo, Route});
add_route(RouteInfo, Route) ->
    gen_server:cast(?SERVER, {add_route, RouteInfo, Route}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(BootstrapApp :: atom()) -> {ok, State :: term()} |
                                      {ok, State :: term(), Timeout :: timeout()} |
                                      {ok, State :: term(), hibernate} |
                                      {stop, Reason :: term()} |
                                      ignore.
init(BootstrapApp) ->
    process_flag(trap_exit, true),
    collect_routes(),
    {ok, #state{route_table = #{},
                apps = #{},
                static_route_table = #{},
                main_app = BootstrapApp
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({status_page, StatusCode, NovaHttpState}, _From, State = #state{static_route_table = SRT}) ->
    case maps:get(StatusCode, SRT, undefined) of
        {Mod, Func} ->
            Reply = nova_http_handler:handle(Mod, Func, NovaHttpState#{mod => dummy,
                                                                       func => dummy,
                                                                       methods => '_'}),
            {reply, Reply, State};
        _ ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_app, App}, _From, State = #state{apps = AppsInfo}) ->
    Return =
        case maps:get(App, AppsInfo, undefined) of
            undefined ->
                {error, not_found};
            AppInfo ->
                {ok, AppInfo}
        end,
    {reply, Return, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(collect_routes, State = #state{worker_pid = undefined, main_app = MainApp}) ->
    {ParseMod, ParseFun} = application:get_env(nova, route_parse_mf, {?MODULE, parse_routefile}),
    WorkerPid = spawn_link(ParseMod, ParseFun, [MainApp]),
    {noreply, State#state{worker_pid = WorkerPid}};
handle_cast({add_route, #{application := Application, prefix := Prefix,
                          host := Host, security := Secure, controller_data := ControllerData}, RouteDetails},
            State = #state{static_route_table = SRT, route_table = RouteTable, apps = AppsInfo}) ->
    InitialState = #{app => Application,
                     controller_data => ControllerData},
    case parse_route(RouteDetails, Prefix, Secure, InitialState) of
        {status, {StatusCode, {Module, Function}}} ->
            SRT0 = maps:put(StatusCode, {Module, Function}, SRT),
            AppsInfo0 = add_route_to_app(Application, Prefix, Host, false, RouteDetails, AppsInfo),
            {noreply, State#state{static_route_table = SRT0, apps = AppsInfo0}};
        {Label, #{route := Route, entries := RouteEntry} = NovaRouteObj} when Label == route orelse
                                                                              Label == static ->

            AppsInfo0 = add_route_to_app(Application, Prefix, Host, Secure, RouteDetails, AppsInfo),
            HostRoutes = maps:get(Host, RouteTable, #{}),
            RouteObj =
                case maps:get(Route, HostRoutes, false) of
                    false ->
                        %% There's no previous record of this route - Just add it
                        NovaRouteObj;
                    #{entries := ExistingMethods} ->
                        %% Just merge the existing objects with the route we have
                        NovaRouteObj#{entries => maps:merge(RouteEntry, ExistingMethods)}
                end,
            NewRouteTable = RouteTable#{Host => HostRoutes#{Route => RouteObj}},
            {noreply, State#state{route_table = NewRouteTable, apps = AppsInfo0}};
        false ->
            %% Do not change anything - Something was wrong with this route
            {noreply, State}
    end;
handle_cast(apply_routes, State = #state{route_table = RouteTable}) ->
    CatchAllRoute = [{'_', [{'_', nova_http_handler, no_route}]}],
    HostList = maps:to_list(RouteTable),
    CowboyRoutes = [ to_cowboy_routes(Host, Routes) || {Host, Routes} <- HostList ],
    Dispatch = cowboy_router:compile(CowboyRoutes ++ CatchAllRoute),
    ?DEBUG("Applying routes: ~p", [CowboyRoutes]),
    cowboy:set_env(?NOVA_LISTENER, dispatch, Dispatch),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'EXIT', WorkerPid, normal}, State = #state{worker_pid = WorkerPid}) ->
    ?DEBUG("WorkerPid ~p exited with reason normal", [WorkerPid]),
    apply_routes(),
    {noreply, State#state{worker_pid = undefined}};
handle_info({'EXIT', WorkerPid, Reason}, State = #state{worker_pid = WorkerPid}) ->
    ?WARNING("WorkerPid exited with reason ~p. Restarting process", [Reason]),
    collect_routes(),
    {noreply, State#state{worker_pid = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_route({StatusCode, {Module, Function}}, _Prefix, _Secure, _InitialState) when is_integer(StatusCode) ->
    {status, {StatusCode, {Module, Function}}};
parse_route({static, {Route, DirOrFile}}, Prefix, Secure, InitialState) ->
    parse_route({static, {Route, DirOrFile}, #{}}, Prefix, Secure, InitialState);
parse_route({static, {Route, DirOrFile}, Options}, Prefix, Secure, InitialState) ->
    Application = maps:get(app, InitialState),
    case code:lib_dir(Application, priv) of
        {error, Error} ->
            ?ERROR("Could not apply route ~s. Could not find priv dir of application ~s, got error: ~p",
                   [Route, Application, Error]),
            false;
        PrivDir ->
            Filename = filename:join([PrivDir, DirOrFile]),
            case {filelib:is_dir(Filename), filelib:is_file(Filename)} of
                {true, _} ->
                    MethodRoutes = [{X, #{type => priv_dir,
                                          secure => Secure,
                                          path => DirOrFile}} || X <- [ get_method(Y) || Y <- maps:get(methods, Options, [all]) ] ],
                    {static, InitialState#{route => Prefix ++ Route,
                                           entries => maps:from_list(MethodRoutes)}};
                {_, true} ->
                    MethodRoutes = [{X, #{type => priv_file,
                                          secure => Secure,
                                          path => DirOrFile}} || X <- [ get_method(Y) || Y <- maps:get(methods, Options, [all]) ] ],
                    {static, InitialState#{route => Prefix ++ Route,
                                           entries => maps:from_list(MethodRoutes)}};
                _ ->
                    ?WARNING("Could not find static file ~s which is reffered from the route ~s (app: ~s). " ++
                                 "Ignoring route", [DirOrFile, Application, Route]),
                    false
            end
    end;
parse_route({Route, {Module, Function}}, Prefix, Secure, InitialState) ->
    parse_route({Route, {Module, Function}, #{}}, Prefix, Secure, InitialState);
parse_route({Route, CallbackInfo, #{protocol := ws} = Options}, Prefix, Secure, InitialState) ->
    MethodRoutes = [{X, #{mod => CallbackInfo,
                          secure => Secure,
                          options => Options,
                          nova_handler => nova_ws_handler}} || X <- [ get_method(Y) || Y <- maps:get(methods, Options, [all]) ] ],
    {route, InitialState#{route => Prefix ++ Route,
                          entries => maps:from_list(MethodRoutes)}};
parse_route({Route, {Module, Function}, Options}, Prefix, Secure, InitialState) ->
    MethodRoutes = [{X, #{mod => Module,
                          func => Function,
                          options => Options,
                          secure => Secure,
                          nova_handler => nova_http_handler}} || X <- [ get_method(Y) || Y <- maps:get(methods, Options, [all]) ] ],
    {route, InitialState#{route => Prefix ++ Route,
                          entries => maps:from_list(MethodRoutes)}};
parse_route({_Route, _Module, _Function}, _Prefix, _Secure, _InitialState) ->
    ?DEPRECATED("Route of format {Route, Module, Function} is deprecated!"),
    false;
parse_route(Other, _Prefix, _Secure, _InitialState) ->
    ?WARNING("Could not parse route ~p", [Other]),
    false.


get_method(get)  -> <<"GET">>;
get_method(post) -> <<"POST">>;
get_method(put) -> <<"PUT">>;
get_method(delete) -> <<"DELETE">>;
get_method(options) -> <<"OPTIONS">>;
get_method(patch) -> <<"PATCH">>;
get_method(head) -> <<"HEAD">>;
get_method(all) -> '_';
get_method(_) -> throw(unknown_method).


add_route_to_app(App, Prefix, Host, Security, Route, AppMap) when is_map(AppMap) ->
    case maps:get(App, AppMap, undefined) of
        undefined ->
            #{App => #{name => App,
                       prefix => Prefix,
                       host => Host,
                       security => Security,
                       routes => [Route]}};
        Result ->
            AppMap#{App => Result#{routes => [Route|maps:get(routes, Result)]}}
    end.


to_cowboy_routes(Host, Routes) when is_map(Routes) ->
    %% Each key is a route. Transform into a list of [{Route :: binary(), InitialState :: map()}]
    RoutesList = maps:to_list(Routes),
    {Host, lists:map(fun({Route, InitialState = #{entries := Entries}}) ->
                             [K|_] = maps:keys(Entries),
                             FirstRoute = maps:get(K, Entries),
                             case maps:get(nova_handler, FirstRoute, undefined) of
                                 undefined ->
                                     %% This is a static resource
                                     Type = maps:get(type, FirstRoute),
                                     {Route, cowboy_static, {Type, maps:get(app, InitialState), maps:get(path, FirstRoute)}};
                                 NovaHandler ->
                                     {Route, NovaHandler, InitialState}
                             end
                     end, RoutesList)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Standard implementation of parsing the routefile. This can be
%% overriden by config.
%% @end
%%--------------------------------------------------------------------
-spec parse_routefile(Application :: atom() | #{name := atom(), routes_file => list()}) -> ok.
parse_routefile(#{name := Application, routes_file := RoutesFile} = AppRoute) ->
    %% This is the parsing function. It should send path info back to the gen_server and
    %% exit normally when finished
    case code:lib_dir(Application) of
        {error, _} ->
            ?WARNING("Could not find the application ~s. Check your config and re-run your project!", [Application]),
            ok;
        Filepath ->
            ?DEBUG("Processing routefile ~s", [Filepath]),
            AppPrefix = maps:get(prefix, AppRoute, ""),
            RouteFilePath = filename:join([Filepath, RoutesFile]),
            {ok, AppRoutes} = file:consult(RouteFilePath),
            lists:foreach(fun(AppMap) ->
                                  %% Extract the information from routes
                                  Prefix = filename:join([AppPrefix, maps:get(prefix, AppMap, "")]),
                                  Host = maps:get(host, AppMap, '_'),
                                  Routes = maps:get(routes, AppMap, []),
                                  Statics = maps:get(statics, AppMap, []),
                                  Security = maps:get(security, AppRoute, maps:get(security, AppMap, false)),
                                  %% Build intermediate object
                                  RouteInfo = #{
                                                application => Application,
                                                prefix => Prefix,
                                                host => Host,
                                                security => Security,
                                                controller_data => #{}
                                               },
                                  %% Check for handlers
                                  [ nova_handlers:register_handler(Handle, Callback) ||
                                      {Handle, Callback} <- maps:get(handlers, AppMap, []) ],

                                  %% Add routes
                                  [ add_route(RouteInfo, Route) || Route <- Routes ++ Statics ]
                          end, AppRoutes),
            %% Parse routefile for nova applications this app have included
            OtherApps = application:get_env(Application, nova_applications, []),
            [ parse_routefile(OtherAppRoute) || OtherAppRoute <- OtherApps ]
    end;
parse_routefile(Application) when is_atom(Application) ->
    parse_routefile(#{name => Application, routes_file => get_routefile_path(Application)}).


get_routefile_path(#{name := Application}) ->
    lists:concat(["priv/", Application, ".routes.erl"]);
get_routefile_path(Application) when is_atom(Application) ->
    get_routefile_path(#{name => Application}).



-ifdef(TEST).

static_routes_test_() ->
    [ static_routes(X) || X <- ["", "/v1"] ].

static_routes(Prefix) ->
    [
     ?_assertEqual(parse_route({static, {"/nova.png", <<"static/nova.png">>}}, Prefix, #{app => nova}), {static, {Prefix++"/nova.png", cowboy_static, {priv_file, nova, <<"static/nova.png">>}}}),
     ?_assertEqual(parse_route({static, {"/not_found.png", <<"static/404.png">>}}, Prefix, #{app => nova}), false),
     ?_assertEqual(parse_route({static, {"/secret_dir", <<"static">>}}, Prefix, #{app => nova}), {static,{Prefix++"/secret_dir",cowboy_static, {priv_dir,nova,<<"static">>}}})
    ].

regular_routes_test_() ->
    [ regular_routes(X) || X <- ["", "/v1"] ].

regular_routes(Prefix) ->
    [
     ?_assertEqual(parse_route({"/index", {dummy_mod, dummy_func}}, Prefix, #{}), {route,{Prefix++"/index",nova_http_handler,
                                                                                          #{func => dummy_func, methods => '_', mod => dummy_mod,
                                                                                            nova_handler => nova_http_handler}}}),
     ?_assertEqual(parse_route({"/ws", some_ws_mod, #{protocol => ws}}, Prefix, #{}), {route,{Prefix++"/ws",nova_ws_handler,
                                                                                          #{mod => some_ws_mod, nova_handler => nova_ws_handler,
                                                                                            subprotocols => []}}}),
     ?_assertEqual(parse_route({"/route2", {dummy_mod, dummy_func}, #{}}, Prefix, #{}), {route,{Prefix++"/route2",nova_http_handler,
                                                                                            #{func => dummy_func, methods => '_', mod => dummy_mod,
                                                                                              nova_handler => nova_http_handler}}}),
     ?_assertEqual(parse_route({"/route3", dummy_mod, dummy_func}, Prefix, #{}), false),
     ?_assertEqual(parse_route(illegal_route, Prefix, #{}), false)
    ].

status_code_routes_test_() ->
    [ status_code_routes(X) || X <- ["", "/v1"] ].

status_code_routes(Prefix) ->
    [
     ?_assertEqual(parse_route({404, {dummy_mod, dummy_func}}, Prefix, #{}), {status,{404,{dummy_mod,dummy_func}}}),
     ?_assertEqual(parse_route({500, {dummy_mod, dummy_func}}, Prefix, #{}), {status,{500,{dummy_mod,dummy_func}}})
    ].


get_method_test_() ->
    [ ?_assertEqual(get_method(X), Y) || {X, Y} <- [
                                                    {all, '_'},
                                                    {get, [<<"GET">>]},
                                                    {post, [<<"POST">>]},
                                                    {put, [<<"PUT">>]},
                                                    {delete, [<<"DELETE">>]},
                                                    {options, [<<"OPTIONS">>]},
                                                    {patch, [<<"PATCH">>]},
                                                    {head, [<<"HEAD">>]}
                                                   ] ].
get_method_illegal_test_() ->
    [ ?_assertThrow(unknown_method, get_method(wrong)),
      ?_assertThrow(unknown_method, get_method(hehe))
    ].



-endif.
