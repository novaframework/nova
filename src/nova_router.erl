%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Router module for nova. This module is responsible for compiling routes, dispatching requests to the correct handler
%%% and managing the routing table. It also exposes an API for modulating the routing table at runtime.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_router).
-behaviour(cowboy_middleware).

%% Cowboy middleware-callbacks
-export([
         execute/2
        ]).

%% API
-export([
         compile/1,
         compile/2,
         lookup_url/1,
         lookup_url/2,
         lookup_url/3,
         render_status_page/2,
         render_status_page/3,
         render_status_page/5,

         %% Expose the router-callback
         routes/1,

         %% Fetch information about the routing table
         plugins/0,
         compiled_apps/0,
         compiled_apps/1,

         %% Modulates the routes-table
         add_routes/1,
         add_routes/2,
         add_routes/3,
         remove_application/1,
         remove_application/2,
         delete_dispatch/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_router.hrl").
-include("../include/nova.hrl").

-type bindings() :: #{binary() := binary()}.

-type lookup_result() :: {ok, bindings(), Value :: term()} |
                         {ok, bindings(), Value :: term(), PathInfo :: [binary()]} |
                         {error, not_found} |
                         {error, comparator_not_found, AllowedMethods :: [binary()]}.

-export_type([bindings/0, lookup_result/0]).

%% This module is also exposing callbacks for routers
-callback routes(Env :: atom()) -> Routes :: [map()].
-callback controllers(Env :: atom()) -> Controllers :: [module() | {module(), map()}].

-optional_callbacks([routes/1, controllers/1]).


-define(NOVA_APPS, nova_apps).
-define(NOVA_PLUGINS, nova_plugins).
-define(NOVA_DISPATCH, nova_dispatch).

%% Each Cowboy listener owns a routing table, addressed by a dispatch key.
%% Listeners started by nova_sup:add_application/2 get their own, so two
%% listeners on different ports do not serve each other's routes. The default
%% listener uses nova_dispatch, which is also what every existing caller and
%% every stored dispatch table already uses.
-type dispatch_key() :: term().
-export_type([dispatch_key/0]).

-spec compiled_apps() -> [{App :: atom(), Prefix :: list()}].
compiled_apps() ->
    compiled_apps(?NOVA_DISPATCH).

-spec compiled_apps(DispatchKey :: dispatch_key()) -> [{App :: atom(), Prefix :: list()}].
compiled_apps(DispatchKey) ->
    StorageBackend = storage_backend(),
    StorageBackend:get(apps_key(DispatchKey), []).


%% TODO! We need to implement a way to get and remove plugins for a path
plugins() ->
    StorageBackend = storage_backend(),
    StorageBackend:get(?NOVA_PLUGINS, []).

-spec compile(Apps :: [atom() | {atom(), map()}]) -> nova_routing_trie:trie().
compile(Apps) ->
    compile(Apps, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
%% @doc
%% Compile the given applications into the routing table addressed by
%% `DispatchKey', merging into whatever is already stored there.
%% @end
%%--------------------------------------------------------------------
-spec compile(Apps :: [atom() | {atom(), map()}], DispatchKey :: dispatch_key()) ->
          nova_routing_trie:trie().
compile(Apps, DispatchKey) ->
    UseStrict = application:get_env(nova, use_strict_routing, false),
    StorageBackend = storage_backend(),

    StoredDispatch = StorageBackend:get(DispatchKey,
                                        nova_routing_trie:new(#{strict => UseStrict})),
    Dispatch = compile(Apps, StoredDispatch, #{dispatch_key => DispatchKey}),
    %% Write the updated dispatch to storage
    StorageBackend:put(DispatchKey, Dispatch),
    Dispatch.

-spec execute(Req, Env :: cowboy_middleware:env()) -> {ok, Req, Env0} | {stop, Req}
                                                          when Req::cowboy_req:req(),
                                                               Env0::cowboy_middleware:env().
execute(Req = #{host := Host, path := Path, method := Method}, Env) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(dispatch_key(Env)),
    case nova_routing_trie:find(Host, Path, Method, Dispatch) of
        {error, not_found} ->
            logger:debug(<<"Path ~p not found for ~p in ~p">>, [Path, Method, Host]),
            render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {error, comparator_not_found, AllowedMethods} ->
            logger:debug(<<"Method not allowed: ~p for ~p. Allowed methods: ~p">>, [Method, Path, AllowedMethods]),
            AllowHeader = iolist_to_binary(lists:join(<<", ">>, AllowedMethods)),
            %% Set the 'allow'-header
            Req1 = cowboy_req:set_resp_header(<<"allow">>, AllowHeader, Req),
            render_status_page('_', 405, #{error => "Method not allowed"}, Req1, Env);
        {ok, Bindings, #nova_handler_value{app = App, callback = Callback, secure = Secure, plugins = Plugins,
                                           extra_state = ExtraState}} ->
            {ok,
             Req#{plugins => Plugins,
                  extra_state => ExtraState,
                  bindings => Bindings},
             Env#{app => App,
                  callback => Callback,
                  secure => Secure,
                  controller_data => #{}
                 }
            };
        {ok, Bindings, #nova_handler_value{app = App, callback = Callback,
                                           secure = Secure, plugins = Plugins, extra_state = ExtraState}, Pathinfo} ->
            {ok,
             Req#{plugins => Plugins,
                  extra_state => ExtraState#{pathinfo => Pathinfo},
                  bindings => Bindings},
             Env#{app => App,
                  callback => Callback,
                  secure => Secure,
                  controller_data => #{}
                 }
            };
        {ok, Bindings, #cowboy_handler_value{app = App, handler = Handler, arguments = Args,
                                              plugins = Plugins, secure = Secure}} ->
            {ok,
             Req#{plugins => Plugins,
                  bindings => Bindings},
             Env#{app => App,
                  cowboy_handler => Handler,
                  arguments => Args,
                  secure => Secure
                 }
            };
        Error ->
            ?LOG_ERROR(#{reason => <<"Unexpected return from nova_routing_trie:find/4">>,
                         return_object => Error}),
            render_status_page(Host, 404, #{error => Error}, Req, Env)
    end.

-spec lookup_url(Path :: nova_routing_trie:path()) -> lookup_result().
lookup_url(Path) ->
    lookup_url('_', Path).

-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path()) -> lookup_result().
lookup_url(Host, Path) ->
    lookup_url(Host, Path, '_').

-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path(),
                 Method :: nova_routing_trie:comparator()) -> lookup_result().
lookup_url(Host, Path, Method) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(?NOVA_DISPATCH),
    lookup_url(Host, Path, Method, Dispatch).

-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path(),
                 Method :: nova_routing_trie:comparator(),
                 Dispatch :: nova_routing_trie:trie()) -> lookup_result().
lookup_url(Host, Path, Method, Dispatch) ->
    nova_routing_trie:find(Host, Path, Method, Dispatch).


%%--------------------------------------------------------------------
%% @doc
%% Works the same way as add_routes/2 but with the exception that you
%% don't need to provide the routes explicitly. When using this it's
%% expected that there's a routing-module associated with the application.
%% Eg. for the application 'test' the corresponding router would then be
%% 'test_router'. Read more about routers in the official documentation.
%% @end
%%--------------------------------------------------------------------
-spec add_routes(App :: atom()) -> ok.
add_routes(App) ->
    Env = nova:get_environment(),
    add_routes(App, get_routes(router_module(App), Env)).

%%--------------------------------------------------------------------
%% @doc
%% Add routes to the dispatch-table for the given app. The routes
%% can be either a list of maps or a map. It use the same structure as
%% the routes-callback in the router-module.
%% @end
%%--------------------------------------------------------------------
-spec add_routes(App :: atom(), Routes :: [map()] | map()) -> ok.
add_routes(App, Routes) ->
    add_routes(App, Routes, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
%% @doc
%% As add_routes/2, but against the routing table addressed by `DispatchKey'.
%% @end
%%--------------------------------------------------------------------
-spec add_routes(App :: atom(), Routes :: [map()] | map(), DispatchKey :: dispatch_key()) -> ok.
add_routes(_App, [], _DispatchKey) ->
    ok;
add_routes(App, Routes, DispatchKey) when is_map(Routes) ->
    add_routes(App, [Routes], DispatchKey);
add_routes(App, [Routes|Tl], DispatchKey) when is_list(Routes) ->
    %% A list of route-lists, as produced by a router that returns several
    %% groups. Each group is compiled on its own.
    ok = insert_route_maps(App, Routes, DispatchKey),
    add_routes(App, Tl, DispatchKey);
add_routes(App, [RouteInfo|_Tl] = Routes, DispatchKey) when is_map(RouteInfo) ->
    insert_route_maps(App, Routes, DispatchKey);
add_routes(App, Routes, _DispatchKey) ->
    ?LOG_ERROR(#{reason => <<"Invalid routes structure">>, app => App, routes => Routes}),
    throw({error, {invalid_routes, App, Routes}}).

insert_route_maps(App, Routes, DispatchKey) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(DispatchKey),

    %% Take out the prefix for the app and store it in the persistent store
    AppsKey = apps_key(DispatchKey),
    CompiledApps = StorageBackend:get(AppsKey, []),
    CompiledApps0 =
        case lists:keyfind(App, 1, CompiledApps) of
            false      -> CompiledApps ++ [{App, "/"}];
            _StoredApp -> CompiledApps
        end,

    %% Routes added at runtime replace any route already registered on the
    %% same path and method, which is what the routing guide promises.
    Options = #{app => App, router_file => undefined, dispatch_key => DispatchKey,
                insert_opts => #{on_duplicate => overwrite}},

    {ok, Dispatch1, _Options0} = compile_paths(Routes, Dispatch, Options),

    StorageBackend:put(AppsKey, CompiledApps0),
    StorageBackend:put(DispatchKey, Dispatch1),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Remove all routes associated with the given application.
%% @end
%%--------------------------------------------------------------------
-spec remove_application(Application :: atom()) -> ok.
remove_application(Application) ->
    remove_application(Application, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
%% @doc
%% As remove_application/1, but against the routing table addressed by
%% `DispatchKey'.
%% @end
%%--------------------------------------------------------------------
-spec remove_application(Application :: atom(), DispatchKey :: dispatch_key()) -> ok.
remove_application(Application, DispatchKey) when is_atom(Application) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(DispatchKey),
    {ok, Dispatch0} =
        nova_routing_trie:foldl(Dispatch,
                                fun(Routes) ->
                                        [Route || Route <- Routes, route_app(Route) =/= Application]
                                end),
    AppsKey = apps_key(DispatchKey),
    StorageBackend:put(DispatchKey, Dispatch0),
    StorageBackend:put(AppsKey, lists:keydelete(Application, 1, StorageBackend:get(AppsKey, []))),
    nova:set_env(apps, lists:keydelete(Application, 1, nova:get_env(apps, []))),
    ok.

%% Both handler kinds carry the owning application, and dropping the cowboy
%% one would silently strip every websocket route.
route_app({_Host, _Path, _Method, #nova_handler_value{app = App}})   -> App;
route_app({_Host, _Path, _Method, #cowboy_handler_value{app = App}}) -> App;
route_app(_Route)                                                    -> undefined.


%%--------------------------------------------------------------------
%% @doc
%% Forget a routing table entirely. Called when the listener that owned it is
%% stopped, so its routes and compiled-application list do not outlive it.
%% The default table belongs to the bootstrap listener and is never deleted.
%% @end
%%--------------------------------------------------------------------
-spec delete_dispatch(DispatchKey :: dispatch_key()) -> ok.
delete_dispatch(?NOVA_DISPATCH) ->
    ok;
delete_dispatch(DispatchKey) ->
    case storage_backend() of
        persistent_term ->
            persistent_term:erase(DispatchKey),
            persistent_term:erase(apps_key(DispatchKey)),
            ok;
        _Backend ->
            %% A custom backend has no erase in its contract; leave it to
            %% decide its own lifecycle.
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

get_routes(Router, Env) ->
    %% Call the router
    Controllers = apply_callback(Router, controllers, [Env]),
    apply_callback(Router, routes, [Env])
        ++ lists:append([nova_controller:routes(C, Env) || C <- Controllers ]).

%% yields an empty list if callback does not exist
apply_callback(Module, Function, Args) ->
    Arity = length(Args),
    %% try to ensure callback module is loaded first
    try Module:module_info(module)
    catch _:_ -> ok
    end,
    case erlang:function_exported(Module, Function, Arity) of
        true ->
            apply(Module, Function, Args);
        false ->
            []
    end.

-spec compile(Apps :: [atom() | {atom(), map()}], Dispatch :: nova_routing_trie:trie(), Options :: map()) -> nova_routing_trie:trie().
compile([], Dispatch, _Options) -> Dispatch;
compile([{App, AppOptions}|Tl], Dispatch, GlobalOptions) ->
    %% Per-application options win over the global ones, and must not leak
    %% into the applications compiled after this one.
    Dispatch0 = compile([App], Dispatch, maps:merge(GlobalOptions, AppOptions)),
    compile(Tl, Dispatch0, GlobalOptions);
compile([App|Tl], Dispatch, Options) ->
    Router = router_module(App),

    Env = nova:get_environment(),
    Routes = get_routes(Router, Env),

    CompileParameters = Router:module_info(compile),

    RouterFile = proplists:get_value(source, CompileParameters),
    Options1 = Options#{app => App, router_file => RouterFile},

    {ok, Dispatch1, _Options2} = compile_paths(Routes, Dispatch, Options1),

    %% Take out the prefix for the app and store it in the persistent store
    StorageBackend = storage_backend(),

    CompiledApps = StorageBackend:get(apps_key(maps:get(dispatch_key, Options, ?NOVA_DISPATCH)), []),

    CompiledApps0 = lists:keystore(App, 1, CompiledApps, {App, maps:get(prefix, Options, "/")}),

    StorageBackend:put(apps_key(maps:get(dispatch_key, Options, ?NOVA_DISPATCH)), CompiledApps0),

    compile(Tl, Dispatch1, Options).

%%--------------------------------------------------------------------
%% @doc
%% The router module for an application. Either configured explicitly with
%% the `router_module' application environment key, or derived from the
%% application name using the convention for the language in use.
%% @end
%%--------------------------------------------------------------------
router_module(App) ->
    case application:get_env(App, router_module) of
        {ok, RouterModule} ->
            RouterModule;
        undefined ->
            case nova:detect_language() of
                elixir ->
                    %% We build the router as App.Router
                    erlang:list_to_atom(lists:flatten(io_lib:format("~s.Router", [App])));
                _ ->
                    %% All other languages are using the app_router convention
                    erlang:list_to_atom(lists:flatten(io_lib:format("~s_router", [App])))
            end
    end.

compile_paths([], Dispatch, Options) -> {ok, Dispatch, Options};
compile_paths([RouteInfo|Tl], Dispatch, Options) ->
    App = maps:get(app, Options),
    RouterFile = maps:get(router_file, Options),

    Plugins = resolve_plugins(maps:get(plugin_strategy, Options, local_or_global), RouteInfo, RouterFile),

    Secure =
        case maps:get(override_secure, Options, false) of
            false ->
                normalize_secure(maps:get(secure, Options, maps:get(security, RouteInfo, false)), RouterFile);
            %% The including application overrides the security callback the
            %% sub-application declared for itself.
            Override ->
                normalize_secure(Override, RouterFile)
        end,

    Value = #nova_handler_value{secure = Secure, app = App, plugins = normalize_plugins(Plugins),
                                extra_state = maps:get(extra_state, RouteInfo, #{})},

    Prefix = concat_strings(maps:get(prefix, Options, ""),
                            maps:get(prefix, RouteInfo, "")),
    Host = maps:get(host, RouteInfo, '_'),
    SubApps = maps:get(apps, RouteInfo, []),

    %% We need to add this app info to nova-env
    NovaEnv = nova:get_env(apps, []),
    NovaEnv0 = lists:keystore(App, 1, NovaEnv, {App, #{prefix => Prefix}}),
    nova:set_env(apps, NovaEnv0),

    {ok, Dispatch1} = parse_url(Host, maps:get(routes, RouteInfo, []),
                                #{prefix => Prefix,
                                  router_file => maps:get(router_file, Options),
                                  insert_opts => maps:get(insert_opts, Options, #{})},
                                Value, Dispatch),

    Dispatch2 = compile(SubApps, Dispatch1, Options#{value => Value, prefix => Prefix}),

    compile_paths(Tl, Dispatch2, Options).

parse_url(_Host, [], _Prefix, _Value, Tree) -> {ok, Tree};
parse_url(Host, [{StatusCode, Callback, Options}|Tl], T, Value, Tree) when is_integer(StatusCode) andalso
                                                                           is_function(Callback) ->
    Value0 = Value#nova_handler_value{callback = Callback},
    Res = insert_methods(maps:get(methods, Options, ['_']), Host, StatusCode, Value0, Tree,
                         insert_opts(T), fun(M) -> M end),
    parse_url(Host, Tl, T, Value, Res);
parse_url(Host, [{RemotePath, LocalPath}|Tl], T, Value = #nova_handler_value{}, Tree) when is_list(RemotePath),
                                                                                           is_list(LocalPath) ->
    parse_url(Host, [{RemotePath, LocalPath, #{}}|Tl], T, Value, Tree);
parse_url(Host, [{RemotePath, LocalPath, Options}|Tl], T = #{prefix := Prefix},
          Value = #nova_handler_value{app = App, secure = Secure}, Tree) when is_list(RemotePath), is_list(LocalPath) ->
    %% Static assets - check that the path exists
    PrivPath = filename:join(code:priv_dir(App), LocalPath),

    Payload =
        case {filelib:is_dir(LocalPath), filelib:is_dir(PrivPath)} of
            {false, false} ->
                %% No directory - check if it's a file
                case {filelib:is_file(LocalPath), filelib:is_file(PrivPath)} of
                    {false, false} ->
                        %% No dir nor file
                        ?LOG_WARNING(#{reason => <<"Could not find local path for the given resource">>,
                                       local_path => LocalPath,
                                       remote_path => RemotePath,
                                       router_file => maps:get(router_file, Options, undefined)}),
                        not_found;
                    {true, false} ->
                        {file, LocalPath};
                    {_, true} ->
                        {priv_file, App, LocalPath}
                end;
            {true, false} ->
                {dir, LocalPath};
            {_, true} ->
                {priv_dir, App, LocalPath}
        end,

    TargetFun = case Payload of
                    {file, _} -> get_file;
                    {priv_file, _, _} -> get_file;
                    {dir, _} -> get_dir;
                    {priv_dir, _, _} -> get_dir
                end,

    Value0 = #nova_handler_value{
                app = App,
                callback = fun nova_file_controller:TargetFun/1,
                extra_state = #{static => Payload, options => Options},
                plugins = Value#nova_handler_value.plugins,
                secure = Secure
               },
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree, insert_opts(T)),
    parse_url(Host, Tl, T, Value, Tree0);
parse_url(Host, [{Path, {Mod, Func}, Options}|Tl], T, Value = #nova_handler_value{app = _App, secure = _Secure}, Tree) ->
    RouterFile = maps:get(router_file, T, undefined),
    ?LOG_DEPRECATED(<<"v0.9.24">>, <<"The {Mod,Fun} format have been deprecated. Use the new format for routes.">>, RouterFile),
    parse_url(Host, [{Path, fun Mod:Func/1, Options}|Tl], T, Value, Tree);
parse_url(Host, [{Path, Callback}|Tl], T, Value, Tree) when is_function(Callback) ->
    %% Recurse with same args but with added options
    parse_url(Host, [{Path, Callback, #{}}|Tl], T, Value, Tree);
parse_url(Host, [{Path, Callback, Options}|Tl], T = #{prefix := Prefix}, Value = #nova_handler_value{app = App}, Tree)
  when is_function(Callback) ->
    case maps:get(protocol, Options, http) of
        http ->
            %% Transform the path to a string format
            RealPath = concat_strings(Prefix, Path),

            Methods = maps:get(methods, Options, ['_']),

            ExtraState = maps:get(extra_state, Options, undefined),
            Value0 = Value#nova_handler_value{extra_state = ExtraState},

            Value1 = Value0#nova_handler_value{callback = Callback},
            ?LOG_DEBUG(#{action => <<"Adding route">>, route => RealPath, app => App, methods => Methods,
                         router_file => maps:get(router_file, Options, undefined)}),
            CompiledPaths = insert_methods(Methods, Host, RealPath, Value1, Tree, insert_opts(T),
                                           fun method_to_binary/1),
            parse_url(Host, Tl, T, Value, CompiledPaths);
        OtherProtocol ->
            ?LOG_ERROR(#{reason => <<"Unknown protocol">>, protocol => OtherProtocol,
                        router_file => maps:get(router_file, Options, undefined)}),
            parse_url(Host, Tl, T, Value, Tree)
    end;
parse_url(Host, [{Path, Handler, Options = #{protocol := cowboy}}|Tl], T = #{prefix := Prefix},
          Value = #nova_handler_value{app = App, secure = Secure}, Tree) when is_atom(Handler) ->
    %% Plain cowboy handler (eg cowboy_rest, cowboy_loop or a basic
    %% cowboy_handler). The handler module is invoked with Handler:init/2
    %% and any sub-protocol upgrade it returns is honored by nova_handler.
    Value0 = #cowboy_handler_value{
                app = App,
                handler = Handler,
                arguments = maps:get(arguments, Options, #{}),
                plugins = Value#nova_handler_value.plugins,
                secure = Secure},
    ?LOG_DEBUG(#{action => <<"Adding route">>, protocol => <<"cowboy">>, route => Path, app => App,
                 router_file => maps:get(router_file, T, undefined)}),
    RealPath = concat_strings(Prefix, Path),
    CompiledPaths =
        lists:foldl(
          fun(Method, Tree0) ->
                  insert(Host, RealPath, method_to_binary(Method), Value0, Tree0)
          end, Tree, maps:get(methods, Options, ['_'])),
    parse_url(Host, Tl, T, Value, CompiledPaths);
parse_url(Host,
          [{Path, Mod, #{protocol := ws}} | Tl],
          T = #{prefix := Prefix}, #nova_handler_value{app = App, secure = Secure} = Value,
          Tree) when is_atom(Mod) ->
    Value0 =  #cowboy_handler_value{
                  app = App,
                  handler = nova_ws_handler,
                  arguments = #{module => Mod},
                  plugins = Value#nova_handler_value.plugins,
                  secure = Secure},

    ?LOG_DEBUG(#{action => <<"Adding route">>, protocol => <<"ws">>, route => Path, app => App,
                 router_file => maps:get(router_file, T, undefined)}),
    RealPath = concat_strings(Prefix, Path),
    CompiledPaths = insert(Host, RealPath, '_', Value0, Tree, insert_opts(T)),
    parse_url(Host, Tl, T, Value, CompiledPaths).


-spec render_status_page(StatusCode :: integer(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Req) ->
    render_status_page(StatusCode, #{}, Req).

-spec render_status_page(StatusCode :: integer(), Data :: map(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Data, Req) ->
    render_status_page('_', StatusCode, Data, Req, #{}).

-spec render_status_page(Host :: binary() | atom(),
                         StatusCode :: integer(),
                         Data :: map(),
                         Req :: cowboy_req:req(),
                         Env :: map()) -> {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(Host, StatusCode, Data, Req, Env) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(dispatch_key(Env)),
    {Req0, Env0} =
        case nova_routing_trie:find(Host, StatusCode, '_', Dispatch) of
            {error, _} ->
                %% Render nova page if exists - We need to determine where to find this path?
                {Req, Env#{app => nova,
                           callback => fun nova_error_controller:status_code/1,
                           secure => false,
                           controller_data => #{status => StatusCode, data => Data}}};
            {ok, Bindings, #nova_handler_value{app = App,
                                               callback = Callback,
                                               secure = Secure,
                                               extra_state = ExtraState}} ->
                {
                 Req#{extra_state => ExtraState, bindings => Bindings, resp_status_code => StatusCode},
                 Env#{app => App,
                      callback => Callback,
                      secure => Secure,
                      controller_data => #{status => StatusCode, data => Data},
                      bindings => Bindings}
                }
        end,
    {ok, Req0#{resp_status_code => StatusCode}, Env0}.


insert_opts(T) ->
    maps:get(insert_opts, T, #{}).

%% The module the dispatch table is stored in. Configurable, so it has to be
%% narrowed to a module before it can be called.
-spec storage_backend() -> module().
storage_backend() ->
    case application:get_env(nova, dispatch_backend, persistent_term) of
        Backend when is_atom(Backend) -> Backend;
        Other ->
            ?LOG_ERROR(#{reason => <<"dispatch_backend must be a module">>, value => Other}),
            persistent_term
    end.

%% The listener's dispatch key, defaulting to the one the bootstrap listener
%% uses so an Env built before multi-listener support still resolves.
dispatch_key(Env) ->
    maps:get(nova_dispatch_key, Env, ?NOVA_DISPATCH).

%% Each dispatch table keeps its own list of compiled applications.
apps_key(?NOVA_DISPATCH)  -> ?NOVA_APPS;
apps_key(DispatchKey)     -> {?NOVA_APPS, DispatchKey}.

insert_methods([], _Host, _Path, _Value, Tree, _Options, _ToComparator) ->
    Tree;
insert_methods([Method|Tl], Host, Path, Value, Tree, Options, ToComparator) ->
    Tree0 = insert(Host, Path, ToComparator(Method), Value, Tree, Options),
    insert_methods(Tl, Host, Path, Value, Tree0, Options, ToComparator).

insert(Host, Path, Combinator, Value, Tree, Options) ->
    try nova_routing_trie:insert(Host, Path, Combinator, Value, Tree, Options) of
        {ok, Tree0} ->
            Tree0;
        {error, conflict, Conflict} ->
            ?LOG_ERROR(#{reason => <<"Conflicting route">>, route => Path, combinator => Combinator,
                         conflict => Conflict}),
            throw({error, {route_conflict, Conflict}})
    catch
        throw:Exception ->
            ?LOG_ERROR(#{reason => <<"Error when inserting route">>, route => Path, combinator => Combinator}),
            throw(Exception);
        Type:Exception ->
            ?LOG_ERROR(#{reason => <<"Unexpected exit">>, type => Type, exception => Exception}),
            throw(Exception)
    end.


add_plugin(Plugin) ->
    StorageBackend = storage_backend(),
    StoredPlugins = StorageBackend:get(?NOVA_PLUGINS, []),
    Plugins1 = lists:umerge([[Plugin], StoredPlugins]),
    case Plugins1 of
        StoredPlugins ->
            ok;
        _ ->
            StorageBackend:put(?NOVA_PLUGINS, Plugins1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Work out which plugins apply to a route entry.
%%
%% `local_or_global' is the default and is how Nova has always behaved: a
%% route entry that declares `plugins' uses exactly those, otherwise it uses
%% the globally configured ones. The merging strategies exist for the cases
%% where you want both, and dedupe on `{Type, Module}' keeping the first
%% occurrence, so ordering within a phase is preserved.
%% @end
%%--------------------------------------------------------------------
normalize_secure(false, _RouterFile) ->
    false;
normalize_secure(true, RouterFile) ->
    ?LOG_ERROR(#{reason => <<"'secure' must be false, a fun/1 or {Mod, Fun}. Ignoring 'true'.">>,
                 router_file => RouterFile}),
    false;
normalize_secure({SMod, SFun}, RouterFile) when is_atom(SMod), is_atom(SFun) ->
    ?LOG_DEPRECATED(<<"v0.9.24">>, <<"The {Mod,Fun} format have been deprecated for the 'secure'-section of a route table. Use the new format for routes.">>, RouterFile),
    fun SMod:SFun/1;
normalize_secure(SCallback, _RouterFile) ->
    SCallback.

resolve_plugins(local_or_global, RouteInfo, _RouterFile) ->
    maps:get(plugins, RouteInfo, global_plugins());
resolve_plugins(local_first, RouteInfo, _RouterFile) ->
    dedupe_plugins(local_plugins(RouteInfo) ++ global_plugins());
resolve_plugins(global_first, RouteInfo, _RouterFile) ->
    dedupe_plugins(global_plugins() ++ local_plugins(RouteInfo));
resolve_plugins(local_only, RouteInfo, _RouterFile) ->
    local_plugins(RouteInfo);
resolve_plugins(global_only, _RouteInfo, _RouterFile) ->
    global_plugins();
resolve_plugins({override, PluginList}, _RouteInfo, _RouterFile) when is_list(PluginList) ->
    PluginList;
resolve_plugins(Strategy, RouteInfo, RouterFile) ->
    ?LOG_ERROR(#{reason => <<"Unknown plugin_strategy, falling back to local_or_global">>,
                 plugin_strategy => Strategy, router_file => RouterFile}),
    resolve_plugins(local_or_global, RouteInfo, RouterFile).

local_plugins(RouteInfo) ->
    maps:get(plugins, RouteInfo, []).

global_plugins() ->
    application:get_env(nova, plugins, []).

dedupe_plugins(Plugins) ->
    dedupe_plugins(Plugins, [], []).

dedupe_plugins([], _Seen, Acc) ->
    lists:reverse(Acc);
dedupe_plugins([Plugin|Tl], Seen, Acc) ->
    Key = plugin_key(Plugin),
    case lists:member(Key, Seen) of
        true  -> dedupe_plugins(Tl, Seen, Acc);
        false -> dedupe_plugins(Tl, [Key|Seen], [Plugin|Acc])
    end.

plugin_key({Type, PluginName, _Options}) -> {Type, PluginName};
plugin_key(Plugin)                       -> Plugin.

normalize_plugins(Plugins) ->
    NormalizedPlugins = normalize_plugins(Plugins, []),
    [{Type, lists:reverse(TypePlugins)} || {Type, TypePlugins} <- NormalizedPlugins].

normalize_plugins([], Ack) -> Ack;
normalize_plugins([{Type, PluginName, Options}|Tl], Ack) ->
    ExistingPlugins = proplists:get_value(Type, Ack, []),
    add_plugin(PluginName),
    normalize_plugins(Tl, [{Type, [{fun PluginName:Type/4, Options}|ExistingPlugins]}|proplists:delete(Type, Ack)]).

method_to_binary(get) -> <<"GET">>;
method_to_binary(post) -> <<"POST">>;
method_to_binary(put) -> <<"PUT">>;
method_to_binary(delete) -> <<"DELETE">>;
method_to_binary(options) -> <<"OPTIONS">>;
method_to_binary(head) -> <<"HEAD">>;
method_to_binary(connect) -> <<"CONNECT">>;
method_to_binary(trace) -> <<"TRACE">>;
method_to_binary(patch) -> <<"PATCH">>;
method_to_binary(_) -> '_'.

concat_strings(Path1, Path2) when is_binary(Path1) ->
    concat_strings(unicode:characters_to_list(uri_string:unquote(Path1)), Path2);
concat_strings(Path1, Path2) when is_binary(Path2) ->
    concat_strings(Path1, unicode:characters_to_list(uri_string:unquote(Path2)));
concat_strings(_Path1, Path2) when is_integer(Path2) ->
    Path2;
concat_strings(Path1, Path2) when is_list(Path1), is_list(Path2) ->
    string:concat(Path1, Path2).

%% ============================
%% Callbacks for nova_router
%% ===========================
-spec routes(Env :: atom()) -> [map()].
routes(_) ->
 [#{
    routes => [
               {404, fun nova_error_controller:not_found/1, #{}},
               {500, fun nova_error_controller:server_error/1, #{}}
              ]
   }].

%% =============================
%% Test cases
%% ============================
-ifdef(TEST).
-compile(export_all).
-endif.
