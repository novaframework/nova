%%%-------------------------------------------------------------------
%%% Author: Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_router).
-moduledoc "Router module for nova. This module is responsible for compiling routes, dispatching requests to the correct handler and managing the routing table. It also exposes an API for modulating the routing table at runtime.".
-behaviour(cowboy_middleware).

%% Cowboy middleware-callbacks
-export([
         execute/2
        ]).

%% API
-export([
         compile/1,
         compile/2,
         compile_fresh/2,
         overrides_in_force/1,
         secures_in_force/1,
         lookup_url/1,
         lookup_url/2,
         lookup_url/3,
         lookup_url/4,
         dispatch_key/1,
         canonical_path/1,
         render_status_page/2,
         render_status_page/3,
         render_status_page/5,

         %% Expose the router-callback
         routes/1,

         %% Fetch information about the routing table
         plugins/0,
         forget_plugins/0,
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

-ifdef(TEST).
-export([add_plugin/1, apply_callback/3, concat_strings/2, insert/6, method_to_binary/1, normalize_plugins/2,
         normalize_secure/2, plugins_after_security/2, resolve_secure/3, router_module/1]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_router.hrl").
-include("../include/nova.hrl").

-type bindings() :: #{binary() => binary()}.

%% A request the router has matched. Its bindings are keyed by binary, which
%% cowboy_req:req() has no room for, so it is typed as a map.
-type routed_req() :: map().

-type lookup_result() :: {ok, bindings(), Value :: term()} |
                         {ok, bindings(), Value :: term(), PathInfo :: [binary()]} |
                         {error, not_found} |
                         {error, comparator_not_found, AllowedMethods :: [nova_routing_trie:comparator()]}.

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

-doc false.
-spec forget_plugins() -> ok.
forget_plugins() ->
    StorageBackend = storage_backend(),
    StorageBackend:put(?NOVA_PLUGINS, []),
    ok.

-spec compile(Apps :: [atom() | {atom(), map()}]) -> nova_routing_trie:trie().
compile(Apps) ->
    compile(Apps, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
-doc "Compile the given applications into the routing table addressed by `DispatchKey`, merging into whatever is already stored there.".
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

%%--------------------------------------------------------------------
%% As compile/2, but starting from an empty routing table that replaces
%% whatever is stored under DispatchKey. For nova_sup: a listener that is
%% starting uses this, so routes and security callbacks from an earlier run
%% in the same VM are not carried over.
-doc false.
-spec compile_fresh(Apps :: [atom() | {atom(), map()}], DispatchKey :: dispatch_key()) ->
          nova_routing_trie:trie().
compile_fresh(Apps, DispatchKey) ->
    UseStrict = application:get_env(nova, use_strict_routing, false),
    StorageBackend = storage_backend(),
    StorageBackend:put(apps_key(DispatchKey), []),
    Dispatch = compile(Apps, nova_routing_trie:new(#{strict => UseStrict}),
                       #{dispatch_key => DispatchKey}),
    StorageBackend:put(DispatchKey, Dispatch),
    Dispatch.

-spec execute(Req, Env :: cowboy_middleware:env()) -> {ok, Req, Env0} | {stop, Req}
                                                          when Req::cowboy_req:req(),
                                                               Env0::cowboy_middleware:env().
execute(Req0 = #{host := Host, path := Path, method := Method}, Env) ->
    StorageBackend = storage_backend(),
    DispatchKey = dispatch_key(Env),
    %% nova_session only tells nova_stream_h about session changes made by the
    %% process serving the request.
    Req = Req0#{nova_dispatch_key => DispatchKey, nova_session_owner => self()},
    Dispatch = StorageBackend:get(DispatchKey),
    case nova_routing_trie:find(Host, Path, Method, Dispatch) of
        {error, not_found} ->
            logger:debug(<<"Path ~p not found for ~p in ~p">>, [Path, Method, Host]),
            render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {error, comparator_not_found, AllowedMethods} ->
            logger:debug(<<"Method not allowed: ~p for ~p. Allowed methods: ~p">>, [Method, Path, AllowedMethods]),
            Allowed = [AllowedMethod || AllowedMethod <- AllowedMethods, is_binary(AllowedMethod)],
            AllowHeader = iolist_to_binary(lists:join(<<", ">>, Allowed)),
            %% Set the 'allow'-header
            Req1 = cowboy_req:set_resp_header(<<"allow">>, AllowHeader, Req),
            {ok, Req2, Env1} = render_status_page('_', 405, #{error => "Method not allowed"}, Req1, Env),
            {ok, with_preflight_plugins(Req2, Host, Path, Dispatch), Env1};
        {ok, Bindings, #nova_handler_value{app = App, callback = Callback, secure = Secure, plugins = RoutePlugins,
                                           extra_state = ExtraState}} ->
            {Route, Plugins} = take_route(RoutePlugins),
            {ok,
             put_route(Route, Req#{plugins => Plugins,
                                   extra_state => ExtraState,
                                   bindings => Bindings}),
             Env#{app => App,
                  callback => Callback,
                  secure => Secure,
                  controller_data => #{}
                 }
            };
        {ok, Bindings, #nova_handler_value{app = App, callback = Callback,
                                           secure = Secure, plugins = RoutePlugins, extra_state = ExtraState}, Pathinfo} ->
            {Route, Plugins} = take_route(RoutePlugins),
            {ok,
             put_route(Route, Req#{plugins => Plugins,
                                   extra_state => with_pathinfo(ExtraState, Pathinfo),
                                   bindings => Bindings}),
             Env#{app => App,
                  callback => Callback,
                  secure => Secure,
                  controller_data => #{}
                 }
            };
        {ok, Bindings, #cowboy_handler_value{app = App, handler = Handler, arguments = Args,
                                              plugins = RoutePlugins, secure = Secure}} ->
            {Route, Plugins} = take_route(RoutePlugins),
            {ok,
             put_route(Route, put_extra_state(handler_extra_state(Args),
                                              Req#{plugins => Plugins,
                                                   bindings => Bindings})),
             Env#{app => App,
                  cowboy_handler => Handler,
                  arguments => Args,
                  secure => Secure
                 }
            };
        {ok, Bindings, #cowboy_handler_value{app = App, handler = Handler, arguments = Args,
                                              plugins = RoutePlugins, secure = Secure}, Pathinfo} ->
            {Route, Plugins} = take_route(RoutePlugins),
            {ok,
             put_route(Route, Req#{plugins => Plugins,
                                   extra_state => with_pathinfo(handler_extra_state(Args), Pathinfo),
                                   bindings => Bindings}),
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

%%--------------------------------------------------------------------
-doc "Look up a path in the routing table of the default listener, for any host and method. Use lookup_url/4 for the table of another listener.".
-spec lookup_url(Path :: nova_routing_trie:path()) -> lookup_result().
lookup_url(Path) ->
    lookup_url('_', Path).

-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path()) -> lookup_result().
lookup_url(Host, Path) ->
    lookup_url(Host, Path, '_').

-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path(),
                 Method :: nova_routing_trie:comparator()) -> lookup_result().
lookup_url(Host, Path, Method) ->
    lookup_url(Host, Path, Method, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
-doc "As lookup_url/3, but in the routing table addressed by `DispatchKey`. dispatch_key/1 gives the key of the listener a request came in on.".
-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path(),
                 Method :: nova_routing_trie:comparator(),
                 DispatchKey :: dispatch_key()) -> lookup_result().
lookup_url(Host, Path, Method, DispatchKey) ->
    StorageBackend = storage_backend(),
    nova_routing_trie:find(Host, Path, Method, StorageBackend:get(DispatchKey)).

%%--------------------------------------------------------------------
-doc """
The path a request is routed on, as opposed to the `path` of the request, which is what the client sent. Empty segments are dropped and `.` and `..` are resolved, so `/public/../admin//users` gives `/admin/users`.

A plugin or security callback that decides by path must look at this one. Compare whole segments, not bytes: `/api` is not a prefix of `/apikeys`.
""".
-spec canonical_path(Req :: cowboy_req:req() | binary()) -> binary().
canonical_path(#{path := Path}) ->
    canonical_path(Path);
canonical_path(Path) when is_binary(Path) ->
    case nova_routing_trie:canonical_segments(Path) of
        [] -> <<"/">>;
        Segments -> iolist_to_binary([[$/, Segment] || Segment <- Segments])
    end.

%%--------------------------------------------------------------------
-doc "Works the same way as add_routes/2 but with the exception that you don't need to provide the routes explicitly. When using this it's expected that there's a routing-module associated with the application. Eg. for the application 'test' the corresponding router would then be 'test_router'. Read more about routers in the official documentation.".
-spec add_routes(App :: atom()) -> ok.
add_routes(App) ->
    Env = nova:get_environment(),
    add_routes(App, get_routes(router_module(App), Env)).

%%--------------------------------------------------------------------
-doc "Add routes to the dispatch-table for the given app. The routes can be either a list of maps or a map. It use the same structure as the routes-callback in the router-module.".
-spec add_routes(App :: atom(), Routes :: [map() | [map()]] | map()) -> ok.
add_routes(App, Routes) ->
    add_routes(App, Routes, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
-doc "As add_routes/2, but against the routing table addressed by `DispatchKey`.".
-spec add_routes(App :: atom(), Routes :: [map() | [map()]] | map(), DispatchKey :: dispatch_key()) -> ok.
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
-doc "Remove all routes associated with the given application.".
-spec remove_application(Application :: atom()) -> ok.
remove_application(Application) ->
    remove_application(Application, ?NOVA_DISPATCH).

%%--------------------------------------------------------------------
-doc "As remove_application/1, but against the routing table addressed by `DispatchKey`.".
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
    nova:set_env(apps, lists:keydelete(Application, 1, mounted_apps())),
    ok.

%% Both handler kinds carry the owning application, and dropping the cowboy
%% one would silently strip every websocket route.
route_app({_Host, _Path, _Method, #nova_handler_value{app = App}})   -> App;
route_app({_Host, _Path, _Method, #cowboy_handler_value{app = App}}) -> App;
route_app(_Route)                                                    -> undefined.


%%--------------------------------------------------------------------
-doc "Forget a routing table entirely. Called when the listener that owned it is stopped, so its routes and compiled-application list do not outlive it. The default table belongs to the bootstrap listener and is never deleted.".
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
    ok = misplaced_security([{security, Value, mount_path(GlobalOptions, AppOptions)}
                             || security := Value <- AppOptions],
                            App, maps:get(router_file, GlobalOptions, undefined)),
    Dispatch0 = compile([App], Dispatch, mount_options(GlobalOptions, AppOptions)),
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

%% The secure and the override_secure that are in force around an
%% application are the options its own entry does not replace. They stay in
%% force, in front of those of the entry.
mount_options(GlobalOptions, AppOptions) ->
    Options = maps:merge(GlobalOptions, AppOptions),
    Options#{secure => maps:get(secure, AppOptions, false),
             inherited_secure => secures_in_force(GlobalOptions) ++ maps:get(inherited_secure, AppOptions, []),
             override_secure => maps:get(override_secure, AppOptions, false),
             inherited_override_secure => overrides_in_force(GlobalOptions) ++
                 maps:get(inherited_override_secure, AppOptions, [])}.

mount_path(GlobalOptions, AppOptions) ->
    case concat_strings(maps:get(prefix, GlobalOptions, ""), maps:get(prefix, AppOptions, "")) of
        "" -> "/";
        Path -> Path
    end.

%%--------------------------------------------------------------------
%% The secure values that apply to an application mounted with Options,
%% outermost first, as they were written. For nova_sup, which hands them to
%% the applications that one mounts in turn.
-doc false.
-spec secures_in_force(Options :: map()) -> [term()].
secures_in_force(Options) ->
    maps:get(inherited_secure, Options, []) ++
        [Secure || Secure <- [maps:get(secure, Options, false)], Secure =/= false].

%%--------------------------------------------------------------------
%% The override_secure values that apply to an application mounted with
%% Options, outermost first, as they were written. For nova_sup, which hands
%% them to the applications that one mounts in turn.
-doc false.
-spec overrides_in_force(Options :: map()) -> [term()].
overrides_in_force(Options) ->
    maps:get(inherited_override_secure, Options, []) ++
        [Override || Override <- [maps:get(override_secure, Options, false)], Override =/= false].

%%--------------------------------------------------------------------
-doc "The router module for an application. Either configured explicitly with the `router_module` application environment key, or derived from the application name using the convention for the language in use.".
router_module(App) ->
    case application:get_env(App, router_module) of
        {ok, RouterModule} ->
            RouterModule;
        undefined ->
            Names = router_names(nova:detect_language(), atom_to_list(App)),
            case first_module(Names) of
                {ok, RouterModule} ->
                    RouterModule;
                error ->
                    ?LOG_ERROR(#{reason => <<"Could not find the router module of the application">>,
                                 app => App, router_module => Names}),
                    throw({error, {router_module_not_found, App}})
            end
    end.

%% An Elixir router is App.Router. `mix nova.new` names the application by
%% its module, 'Elixir.MyApp', and an application named as OTP names it,
%% my_app, has its router in the module Elixir derives from that name.
%% All other languages use the app_router convention.
router_names(elixir, App) ->
    [App ++ ".Router", "Elixir." ++ camelised(App) ++ ".Router"];
router_names(_Language, App) ->
    [App ++ "_router"].

%% my_app is MyApp.
camelised([]) ->
    [];
camelised([$_ | Rest]) ->
    camelised(Rest);
camelised([First | Rest]) ->
    {Part, Parts} = lists:splitwith(fun(Char) -> Char =/= $_ end, Rest),
    [upper(First) | Part] ++ camelised(Parts).

upper(Char) when Char >= $a, Char =< $z -> Char - ($a - $A);
upper(Char) -> Char.

%% A name can be an atom without being a module, so the module is loaded
%% before it is taken for the router.
first_module([]) ->
    error;
first_module([Name | Names]) ->
    case nova_code:module_named(Name) of
        {ok, Module} ->
            case code:ensure_loaded(Module) of
                {module, Module} -> {ok, Module};
                {error, _Reason} -> first_module(Names)
            end;
        error ->
            first_module(Names)
    end.

compile_paths([], Dispatch, Options) -> {ok, Dispatch, Options};
compile_paths([RouteInfo|Tl], Dispatch, Options) ->
    App = maps:get(app, Options),
    RouterFile = maps:get(router_file, Options),

    ok = warn_route_plugin_strategy(App, RouteInfo, RouterFile),
    Plugins = resolve_plugins(maps:get(plugin_strategy, Options, local_or_global), RouteInfo, RouterFile),

    Secure = resolve_secure(Options, RouteInfo, RouterFile),

    AfterSecurity = plugins_after_security(RouteInfo, RouterFile),
    Value = #nova_handler_value{secure = Secure, app = App, plugins = normalize_plugins(Plugins, AfterSecurity),
                                extra_state = maps:get(extra_state, RouteInfo, #{})},

    Prefix = concat_strings(maps:get(prefix, Options, ""),
                            maps:get(prefix, RouteInfo, "")),
    Host = maps:get(host, RouteInfo, '_'),
    SubApps = maps:get(apps, RouteInfo, []),
    ok = misplaced_security(misplaced_in_group(Prefix, RouteInfo), App, RouterFile),

    %% We need to add this app info to nova-env
    nova:set_env(apps, lists:keystore(App, 1, mounted_apps(), {App, #{prefix => Prefix}})),

    Group = maps:with([extra_state], RouteInfo),
    {ok, Dispatch1} = parse_url(Host, maps:get(routes, RouteInfo, []),
                                Group#{prefix => Prefix,
                                       router_file => maps:get(router_file, Options),
                                       insert_opts => maps:get(insert_opts, Options, #{})},
                                Value, Dispatch),

    %% The applications the group lists are behind its security as well.
    Dispatch2 = compile(SubApps, Dispatch1,
                        Options#{value => Value, prefix => Prefix, secure => false,
                                 inherited_secure => secures_in_force(Options) ++
                                     [Security || security := Security <- RouteInfo, Security =/= false]}),

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
                        ?LOG_ERROR(#{reason => <<"Could not find local path for the given resource">>,
                                     local_path => LocalPath,
                                     remote_path => RemotePath,
                                     router_file => maps:get(router_file, Options, undefined)}),
                        throw({error, {static_path_not_found, LocalPath}});
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

    RealPath = string:concat(Prefix, RemotePath),
    Value0 = #nova_handler_value{
                app = App,
                callback = fun nova_file_controller:TargetFun/1,
                extra_state = nova_file_controller:static_state(Payload, Options),
                plugins = with_route(RealPath, Value#nova_handler_value.plugins),
                secure = Secure
               },
    Tree0 = insert(Host, RealPath, '_', Value0, Tree, insert_opts(T)),
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

            ExtraState = route_extra_state(Options, T),
            Value0 = Value#nova_handler_value{extra_state = ExtraState},

            Value1 = Value0#nova_handler_value{callback = Callback,
                                               plugins = with_route(RealPath, Value#nova_handler_value.plugins)},
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
parse_url(Host,
          [{Path, Mod, #{protocol := ws} = Options} | Tl],
          T = #{prefix := Prefix}, #nova_handler_value{app = App, secure = Secure} = Value,
          Tree) when is_atom(Mod) ->
    RealPath = concat_strings(Prefix, Path),
    Value0 =  #cowboy_handler_value{
                  app = App,
                  handler = nova_ws_handler,
                  arguments = websocket_arguments(Mod, App, Options, T),
                  plugins = with_route(RealPath, Value#nova_handler_value.plugins),
                  secure = Secure},

    ?LOG_DEBUG(#{action => <<"Adding route">>, protocol => <<"ws">>, route => Path, app => App,
                 router_file => maps:get(router_file, T, undefined)}),
    CompiledPaths = insert(Host, RealPath, '_', Value0, Tree, insert_opts(T)),
    parse_url(Host, Tl, T, Value, CompiledPaths).


-spec render_status_page(StatusCode :: integer(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: routed_req(), Env :: map()}.
render_status_page(StatusCode, Req) ->
    render_status_page(StatusCode, #{}, Req).

-spec render_status_page(StatusCode :: integer(), Data :: map(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: routed_req(), Env :: map()}.
render_status_page(StatusCode, Data, Req) ->
    render_status_page('_', StatusCode, Data, Req, #{nova_dispatch_key => dispatch_key(Req)}).

-spec render_status_page(Host :: binary() | atom(),
                         StatusCode :: integer(),
                         Data :: map(),
                         Req :: cowboy_req:req(),
                         Env :: map()) -> {ok, Req0 :: routed_req(), Env :: map()}.
render_status_page(Host, StatusCode, Data, Req, Env) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(dispatch_key(Env)),
    %% A status page is rendered by a controller. Left in place, the handler
    %% of a websocket route would run instead and upgrade the connection.
    Env1 = maps:without([cowboy_handler, arguments], Env),
    {Req0, Env0} =
        case nova_routing_trie:find(Host, StatusCode, '_', Dispatch) of
            {error, _} ->
                %% Render nova page if exists - We need to determine where to find this path?
                {Req, Env1#{app => nova,
                           callback => fun nova_error_controller:status_code/1,
                           secure => false,
                           controller_data => #{status => StatusCode, data => Data}}};
            {ok, Bindings, #nova_handler_value{app = App,
                                               callback = Callback,
                                               secure = Secure,
                                               extra_state = ExtraState}} ->
                {
                 Req#{extra_state => ExtraState, bindings => Bindings, resp_status_code => StatusCode},
                 Env1#{app => App,
                      callback => Callback,
                      secure => Secure,
                      controller_data => #{status => StatusCode, data => Data},
                      bindings => Bindings}
                }
        end,
    {ok, Req0#{resp_status_code => StatusCode}, Env0}.


insert_opts(T) ->
    maps:get(insert_opts, T, #{}).

-spec mounted_apps() -> [{atom(), map()}].
mounted_apps() ->
    case nova:get_env(apps, []) of
        Apps when is_list(Apps) ->
            [{App, Info} || {App, Info} <- Apps, is_atom(App), is_map(Info)]
    end.

%% The route template rides at the head of the compiled plugin list so the
%% routing records keep their arity: rebar3_nova and other tools match them
%% positionally. execute/2 takes it off again before the list reaches the Req.
with_route(RealPath, Plugins) ->
    [{nova_route, unicode:characters_to_binary(RealPath)} | Plugins].

take_route([{nova_route, Route} | Plugins]) -> {Route, Plugins};
take_route(Plugins)                          -> {undefined, Plugins}.

put_route(undefined, Req) -> Req;
put_route(Route, Req)     -> Req#{nova_route => Route}.

%% A CORS preflight asks, with OPTIONS, about a request it is going to make
%% with another method. A route that does not take OPTIONS still has to let
%% the plugins of the route for that method answer, or the browser never
%% sends the request. They run before the 405 page, which stays the answer
%% when none of them stops the request.
with_preflight_plugins(Req = #{method := ~"OPTIONS"}, Host, Path, Dispatch) ->
    case cowboy_req:header(~"origin", Req) =/= undefined andalso
        cowboy_req:header(~"access-control-request-method", Req) of
        Requested when is_binary(Requested) ->
            case route_plugins(nova_routing_trie:find(Host, Path, Requested, Dispatch)) of
                undefined ->
                    Req;
                RoutePlugins ->
                    {_Route, Plugins} = take_route(RoutePlugins),
                    Req#{plugins => proplists:delete(after_security, Plugins)}
            end;
        _ ->
            Req
    end;
with_preflight_plugins(Req, _Host, _Path, _Dispatch) ->
    Req.

route_plugins({ok, _Bindings, Value})            -> route_plugins(Value);
route_plugins({ok, _Bindings, Value, _Pathinfo}) -> route_plugins(Value);
route_plugins(#nova_handler_value{plugins = Plugins})   -> Plugins;
route_plugins(#cowboy_handler_value{plugins = Plugins}) -> Plugins;
route_plugins(_NoRoute)                          -> undefined.

%% A route without an extra_state of its own takes the one its route group
%% declares, an empty map included, and has undefined when the group
%% declares none.
route_extra_state(#{extra_state := ExtraState}, _Group) -> ExtraState;
route_extra_state(_Options, #{extra_state := ExtraState}) -> ExtraState;
route_extra_state(_Options, _Group) -> undefined.

%% The routing record of a websocket route has no field for extra_state and
%% has to keep its arity, so the state travels in the handler arguments: the
%% one the route declares, or else the one its route group declares.
websocket_arguments(Mod, App, #{extra_state := ExtraState}, _Group) ->
    #{module => Mod, app => App, extra_state => ExtraState};
websocket_arguments(Mod, App, _Options, #{extra_state := ExtraState}) ->
    #{module => Mod, app => App, extra_state => ExtraState};
websocket_arguments(Mod, App, _Options, _Group) ->
    #{module => Mod, app => App}.

handler_extra_state(#{extra_state := ExtraState}) -> ExtraState;
handler_extra_state(_Arguments)                   -> undefined.

put_extra_state(undefined, Req)  -> Req;
put_extra_state(ExtraState, Req) -> Req#{extra_state => ExtraState}.

%% A route declared without extra_state carries undefined, and one may carry
%% any term. Only a map can take the catch-all's trailing segments.
with_pathinfo(ExtraState, Pathinfo) when is_map(ExtraState) ->
    ExtraState#{pathinfo => Pathinfo};
with_pathinfo(undefined, Pathinfo) ->
    #{pathinfo => Pathinfo};
with_pathinfo(ExtraState, _Pathinfo) ->
    ExtraState.

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

%%--------------------------------------------------------------------
-doc "The key of the routing table serving a request, for the functions that take a `DispatchKey`. Takes the request, which carries the key once it has passed the router, or the middleware environment where that is at hand. Anything else gives the key of the default listener.".
-spec dispatch_key(ReqOrEnv :: cowboy_req:req() | cowboy_middleware:env()) -> dispatch_key().
dispatch_key(#{nova_dispatch_key := DispatchKey}) ->
    DispatchKey;
dispatch_key(_) ->
    ?NOVA_DISPATCH.

%% Each dispatch table keeps its own list of compiled applications.
apps_key(?NOVA_DISPATCH)  -> ?NOVA_APPS;
apps_key(DispatchKey)     -> {?NOVA_APPS, DispatchKey}.

insert_methods([], _Host, _Path, _Value, Tree, _Options, _ToComparator) ->
    Tree;
insert_methods([Method|Tl], Host, Path, Value, Tree, Options, ToComparator) ->
    Comparator =
        try ToComparator(Method)
        catch
            throw:{error, {invalid_method, _}} = Error ->
                ?LOG_ERROR(#{reason => <<"Unknown HTTP method in the methods of a route. Use get, post, put, delete, options, head, connect, trace, patch or '_', or a binary or string for any other method.">>,
                             route => Path, method => Method}),
                throw(Error)
        end,
    Tree0 = insert(Host, Path, Comparator, Value, Tree, Options),
    insert_methods(Tl, Host, Path, Value, Tree0, Options, ToComparator).

insert(Host, Path, Combinator, Value, Tree, Options) ->
    App = route_app({Host, Path, Combinator, Value}),
    try nova_routing_trie:insert(Host, Path, Combinator, Value, Tree, Options#{app => App}) of
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
    end,
    nova_plugin_manager:ensure_plugin(Plugin).

normalize_secure(false, _RouterFile) ->
    false;
normalize_secure({SMod, SFun}, RouterFile) when is_atom(SMod), is_atom(SFun) ->
    ?LOG_DEPRECATED(<<"v0.9.24">>, <<"The {Mod,Fun} format have been deprecated for the 'secure'-section of a route table. Use the new format for routes.">>, RouterFile),
    fun SMod:SFun/1;
normalize_secure(SCallback, _RouterFile) when is_function(SCallback, 1) ->
    SCallback;
normalize_secure(Invalid, RouterFile) ->
    ?LOG_ERROR(#{reason => <<"A security value must be false, a fun/1 or {Mod, Fun}.">>,
                 value => Invalid, router_file => RouterFile}),
    throw({error, {invalid_security, Invalid}}).

%% An application mounted with `secure` has that callback in front of every
%% route group, and a group that declares `security` has its own behind it:
%% all of them have to allow a request, so neither the application that
%% mounts nor the one that is mounted can take away what the other one asks
%% for. The secure of the applications this one was mounted through, and
%% the security of a group that lists it under `apps`, come first, outermost
%% first. Every value is checked here, also where it does not end up on the
%% route because an `override_secure` replaces them: that is the explicit
%% way to replace a declared callback.
resolve_secure(Options, RouteInfo, RouterFile) ->
    Overrides = override_callbacks(Options, RouterFile),
    Declared = [normalize_secure(Secure, RouterFile) || Secure <- secures_in_force(Options)] ++
        [normalize_secure(maps:get(security, RouteInfo, false), RouterFile)],
    Callbacks = case Overrides of
                    [] -> without_repeats([Callback || Callback <- Declared, Callback =/= false]);
                    _ -> Overrides
                end,
    case Callbacks of
        [] -> false;
        [Callback] -> Callback;
        _Several -> Callbacks
    end.

%% Every override_secure in force has to allow a request: those of the
%% applications this one was mounted through first, then its own.
override_callbacks(Options, RouterFile) ->
    Callbacks = [normalize_secure(Override, RouterFile) || Override <- overrides_in_force(Options)],
    without_repeats([Callback || Callback <- Callbacks, Callback =/= false]).

without_repeats([Callback | Rest]) ->
    [Callback | without_repeats([Other || Other <- Rest, Other =/= Callback])];
without_repeats([]) ->
    [].

%%--------------------------------------------------------------------
-doc """
Work out which plugins apply to a route entry.

`local_or_global` is the default and is how Nova has always behaved: a route entry that declares `plugins` uses exactly those, otherwise it uses the globally configured ones. The merging strategies exist for the cases where you want both, and dedupe on `{Type, Module}` keeping the first occurrence, so ordering within a phase is preserved.
""".
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

%% The routing guide for 0.17.0 to 0.18.1 put plugin_strategy on the route
%% entry, where it was never read. Once per application, so a router that is
%% compiled again does not log again.
warn_route_plugin_strategy(App, #{plugin_strategy := Strategy}, RouterFile) ->
    Key = {?MODULE, route_plugin_strategy_warned, App},
    case nova_once:first(Key) of
        true ->
            ?LOG_WARNING(#{msg => ~"plugin_strategy on a route entry is ignored. Set it in the application's nova_apps options instead: {nova_apps, [{App, #{plugin_strategy => Strategy}}]}",
                           app => App, plugin_strategy => Strategy, router_file => RouterFile});
        false ->
            ok
    end;
warn_route_plugin_strategy(_App, _RouteInfo, _RouterFile) ->
    ok.

%% A route group is protected by its `security`, and a mounted application
%% by the `secure` of its entry. The same wish written as `secure` on the
%% group, as either key in the options of one route, or as `security` on the
%% entry, is never read. Written as a callback, the route its author
%% believes protected would be public, so the routes are refused.
misplaced_in_group(Prefix, RouteInfo) ->
    [{secure, Value, Prefix} || secure := Value <- RouteInfo] ++
        [{Key, maps:get(Key, RouteOptions), concat_strings(Prefix, Path)}
         || {Path, _Target, RouteOptions} <- maps:get(routes, RouteInfo, []), is_map(RouteOptions),
            Key <- [security, secure], is_map_key(Key, RouteOptions)].

misplaced_security(Misplaced, App, RouterFile) ->
    _ = [?LOG_WARNING(#{msg => ~"This key is not read here, so it protects nothing. Declare a callback as security on the route group, or as secure on the nova_apps entry of the application",
                        app => App, path => Path, key => Key, router_file => RouterFile})
         || {Key, false, Path} <- Misplaced],
    case [{Key, Path} || {Key, Value, Path} <- Misplaced, Value =/= false] of
        [] ->
            ok;
        [{Key, Path} | _] = Refused ->
            _ = [?LOG_ERROR(#{msg => ~"This key is not read here, so the callback would protect nothing. Declare it as security on the route group, or as secure on the nova_apps entry of the application",
                              app => App, path => RefusedPath, key => RefusedKey, router_file => RouterFile})
                 || {RefusedKey, RefusedPath} <- Refused],
            throw({error, {misplaced_security, App, Key, Path}})
    end.

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

normalize_plugins(Plugins, AfterSecurity) ->
    NormalizedPlugins = normalize_plugins(Plugins, AfterSecurity, []),
    [{Phase, lists:reverse(PhasePlugins)} || {Phase, PhasePlugins} <- NormalizedPlugins].

normalize_plugins([], _AfterSecurity, Ack) -> Ack;
normalize_plugins([{Type, PluginName, Options}|Tl], AfterSecurity, Ack) ->
    Phase = plugin_phase(Type, PluginName, AfterSecurity),
    ExistingPlugins = lists:append([PhasePlugins || {Existing, PhasePlugins} <- Ack, Existing =:= Phase]),
    add_plugin(PluginName),
    normalize_plugins(Tl, AfterSecurity,
                      [{Phase, [{fun PluginName:Type/4, Options}|ExistingPlugins]}|proplists:delete(Phase, Ack)]).

%% A pre_request plugin listed in plugins_after_security keeps its callback
%% and its place among the listed ones, and runs once the security callback
%% has let the request through.
plugin_phase(pre_request, PluginName, AfterSecurity) ->
    case lists:member(PluginName, AfterSecurity) of
        true -> after_security;
        false -> pre_request
    end;
plugin_phase(Type, _PluginName, _AfterSecurity) ->
    Type.

plugins_after_security(RouteInfo, RouterFile) ->
    Modules = maps:get(plugins_after_security, RouteInfo,
                       application:get_env(nova, plugins_after_security, [])),
    case is_list(Modules) andalso lists:all(fun erlang:is_atom/1, Modules) of
        true ->
            Modules;
        false ->
            ?LOG_ERROR(#{reason => <<"plugins_after_security must be a list of plugin modules">>,
                         value => Modules, router_file => RouterFile}),
            throw({error, {invalid_plugins_after_security, Modules}})
    end.

%% A method that is not recognised stops the route table from compiling.
%% It used to become the wildcard, which opened the route to every method.
%% An atom is one of the methods Nova knows. A binary or a string is any
%% method, such as PURGE or PROPFIND, in the case it is sent in.
method_to_binary('_') ->
    '_';
method_to_binary(Method) when is_atom(Method) ->
    known_method(atom_to_binary(Method), Method);
method_to_binary(Method) when is_binary(Method) ->
    method_token(Method, Method);
method_to_binary(Method) when is_list(Method) ->
    try unicode:characters_to_binary(Method) of
        Name when is_binary(Name) -> method_token(Name, Method);
        _ -> throw({error, {invalid_method, Method}})
    catch
        error:badarg -> throw({error, {invalid_method, Method}})
    end;
method_to_binary(Method) ->
    throw({error, {invalid_method, Method}}).

known_method(Name, Method) ->
    case string:uppercase(Name) of
        Upper when Upper =:= <<"GET">>; Upper =:= <<"POST">>; Upper =:= <<"PUT">>;
                   Upper =:= <<"DELETE">>; Upper =:= <<"OPTIONS">>; Upper =:= <<"HEAD">>;
                   Upper =:= <<"CONNECT">>; Upper =:= <<"TRACE">>; Upper =:= <<"PATCH">> ->
            Upper;
        _ ->
            throw({error, {invalid_method, Method}})
    end.

%% RFC 9110 section 9.1: a method is a token, and it is case-sensitive.
method_token(Name, Method) ->
    case Name =/= <<>> andalso is_token(Name) of
        true -> Name;
        false -> throw({error, {invalid_method, Method}})
    end.

is_token(<<Char, Rest/binary>>) ->
    is_token_char(Char) andalso is_token(Rest);
is_token(<<>>) ->
    true.

is_token_char(Char) when Char >= $a, Char =< $z; Char >= $A, Char =< $Z; Char >= $0, Char =< $9 ->
    true;
is_token_char(Char) ->
    lists:member(Char, "!#$%&'*+-.^_`|~").

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
