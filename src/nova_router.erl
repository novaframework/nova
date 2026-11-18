%%%-------------------------------------------------------------------
%%% Author: Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_router).
-moduledoc "Router module for nova. This module is responsible for compiling routes, dispatching requests to the correct handler and managing the routing table. It also exposes an API for modulating the routing table at runtime.".
-behaviour(nova_pipeline).

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
         status_page_request/3,

         %% Expose the router-callback
         routes/1,

         %% Fetch information about the routing table
         plugins/0,
         forget_plugins/0,
         compiled_apps/0,
         compiled_apps/1,
         dispatch_keys/0,

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
         normalize_secure/2, plugins_before_security/3, resolve_secure/3, router_module/1]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_router.hrl").
-include("../include/nova.hrl").

-doc "The values of the bindings of a matched route, keyed by the name of the binding.".
-type bindings() :: #{binary() => binary()}.

%% A request the router has matched. Its bindings are keyed by binary, which
%% nova_req:req() has no room for, so it is typed as a map.
-doc "A request the router has matched.".
-type routed_req() :: map().

-doc """
What a lookup in a routing table gives: the bindings and the value stored
for the route, with the remaining path segments for a route that ends in a
wildcard. `{error, not_found}` when no route matches, and
`{error, comparator_not_found, AllowedMethods}` when the path matches for
other methods only.
""".
-type lookup_result() :: {ok, bindings(), Value :: term()} |
                         {ok, bindings(), Value :: term(), PathInfo :: [binary()]} |
                         {error, not_found} |
                         {error, comparator_not_found, AllowedMethods :: [nova_routing_trie:comparator()]}.

-export_type([bindings/0, lookup_result/0]).

-doc """
Called when the routes of the application are compiled: when the
application starts on a listener, when it is mounted by another application
and from `add_routes/1`. `Env` is what `nova:get_environment/0` returns.

Returns the route groups of the application, each a map as described in the
routing guide. A router without `routes/1` adds no routes of its own.
""".
-callback routes(Env :: atom()) -> Routes :: [map()].

-doc """
Called together with `routes/1`, with the same `Env`. Returns the controller
modules to ask for routes, each as `Module` or `{Module, Options}`. Nova
calls `c:nova_controller:routes/2` of every one that exports it and adds what
it returns after the routes of `routes/1`.
""".
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
-doc """
The key of the routing table of a listener. `nova_dispatch` is the table of
the listener of the bootstrap application.
""".
-type dispatch_key() :: term().
-export_type([dispatch_key/0]).

-type plugin() :: {Type :: atom(), Module :: module(), Options :: term()}.
-type callback() :: {Callback :: fun(), Options :: term()}.
-type phased() :: [{Phase :: atom(), [callback()]}].
-type order() :: #{before := all | [module()], secured := boolean()}.
-type order_warning() :: body_behind_security | form_behind_security | pre_request_behind_security |
                         pre_request_not_for_preflight.

-doc "The key of the routing table of every listener Nova has started, for the functions that take a `DispatchKey`.".
-spec dispatch_keys() -> [dispatch_key()].
dispatch_keys() ->
    nova_sup:dispatch_keys().

-doc """
The applications compiled into the routing table of the default listener,
each with the prefix it is mounted under.
""".
-spec compiled_apps() -> [{App :: atom(), Prefix :: list()}].
compiled_apps() ->
    compiled_apps(?NOVA_DISPATCH).

-doc "As `compiled_apps/0`, but for the routing table addressed by `DispatchKey`.".
-spec compiled_apps(DispatchKey :: dispatch_key()) -> [{App :: atom(), Prefix :: list()}].
compiled_apps(DispatchKey) ->
    StorageBackend = storage_backend(),
    StorageBackend:get(apps_key(DispatchKey), []).


%% TODO! We need to implement a way to get and remove plugins for a path
-doc false.
plugins() ->
    StorageBackend = storage_backend(),
    StorageBackend:get(?NOVA_PLUGINS, []).

-doc false.
-spec forget_plugins() -> ok.
forget_plugins() ->
    StorageBackend = storage_backend(),
    StorageBackend:put(?NOVA_PLUGINS, []),
    ok.

-doc "As `compile/2`, into the routing table of the default listener.".
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

-doc false.
-spec execute(Req, Env :: nova_server:env()) -> {ok, Req, Env0} | {stop, Req}
                                                          when Req::nova_req:req(),
                                                               Env0::nova_server:env().
execute(Req0 = #{path := Path}, Env) ->
    StorageBackend = storage_backend(),
    DispatchKey = dispatch_key(Env),
    %% nova_session only tells nova_stream_h about session changes made by the
    %% process serving the request.
    Req = Req0#{nova_dispatch_key => DispatchKey, nova_session_owner => self()},
    case is_origin_form(Path) andalso
        (nova_settings:value(non_canonical_paths) =:= resolve orelse nova_routing_trie:is_canonical(Path)) of
        true ->
            route(Req, Env, StorageBackend:get(DispatchKey));
        false ->
            ?LOG_DEBUG(#{msg => ~"Request path is not canonical", path => Path}),
            render_status_page('_', 400, #{error => "Path is not canonical"}, Req, Env)
    end.

%% HTTP/2 lets a client send a path that does not start with a slash. It is
%% not one to resolve: nothing that decides by how a path starts could tell
%% where it leads. `*` is the target of a server-wide OPTIONS.
-spec is_origin_form(Path :: binary()) -> boolean().
is_origin_form(~"*") -> true;
is_origin_form(<<"/", _/binary>>) -> true;
is_origin_form(_Path) -> false.

route(Req = #{host := Host, path := Path, method := Method}, Env, Dispatch) ->
    case nova_routing_trie:find(Host, Path, Method, Dispatch) of
        {error, not_found} ->
            logger:debug(<<"Path ~p not found for ~p in ~p">>, [Path, Method, Host]),
            render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {error, comparator_not_found, AllowedMethods} ->
            logger:debug(<<"Method not allowed: ~p for ~p. Allowed methods: ~p">>, [Method, Path, AllowedMethods]),
            Allowed = [AllowedMethod || AllowedMethod <- AllowedMethods, is_binary(AllowedMethod)],
            AllowHeader = iolist_to_binary(lists:join(<<", ">>, Allowed)),
            %% Set the 'allow'-header
            Req1 = nova_req:set_resp_header(<<"allow">>, AllowHeader, Req),
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
            ?LOG_ERROR(#{msg => ~"Unexpected return from nova_routing_trie:find/4",
                         return_object => Error}),
            render_status_page(Host, 404, #{error => Error}, Req, Env)
    end.

%%--------------------------------------------------------------------
-doc """
Look up a path in the routing table of the default listener, among the
routes declared without a host, with `'_'` as the method. Use `lookup_url/4`
for the table of another listener.
""".
-spec lookup_url(Path :: nova_routing_trie:path()) -> lookup_result().
lookup_url(Path) ->
    lookup_url('_', Path).

-doc """
As `lookup_url/1`, among the routes of `Host`. `'_'`, and a host that has no
routes of its own, give the routes declared without a host.
""".
-spec lookup_url(Host :: binary() | atom(), Path :: nova_routing_trie:path()) -> lookup_result().
lookup_url(Host, Path) ->
    lookup_url(Host, Path, '_').

-doc """
As `lookup_url/2`, for `Method`, a binary such as `~"GET"`. With `'_'` only
a route declared without `methods` is found. A route declared for some
methods gives `{error, comparator_not_found, Methods}`.
""".
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

A request whose path is written another way is answered `400` before a route is looked up, so before any plugin and before the security callback, unless the `nova` environment sets `non_canonical_paths` to `resolve`. What is refused is a path with an empty segment ahead of its last one (`//a`, `/a//b`), or with a `.` or `..` segment, the dots written out or percent-encoded (`/a/../b`, `/a/%2e%2e/b`). One trailing slash is not refused, so `path` and this function still differ by it: `/users/` gives `/users`. A path that does not start with a slash, which HTTP/2 lets a client send, is refused whatever `non_canonical_paths` is. The answer is the `400` status route of the application when it has one.
""".
-spec canonical_path(Req :: nova_req:req() | binary()) -> binary().
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
-doc "Add routes to the dispatch-table for the given app. The routes can be either a list of maps or a map. It use the same structure as the routes-callback in the router-module. Throws `{error, {misplaced_security, App, Key, Path}}` for routes with a security callback under a key that is not read: `secure` on a route group, or `security` or `secure` in the options of a route.".
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
    ?LOG_ERROR(#{msg => ~"Invalid routes structure", app => App, routes => Routes}),
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
%% The router module for an application. Either configured explicitly with
%% the `router_module' application environment key, or derived from the
%% application name using the convention for the language in use.
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
                    ?LOG_ERROR(#{msg => ~"Could not find the router module of the application",
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

    Prefix = concat_strings(maps:get(prefix, Options, ""),
                            maps:get(prefix, RouteInfo, "")),
    Named = #{app => App, prefix => Prefix, router_file => RouterFile},

    ok = warn_route_plugin_strategy(App, RouteInfo, RouterFile),
    Plugins = [with_checked_options(Plugin, Named)
               || Plugin <- resolve_plugins(maps:get(plugin_strategy, Options, local_or_global), RouteInfo,
                                            RouterFile)],

    Secure = resolve_secure(Options, RouteInfo, RouterFile),

    ok = removed_keys(RouteInfo, Named),
    Order = #{before => plugins_before_security(RouteInfo, Plugins, Named), secured => Secure =/= false},
    ok = warn_about_order(RouteInfo, Secure, Plugins, Order, Named),
    Value = #nova_handler_value{secure = Secure, app = App, plugins = normalize_plugins(Plugins, Order),
                                extra_state = maps:get(extra_state, RouteInfo, #{})},

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
                        ?LOG_ERROR(#{msg => ~"Could not find local path for the given resource",
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
            ?LOG_ERROR(#{msg => ~"Unknown protocol", protocol => OtherProtocol,
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
                  plugins = with_route(RealPath, with_origin_check(Value#nova_handler_value.plugins)),
                  secure = Secure},

    ?LOG_DEBUG(#{action => <<"Adding route">>, protocol => <<"ws">>, route => Path, app => App,
                 router_file => maps:get(router_file, T, undefined)}),
    CompiledPaths = insert(Host, RealPath, '_', Value0, Tree, insert_opts(T)),
    parse_url(Host, Tl, T, Value, CompiledPaths).


-doc "As `render_status_page/3` with no data.".
-spec render_status_page(StatusCode :: integer(), Req :: nova_req:req()) ->
                                {ok, Req0 :: routed_req(), Env :: map()}.
render_status_page(StatusCode, Req) ->
    render_status_page(StatusCode, #{}, Req).

-doc """
As `render_status_page/5`, for any host and the routing table that serves
`Req`.
""".
-spec render_status_page(StatusCode :: integer(), Data :: map(), Req :: nova_req:req()) ->
                                {ok, Req0 :: routed_req(), Env :: map()}.
render_status_page(StatusCode, Data, Req) ->
    render_status_page('_', StatusCode, Data, Req, #{nova_dispatch_key => dispatch_key(Req)}).

-doc """
Points a request at the status page for `StatusCode`. For a Cowboy
middleware: nothing is sent here, the page is rendered by the controller step
that the returned `{ok, Req, Env}` is handed to.

The page is the route registered for `StatusCode` on `Host`, or the page of
Nova when there is none. Its controller is called with the request, which
has `StatusCode` under `resp_status_code` and `#{status => StatusCode,
data => Data}` under `controller_data`.
""".
-spec render_status_page(Host :: binary() | atom(),
                         StatusCode :: integer(),
                         Data :: map(),
                         Req :: nova_req:req(),
                         Env :: map()) -> {ok, Req0 :: routed_req(), Env :: map()}.
render_status_page(Host, StatusCode, Data, Req, Env) ->
    StorageBackend = storage_backend(),
    Dispatch = StorageBackend:get(dispatch_key(Env)),
    %% A status page is rendered by a controller. Left in place, the handler
    %% of a websocket route would run instead and upgrade the connection.
    Env1 = maps:without([cowboy_handler, arguments], Env),
    ControllerData = #{status => StatusCode, data => Data},
    {Req0, Env0} =
        case nova_routing_trie:find(Host, StatusCode, '_', Dispatch) of
            {error, _} ->
                %% Render nova page if exists - We need to determine where to find this path?
                {Req, Env1#{app => nova,
                           callback => fun nova_error_controller:status_code/1,
                           secure => false,
                           controller_data => ControllerData}};
            {ok, Bindings, #nova_handler_value{app = App,
                                               callback = Callback,
                                               secure = Secure,
                                               extra_state = ExtraState}} ->
                {
                 Req#{extra_state => ExtraState, bindings => Bindings, resp_status_code => StatusCode},
                 Env1#{app => App,
                      callback => Callback,
                      secure => Secure,
                      controller_data => ControllerData,
                      bindings => Bindings}
                }
        end,
    {ok, status_page_request(StatusCode, Data, Req0), Env0#{status_page => StatusCode}}.

%% What the controller of a status page finds in its request, however the
%% page was reached. It is called with the request alone, so the status and
%% what the caller gave for the page go there. For nova_handler, which
%% renders the page of a controller that failed.
-doc false.
-spec status_page_request(StatusCode :: integer(), Data :: map(), Req :: routed_req()) -> routed_req().
status_page_request(StatusCode, Data, Req) ->
    Req#{resp_status_code => StatusCode, controller_data => #{status => StatusCode, data => Data}}.


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
%% when none of them stops the request. Only pre_security/4 runs: no
%% security callback is asked about a preflight, so pre_request/4 of a
%% plugin the route group lists to run ahead of its callback would read
%% the body for anybody.
with_preflight_plugins(Req = #{method := ~"OPTIONS"}, Host, Path, Dispatch) ->
    case nova_req:header(~"origin", Req) =/= undefined andalso
        nova_req:header(~"access-control-request-method", Req) of
        Requested when is_binary(Requested) ->
            case route_plugins(nova_routing_trie:find(Host, Path, Requested, Dispatch)) of
                undefined ->
                    Req;
                RoutePlugins ->
                    {_Route, Plugins} = take_route(RoutePlugins),
                    Ahead = [Entry || Entry = {Callback, _Options} <- proplists:get_value(pre_request, Plugins, []),
                                      erlang:fun_info(Callback, name) =:= {name, pre_security}],
                    Req#{plugins => lists:keystore(pre_request, 1, proplists:delete(after_security, Plugins),
                                                   {pre_request, Ahead})}
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
            ?LOG_ERROR(#{msg => ~"dispatch_backend must be a module", value => Other}),
            persistent_term
    end.

%%--------------------------------------------------------------------
-doc "The key of the routing table serving a request, for the functions that take a `DispatchKey`. Takes the request, which carries the key once it has passed the router, or the middleware environment where that is at hand. Anything else gives the key of the default listener.".
-spec dispatch_key(ReqOrEnv :: nova_req:req() | nova_server:env()) -> dispatch_key().
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
                ?LOG_ERROR(#{msg => ~"Unknown HTTP method in the methods of a route. Use get, post, put, delete, options, head, connect, trace, patch or '_', or a binary or string for any other method.",
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
            ?LOG_ERROR(#{msg => ~"Conflicting route", route => Path, combinator => Combinator,
                         conflict => Conflict}),
            throw({error, {route_conflict, Conflict}})
    catch
        throw:Exception ->
            ?LOG_ERROR(#{msg => ~"Error when inserting route", route => Path, combinator => Combinator}),
            throw(Exception);
        Type:Exception ->
            ?LOG_ERROR(#{msg => ~"Unexpected exit", type => Type, exception => Exception}),
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
    ?LOG_ERROR(#{msg => ~"A security value must be false, a fun/1 or {Mod, Fun}.",
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
%% Work out which plugins apply to a route entry.
%%
%% `local_or_global' is the default and is how Nova has always behaved: a
%% route entry that declares `plugins' uses exactly those, otherwise it uses
%% the globally configured ones. The merging strategies exist for the cases
%% where you want both, and dedupe on `{Type, Module}' keeping the first
%% occurrence, so ordering within a phase is preserved.
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
    ?LOG_ERROR(#{msg => ~"Unknown plugin_strategy, falling back to local_or_global",
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

%% trusted_origins is an option of these two plugins as well as a setting.
%% It is checked by the rule of the setting, when the routes are compiled
%% and not by the first request that goes by it, and kept as that keeps it.
%% Under the name the setting had before any release, it is refused as the
%% setting is. nova_csrf_plugin is refused on a node without sessions, where
%% every request to its routes would fail.
-spec with_checked_options(Plugin :: plugin(), Named :: map()) -> plugin().
with_checked_options({pre_request, nova_csrf_plugin, _Options} = Plugin, Named) ->
    case nova_session:enabled() of
        true ->
            with_checked_origins(Plugin, Named);
        false ->
            ?LOG_ERROR(Named#{msg => ~"nova_csrf_plugin keeps its token in the session, or signs it for the session cookie, and use_sessions is false",
                              setting => use_sessions, value => false, plugin => nova_csrf_plugin}),
            throw({error, {invalid_setting, use_sessions, {required_by, nova_csrf_plugin}}})
    end;
with_checked_options(Plugin, Named) ->
    with_checked_origins(Plugin, Named).

-spec with_checked_origins(Plugin :: plugin(), Named :: map()) -> plugin().
with_checked_origins({_Type, Plugin, #{allowed_origins := _}}, Named)
  when Plugin =:= nova_csrf_plugin; Plugin =:= nova_ws_origin_plugin ->
    ?LOG_ERROR(Named#{msg => nova_settings:removed_notice(allowed_origins), setting => allowed_origins,
                      replaced_by => trusted_origins, plugin => Plugin}),
    throw({error, {invalid_setting, allowed_origins, {replaced_by, trusted_origins}}});
with_checked_origins({Type, Plugin, Options = #{trusted_origins := Value}}, Named)
  when Plugin =:= nova_csrf_plugin; Plugin =:= nova_ws_origin_plugin ->
    case nova_settings:checked(trusted_origins, Value) of
        {ok, Origins} ->
            {Type, Plugin, Options#{trusted_origins := Origins}};
        {error, Reason} ->
            ?LOG_ERROR(Named#{msg => nova_settings:expected(trusted_origins), setting => trusted_origins,
                              value => Reason, plugin => Plugin}),
            throw({error, {invalid_setting, trusted_origins, Reason}})
    end;
with_checked_origins(Plugin, _Named) ->
    Plugin.

%% A browser sends the cookies of a site with a websocket upgrade from any
%% page, so the Origin is checked before anything else of a websocket route
%% runs: no plugin and no security callback acts on a request that is
%% refused for it. The check goes by the options the route group gave
%% nova_ws_origin_plugin, wherever in its plugins the group lists it.
-spec with_origin_check(phased()) -> phased().
with_origin_check(Phased) ->
    Check = fun nova_ws_origin_plugin:pre_security/4,
    Options = case [Options || {_Phase, Callbacks} <- Phased, {Callback, Options} <- Callbacks, Callback =:= Check] of
                  [OfTheGroup | _] -> OfTheGroup;
                  [] -> #{}
              end,
    Others = [{Phase, [Entry || Entry = {Callback, _Options} <- Callbacks, Callback =/= Check]}
              || {Phase, Callbacks} <- Phased],
    add_plugin(nova_ws_origin_plugin),
    Ahead = lists:append([Callbacks || {pre_request, Callbacks} <- Others]),
    lists:keystore(pre_request, 1, Others, {pre_request, [{Check, Options} | Ahead]}).

%% The plugins of a route by the phase they run in, each phase in the order
%% the plugins are listed. `pre_request` is the phase ahead of the security
%% callback and `after_security` the one behind it.
normalize_plugins(Plugins, Order) ->
    Phased = lists:append([phased(Plugin, Order) || Plugin <- Plugins]),
    [{Phase, [Callback || {In, Callback} <- Phased, In =:= Phase]}
     || Phase <- lists:usort([Phase || {Phase, _Callback} <- Phased])].

%% A plugin listed as pre_request takes part before the controller: its
%% pre_security/4 ahead of the security callback, and its pre_request/4 once
%% the request is let through, so that nothing is read from a client that is
%% refused. plugins_before_security names the modules whose pre_request/4
%% runs ahead of security as it did in earlier releases.
-spec phased(plugin(), order()) -> [{Phase :: atom(), callback()}].
phased({pre_request, PluginName, Options}, Order) ->
    add_plugin(PluginName),
    HasPreSecurity = exports(PluginName, pre_security),
    [{phase(true), {fun PluginName:pre_security/4, Options}} || HasPreSecurity] ++
        [{phase(is_ahead(PluginName, Order)), {fun PluginName:pre_request/4, Options}}
         || not HasPreSecurity orelse exports(PluginName, pre_request)];
phased({Type, PluginName, Options}, _Order) ->
    add_plugin(PluginName),
    [{Type, {fun PluginName:Type/4, Options}}].

-spec phase(AheadOfSecurity :: boolean()) -> pre_request | after_security.
phase(true) -> pre_request;
phase(false) -> after_security.

%% Whether pre_request/4 of a plugin runs ahead of the security callback.
-spec is_ahead(module(), order()) -> boolean().
is_ahead(PluginName, #{before := Before}) ->
    Before =:= all orelse lists:member(PluginName, Before).

-spec exports(module(), Function :: atom()) -> boolean().
exports(Module, Function) ->
    _ = code:ensure_loaded(Module),
    erlang:function_exported(Module, Function, 4).

%% Earlier releases ran pre_request/4 of every plugin ahead of the security
%% callback. What relied on that goes on without anything failing: a
%% callback that reads the body finds none and may let the request in, and
%% a plugin that turned requests away does not see the ones that are
%% refused. Each is said once for a route group, when its routes are
%% compiled, and is said until 1.0. A plugin from outside Nova is named
%% unless it has pre_security/4, which is how its author says what runs
%% first. Listing it under plugins_before_security ends what is said about
%% refused requests and not what is said about preflights, for which
%% pre_request/4 does not run wherever the plugin is listed.
-spec warn_about_order(RouteInfo :: map(), Secure :: term(), [plugin()], order(), Named :: map()) -> ok.
warn_about_order(RouteInfo, Secure, Plugins, Order = #{secured := Secured}, Named) ->
    Behind = [{Module, Options} || {pre_request, Module, Options} <- Plugins, Secured, not is_ahead(Module, Order)],
    ok = body_behind_security(said_about_the_body(RouteInfo, Secure),
                              [Plugin || Plugin = {Module, Options} <- Behind, reads_body(Module, Options)],
                              lists:keymember(nova_csrf_plugin, 2, Plugins), Named),
    Outside = [Module || {pre_request, Module, _Options} <- Plugins, not is_plugin_of_nova(Module),
                         not exports(Module, pre_security)],
    Refused = [Module || Module <- Outside, lists:keymember(Module, 1, Behind)],
    ok = say_once(warning, pre_request_behind_security, Refused, Named),
    case has_route_without_options(RouteInfo) of
        true -> say_once(warning, pre_request_not_for_preflight, Outside -- Refused, Named);
        false -> ok
    end.

%% What a route group says with security_reads_body about a body that is
%% decoded behind the security callback. `false` is that the callback does
%% not read it, which ends what is said about it. That is the word of the
%% author of the route group about the callback the group declares, and it
%% is taken for nothing said when another callback is in front of the
%% routes: the secure or the override_secure of the nova_apps entry an
%% application is mounted with, or the security of the route group that
%% lists it, is written by somebody else. `true` is that the callback reads
%% the body, which asks for more and is taken from anybody.
-spec said_about_the_body(RouteInfo :: map(), Secure :: term()) -> {said, term()} | not_said.
said_about_the_body(RouteInfo = #{security_reads_body := false}, Secure) ->
    case is_declared_by_the_group(Secure, RouteInfo) of
        true -> {said, false};
        false -> not_said
    end;
said_about_the_body(#{security_reads_body := Value}, _Secure) ->
    {said, Value};
said_about_the_body(_RouteInfo, _Secure) ->
    not_said.

%% Whether no callback is in front of the routes of a group but the one the
%% group has as its security.
-spec is_declared_by_the_group(Secure :: term(), RouteInfo :: map()) -> boolean().
is_declared_by_the_group(Callbacks, RouteInfo) when is_list(Callbacks) ->
    [] =:= [Callback || Callback <- Callbacks, not is_declared_by_the_group(Callback, RouteInfo)];
is_declared_by_the_group(false, _RouteInfo) ->
    true;
is_declared_by_the_group(Callback, RouteInfo) ->
    case maps:get(security, RouteInfo, false) of
        {Module, Function} when is_atom(Module), is_atom(Function) -> Callback =:= fun Module:Function/1;
        Security -> Callback =:= Security
    end.

%% `true` is that the callback reads the body, and the routes do not compile
%% until the body is decoded ahead of the callback. A route group that says
%% nothing is warned about. One that reads a form for nova_csrf_plugin and
%% nothing else, which is the setup the guides and the generators give, is
%% told with a notice of its own: nothing shows whether its callback reads
%% the params, and the callback finds none.
-spec body_behind_security(Said :: {said, term()} | not_said, Readers :: [{module(), Options :: term()}],
                           HasCsrfPlugin :: boolean(), Named :: map()) -> ok.
body_behind_security({said, false}, _Readers, _HasCsrfPlugin, _Named) ->
    ok;
body_behind_security({said, true}, [], _HasCsrfPlugin, _Named) ->
    ok;
body_behind_security({said, true}, _Readers, _HasCsrfPlugin, Named) ->
    invalid_security_reads_body({requires, plugins_before_security}, Named);
body_behind_security({said, Value}, _Readers, _HasCsrfPlugin, Named) ->
    invalid_security_reads_body(Value, Named);
body_behind_security(not_said, Readers, HasCsrfPlugin, Named) ->
    ForTheToken = [Module || {Module, Options} <- Readers, HasCsrfPlugin andalso reads_a_form_alone(Options)],
    ok = say_once(warning, body_behind_security, [Module || {Module, _Options} <- Readers] -- ForTheToken, Named),
    say_once(notice, form_behind_security, ForTheToken, Named).

-spec invalid_security_reads_body(term(), Named :: map()) -> no_return().
invalid_security_reads_body(Value, Named) ->
    ?LOG_ERROR(Named#{msg => ~"security_reads_body of a route group must be true or false, and true needs the plugin that decodes the body to be listed under plugins_before_security",
                      setting => security_reads_body, value => Value}),
    throw({error, {invalid_setting, security_reads_body, Value}}).

-spec reads_a_form_alone(Options :: term()) -> boolean().
reads_a_form_alone(Options) when is_map(Options) ->
    maps:get(decode_json_body, Options, false) =/= true;
reads_a_form_alone(_Options) ->
    false.

-spec say_once(Level :: warning | notice, order_warning(), [module()], Named :: map()) -> ok.
say_once(_Level, _About, [], _Named) ->
    ok;
say_once(Level, About, Listed, Named = #{app := App, prefix := Prefix}) ->
    Plugins = lists:usort(Listed),
    case nova_once:first({?MODULE, About, App, Prefix, Plugins}) of
        true -> ?LOG(Level, Named#{msg => order_warning(About), notice => About, plugins => Plugins});
        false -> ok
    end.

-spec order_warning(order_warning()) -> binary().
order_warning(body_behind_security) ->
    ~"The security callback of this route group runs before nova_request_plugin reads the body, so it finds no json or params in the request. Earlier releases read the body first. If the callback reads them, list nova_request_plugin under plugins_before_security of the route group. If it does not, set security_reads_body => false in the route group. That answers for the callback the route group declares as its security, and not for one the application is mounted behind: the secure or override_secure of a nova_apps entry, or the security of a route group that lists the application";
order_warning(form_behind_security) ->
    ~"The security callback of this route group runs before nova_request_plugin reads the form, which the route group reads for the token of nova_csrf_plugin, so the callback finds no params in the request. Nothing shows whether it reads them. If it does not, say so with security_reads_body => false in the route group, which ends this notice. If it does, list nova_request_plugin under plugins_before_security of the route group";
order_warning(pre_request_behind_security) ->
    ~"pre_request/4 of these plugins runs once the security callback of this route group has let a request through, where earlier releases ran it first. It does not run for a request that is refused, nor for a CORS preflight to a route that does not take OPTIONS. What has to run for those belongs in pre_security/4. To run pre_request/4 ahead of the security callback as before, list the plugin under plugins_before_security";
order_warning(pre_request_not_for_preflight) ->
    ~"pre_request/4 of these plugins does not run for a CORS preflight to a route of this group that does not take OPTIONS, also when the plugin is listed under plugins_before_security. A plugin that answers preflights has to do it in pre_security/4".

-spec reads_body(module(), Options :: term()) -> boolean().
reads_body(nova_request_plugin, Options) when is_map(Options) ->
    maps:get(decode_json_body, Options, false) =:= true orelse
        maps:get(read_urlencoded_body, Options, false) =:= true;
reads_body(_Module, _Options) ->
    false.

%% A preflight to such a route is answered by pre_security/4 of the plugins
%% of the route for the method it asks about, and by nothing else.
-spec has_route_without_options(RouteInfo :: map()) -> boolean().
has_route_without_options(RouteInfo) ->
    [] =/= [Path || {Path, _Target, #{methods := Methods}} <- maps:get(routes, RouteInfo, []),
                    not is_integer(Path), is_list(Methods), not takes_options(Methods)].

-spec takes_options(Methods :: [term()]) -> boolean().
takes_options(Methods) ->
    [] =/= [Method || Method <- Methods, Method =:= '_' orelse is_options(Method)].

-spec is_options(Method :: term()) -> boolean().
is_options(Method) when is_atom(Method) ->
    string:uppercase(atom_to_binary(Method)) =:= ~"OPTIONS";
is_options(Method) ->
    Method =:= ~"OPTIONS" orelse Method =:= "OPTIONS".

%% The routes of an application can be compiled by a tool that has not
%% loaded Nova, which is what tells the modules of an application.
-spec is_plugin_of_nova(module()) -> boolean().
is_plugin_of_nova(PluginName) ->
    _ = application:load(nova),
    application:get_application(PluginName) =:= {ok, nova}.

%% The list of a route group is checked here, when its routes are compiled,
%% by the rule the one of the nova environment was checked with at the boot.
%% It also has to name plugins the group lists as pre_request: any other
%% name changes nothing, which a misspelt one would do without a sound.
plugins_before_security(#{plugins_before_security := Value}, Plugins, Group) ->
    Listed = [Module || {pre_request, Module, _Options} <- Plugins],
    case nova_settings:checked(plugins_before_security, Value) of
        {ok, all} ->
            all;
        {ok, Modules} when is_list(Modules) ->
            case Modules -- Listed of
                [] -> Modules;
                _NotOfTheGroup -> invalid_plugins_before_security(Value, Group)
            end;
        {error, Reason} ->
            invalid_plugins_before_security(Reason, Group)
    end;
plugins_before_security(_RouteInfo, _Plugins, _Group) ->
    nova_settings:value(plugins_before_security).

-spec invalid_plugins_before_security(term(), map()) -> no_return().
invalid_plugins_before_security(Value, Group) ->
    ?LOG_ERROR(Group#{msg => ~"plugins_before_security of a route group must be all or a list of its pre_request plugins that have pre_request/4",
                      setting => plugins_before_security, value => Value}),
    throw({error, {invalid_setting, plugins_before_security, Value}}).

%% plugins_after_security was in no release. A route group that still has it
%% is refused with the name of what took its place, as the nova environment is.
-spec removed_keys(RouteInfo :: map(), Named :: map()) -> ok.
removed_keys(#{plugins_after_security := _}, Named) ->
    {plugins_after_security, Replacement} = lists:keyfind(plugins_after_security, 1, nova_settings:removed()),
    ?LOG_ERROR(Named#{msg => nova_settings:removed_notice(plugins_after_security),
                      setting => plugins_after_security, replaced_by => Replacement}),
    throw({error, {invalid_setting, plugins_after_security, {replaced_by, Replacement}}});
removed_keys(_RouteInfo, _Named) ->
    ok.

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
-doc false.
-spec routes(Env :: atom()) -> [map()].
routes(_) ->
 [#{
    routes => [
               {404, fun nova_error_controller:not_found/1, #{}},
               {500, fun nova_error_controller:server_error/1, #{}}
              ]
   }].
