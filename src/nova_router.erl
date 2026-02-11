%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%%
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
         lookup_url/1,
         lookup_url/2,
         lookup_url/3,
         render_status_page/2,
         render_status_page/3,
         render_status_page/5,

         %% Expose the router-callback
         routes/1,

         %% Modulates the routes-table
         add_routes/2,

         %% Fetch information about the routing table
         plugins/0,
         compiled_apps/0
        ]).

-include_lib("routing_tree/include/routing_tree.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/nova_router.hrl").
-include("../include/nova.hrl").

-type bindings() :: #{binary() := binary()}.
-export_type([bindings/0]).

%% This module is also exposing callbacks for routers
-callback routes(Env :: atom()) -> Routes :: [map()].
-callback controllers(Env :: atom()) -> Controllers :: [module() | {module(), map()}].

-optional_callbacks([routes/1, controllers/1]).


-define(NOVA_APPS, nova_apps).
-define(NOVA_PLUGINS, nova_plugins).

-spec compiled_apps() -> [{App :: atom(), Prefix :: list()}].
compiled_apps() ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    StorageBackend:get(?NOVA_APPS, []).

plugins() ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    StorageBackend:get(?NOVA_PLUGINS, []).

-spec compile(Apps :: [atom() | {atom(), map()}]) -> host_tree().
compile(Apps) ->
    UseStrict = application:get_env(nova, use_strict_routing, false),
    Dispatch = compile(Apps, routing_tree:new(#{use_strict => UseStrict, convert_to_binary => true}), #{}),
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    StorageBackend:put(nova_dispatch, Dispatch),
    Dispatch.

-spec execute(Req, Env :: cowboy_middleware:env()) -> {ok, Req, Env0} | {stop, Req}
                                                          when Req::cowboy_req:req(),
                                                               Env0::cowboy_middleware:env().
execute(Req = #{host := Host, path := Path, method := Method}, Env) ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    Dispatch = StorageBackend:get(nova_dispatch),
    case routing_tree:lookup(Host, Path, Method, Dispatch) of
        {error, not_found} ->
            logger:debug("Path ~p not found for ~p in ~p", [Path, Method, Host]),
            render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {error, comparator_not_found, AllowedMethods} ->
            logger:debug("Method not allowed: ~p for ~p. Allowed methods: ~p", [Method, Path, AllowedMethods]),
            %% Join the elements in AllowedMethods with a colon
            AllowHeader = iolist_to_binary(string:join([binary_to_list(M) || M <- AllowedMethods], ", ")),
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
            ?LOG_ERROR(#{reason => <<"Unexpected return from routing_tree:lookup/4">>,
                         return_object => Error}),
            render_status_page(Host, 404, #{error => Error}, Req, Env)
    end.

lookup_url(Path) ->
    lookup_url('_', Path).

lookup_url(Host, Path) ->
    lookup_url(Host, Path, '_').

lookup_url(Host, Path, Method) ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    Dispatch = StorageBackend:get(nova_dispatch),
    lookup_url(Host, Path, Method, Dispatch).

lookup_url(Host, Path, Method, Dispatch) ->
    routing_tree:lookup(Host, Path, Method, Dispatch).

%%--------------------------------------------------------------------
%% @doc
%% Add routes to the dispatch-table for the given app. The routes
%% can be either a list of maps or a map. It use the same structure as
%% the routes-callback in the router-module.
%% @end
%%--------------------------------------------------------------------
-spec add_routes(App :: atom(), Routes :: [map()] | map()) -> ok.
add_routes(_App, []) -> ok;
add_routes(App, [Routes|Tl]) when is_list(Routes) ->
    Options = #{},
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    Dispatch = StorageBackend:get(nova_dispatch),

    %% Take out the prefix for the app and store it in the persistent store
    CompiledApps = StorageBackend:get(?NOVA_APPS, []),
    CompiledApps0 =
        case lists:keyfind(App, 1, CompiledApps) of
            false ->
                [{App, maps:get(prefix, Options, "/")}|CompiledApps];
            _StoredApp ->
                CompiledApps
        end,

    Options1 = Options#{app => App},

    {ok, Dispatch1, _Options2} = compile_paths(Routes, Dispatch, Options1),

    StorageBackend:put(?NOVA_APPS, CompiledApps0),
    StorageBackend:put(nova_dispatch, Dispatch1),

    add_routes(App, Tl);
add_routes(App, Routes) ->
    ?LOG_ERROR(#{reason => <<"Invalid routes structure">>, app => App, routes => Routes}),
    throw({error, {invalid_routes, App, Routes}}).


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

-spec compile(Apps :: [atom() | {atom(), map()}], Dispatch :: host_tree(), Options :: map()) -> host_tree().
compile([], Dispatch, _Options) -> Dispatch;
compile([{App, Options}|Tl], Dispatch, GlobalOptions) ->
    compile([App|Tl], Dispatch, maps:merge(Options, GlobalOptions));
compile([App|Tl], Dispatch, Options) ->
    %% Fetch the router-module for this application
    Router =
        case nova:detect_language() of
            erlang ->
                %% Router will be app_router
                erlang:list_to_atom(io_lib:format("~s_router", [App]));
            elixir ->
                %% We build the router as App.Router
                erlang:list_to_atom(io_lib:format("~s.Router", [App]));
            lfe ->
                %% We will build the router as app_router here aswell, but might change in the future
                erlang:list_to_atom(io_lib:format("~s_router", [App]))
        end,
    Env = nova:get_environment(),
    Routes = get_routes(Router, Env),

    CompileParameters = Router:module_info(compile),

    RouterFile = proplists:get_value(source, CompileParameters),
    Options1 = Options#{app => App, router_file => RouterFile},

    {ok, Dispatch1, Options2} = compile_paths(Routes, Dispatch, Options1),

    %% Take out the prefix for the app and store it in the persistent store
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),

    CompiledApps = StorageBackend:get(?NOVA_APPS, []),

    CompiledApps0 = [{App, maps:get(prefix, Options, "/")}|CompiledApps],

    StorageBackend:put(?NOVA_APPS, CompiledApps0),

    compile(Tl, Dispatch1, Options2).

compile_paths([], Dispatch, Options) -> {ok, Dispatch, Options};
compile_paths([RouteInfo|Tl], Dispatch, Options) ->
    App = maps:get(app, Options),
    RouterFile = maps:get(router_file, Options),
    %% Fetch the global plugins
    GlobalPlugins = application:get_env(nova, plugins, []),
    Plugins = maps:get(plugins, RouteInfo, GlobalPlugins),

    Secure =
        case maps:get(secure, Options, maps:get(security, RouteInfo, false)) of
            false ->
                false;
            {SMod, SFun} ->
                ?LOG_DEPRECATED("v0.9.24", "The {Mod,Fun} format have been deprecated for "
                                "the 'secure'-section of a route table. Use the new format for routes.", RouterFile),
                fun SMod:SFun/1;
            SCallback ->
                SCallback
        end,

    Value = #nova_handler_value{secure = Secure, app = App, plugins = normalize_plugins(Plugins),
                                extra_state = maps:get(extra_state, RouteInfo, #{})},

    Prefix = concat_strings(maps:get(prefix, Options, ""), maps:get(prefix, RouteInfo, "")),
    Host = maps:get(host, RouteInfo, '_'),
    SubApps = maps:get(apps, RouteInfo, []),
    %% We need to add this app info to nova-env
    NovaEnv = nova:get_env(apps, []),
    NovaEnv0 = [{App, #{prefix => Prefix}} | NovaEnv],
    nova:set_env(apps, NovaEnv0),

    {ok, Dispatch1} = parse_url(Host, maps:get(routes, RouteInfo, []), #{prefix => Prefix,
                                                                         router_file => maps:get(router_file, Options)},
                                Value, Dispatch),

    Dispatch2 = compile(SubApps, Dispatch1, Options#{value => Value, prefix => Prefix}),

    compile_paths(Tl, Dispatch2, Options).

parse_url(_Host, [], _Prefix, _Value, Tree) -> {ok, Tree};
parse_url(Host, [{StatusCode, Callback, Options}|Tl], T, Value, Tree) when is_integer(StatusCode) andalso
                                                                           is_function(Callback) ->
    Value0 = Value#nova_handler_value{callback = Callback},
    Res = lists:foldl(fun(Method, Tree0) ->
                              insert(Host, StatusCode, Method, Value0, Tree0)
                      end, Tree, maps:get(methods, Options, ['_'])),
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
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree),
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

            CompiledPaths =
                lists:foldl(
                  fun(Method, Tree0) ->
                          BinMethod = method_to_binary(Method),
                          Value1 = Value0#nova_handler_value{
                                     callback = Callback
                                    },
                          ?LOG_DEBUG(#{action => <<"Adding route">>, route => RealPath, app => App, method => Method,
                                       router_file => maps:get(router_file, Options, undefined)}),
                          insert(Host, RealPath, BinMethod, Value1, Tree0)
                  end, Tree, Methods),
            parse_url(Host, Tl, T, Value, CompiledPaths);
        OtherProtocol ->
            ?LOG_ERROR(#{reason => <<"Unknown protocol">>, protocol => OtherProtocol,
                        router_file => maps:get(router_file, Options, undefined)}),
            parse_url(Host, Tl, T, Value, Tree)
    end;
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
    CompiledPaths = insert(Host, RealPath, '_', Value0, Tree),
    parse_url(Host, Tl, T, Value, CompiledPaths).


-spec render_status_page(StatusCode :: integer(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Req) ->
    render_status_page(StatusCode, #{}, Req).

-spec render_status_page(StatusCode :: integer(), Data :: map(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Data, Req) ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    Dispatch = StorageBackend:get(nova_dispatch),
    render_status_page('_', StatusCode, Data, Req, #{dispatch => Dispatch}).

-spec render_status_page(Host :: binary() | atom(),
                         StatusCode :: integer(),
                         Data :: map(),
                         Req :: cowboy_req:req(),
                         Env :: map()) -> {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(Host, StatusCode, Data, Req, Env) ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    Dispatch = StorageBackend:get(nova_dispatch),
    {Req0, Env0} =
        case routing_tree:lookup(Host, StatusCode, '_', Dispatch) of
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


insert(Host, Path, Combinator, Value, Tree) ->
    try routing_tree:insert(Host, Path, Combinator, Value, Tree) of
        Tree0 -> Tree0
    catch
        throw:Exception ->
            ?LOG_ERROR(#{reason => <<"Error when inserting route">>, route => Path, combinator => Combinator}),
            throw(Exception);
        Type:Exception ->
            ?LOG_ERROR(#{reason => <<"Unexpected exit">>, type => Type, exception => Exception}),
            throw(Exception)
    end.


add_plugin(Plugin) ->
    StorageBackend = application:get_env(nova, dispatch_backend, persistent_term),
    StoredPlugins = StorageBackend:get(?NOVA_PLUGINS, []),
    Plugins1 = lists:umerge([[Plugin], StoredPlugins]),
    case Plugins1 of
        StoredPlugins ->
            ok;
        _ ->
            StorageBackend:put(?NOVA_PLUGINS, Plugins1)
    end.

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
    concat_strings(binary_to_list(Path1), Path2);
concat_strings(Path1, Path2) when is_binary(Path2) ->
    concat_strings(Path1, binary_to_list(Path2));
concat_strings(_Path1, Path2) when is_integer(Path2) ->
    Path2;
concat_strings(Path1, Path2) when is_list(Path1), is_list(Path2) ->
    string:concat(Path1, Path2).

-spec routes(Env :: atom()) -> [map()].
routes(_) ->
 [#{
    routes => [
               {404, fun nova_error_controller:not_found/1, #{}},
               {500, fun nova_error_controller:server_error/1, #{}}
              ]
   }].

-ifdef(TEST).
-compile(export_all).
-endif.
