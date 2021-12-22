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
         routes/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").
-include("nova_router.hrl").

-type bindings() :: #{binary() := binary()}.
-export_type([bindings/0]).

%% This module is also exposing callbacks for routers
-callback routes(Env :: atom()) -> {ok, Routes :: any()}.


-spec compile(Apps :: [atom()]) -> host_tree().
compile(Apps) ->
    UseStrict = application:get_env(nova, use_strict_routing, false),
    Dispatch = compile(Apps, routing_tree:new(#{use_strict => UseStrict, convert_to_binary => true}), #{}),
    persistent_put(nova_dispatch, Dispatch),
    Dispatch.

-spec execute(Req, Env :: cowboy_middleware:env()) -> {ok, Req, Env0} | {stop, Req}
                                                          when Req::cowboy_req:req(),
                                                               Env0::cowboy_middleware:env().
execute(Req = #{host := Host, path := Path, method := Method}, Env) ->
    Dispatch = persistent_get(nova_dispatch, Env),
    case routing_tree:lookup(Host, Path, Method, Dispatch) of
        {error, not_found} -> render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {ok, Bindings, #nova_handler_value{app = App, module = Module, function = Function,
                                           secure = Secure, plugins = Plugins, extra_state = ExtraState}} ->
            {ok,
             Req#{plugins => Plugins,
                  bindings => Bindings},
             Env#{app => App,
                  module => Module,
                  function => Function,
                  secure => Secure,
                  controller_data => #{},
                  extra_state => ExtraState
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
        {ok, Bindings, #cowboy_handler_value{app = App, handler = Handler, arguments = Args,
                                              plugins = Plugins, secure = Secure}, PathInfo} ->
            {ok,
             Req#{path_info => PathInfo,
                  plugins => Plugins,
                  bindings => Bindings},
             Env#{app => App,
                  cowboy_handler => Handler,
                  arguments => Args,
                  secure => Secure
                 }
            };
        Error ->
            logger:error(#{"reason" => "Unexpected return from routing_tree:lookup/4", "return_object" => Error}),
            render_status_page(Host, 404, #{error => Error}, Req, Env)
    end.

lookup_url(Path) ->
    lookup_url('_', Path).

lookup_url(Host, Path) ->
    lookup_url(Host, Path, '_').

lookup_url(Host, Path, Method) ->
    Dispatch = persistent_get(nova_dispatch),
    lookup_url(Host, Path, Method, Dispatch).

lookup_url(Host, Path, Method, Dispatch) ->
    routing_tree:lookup(Host, Path, Method, Dispatch).

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

-spec compile(Apps :: [atom()], Dispatch :: host_tree(), Options :: map()) -> host_tree().
compile([], Dispatch, _Options) -> Dispatch;
compile([App|Tl], Dispatch, Options) ->
    %% Fetch the router-module for this application
    Router = erlang:list_to_atom(erlang:atom_to_list(App) ++ "_router"),
    Env = nova:get_environment(),

    %% Ensure that the module is loaded
    Routes = erlang:apply(Router, routes, [Env]),
    Options1 = Options#{app => App},

    {ok, Dispatch1, Options2} = compile_paths(Routes, Dispatch, Options1),
    compile(Tl, Dispatch1, Options2).

compile_paths([], Dispatch, Options) -> {ok, Dispatch, Options};
compile_paths([RouteInfo|Tl], Dispatch, Options) ->
    App = maps:get(app, Options),
    %% Fetch the global plugins
    GlobalPlugins = application:get_env(nova, plugins, []),
    Plugins = maps:get(plugins, RouteInfo, GlobalPlugins),

    Value = #nova_handler_value{secure = maps:get(secure, Options, maps:get(security, RouteInfo, false)),
                                app = App, plugins = normalize_plugins(Plugins),
                                extra_state = maps:get(extra_state, RouteInfo, #{})},

    Prefix = maps:get(prefix, Options, "") ++ maps:get(prefix, RouteInfo, ""),
    Host = maps:get(host, RouteInfo, '_'),
    SubApps = maps:get(apps, RouteInfo, []),
    %% We need to add this app info to nova-env
    NovaEnv = nova:get_env(apps, []),
    NovaEnv0 = [{App, #{prefix => Prefix}} | NovaEnv],
    nova:set_env(apps, NovaEnv0),

    {ok, Dispatch1} = parse_url(Host, maps:get(routes, RouteInfo, []), Prefix, Value, Dispatch),

    Dispatch2 = compile(SubApps, Dispatch1, Options#{value => Value, prefix => Prefix}),

    compile_paths(Tl, Dispatch2, Options).

parse_url(_Host, [], _Prefix, _Value, Tree) -> {ok, Tree};
parse_url(Host, [{StatusCode, StaticFile}|Tl], Prefix, Value, Tree) when is_integer(StatusCode),
                                                                         is_list(StaticFile) ->
    %% We need to signal that we have a static file here somewhere
    Value0 = Value#nova_handler_value{
               module = nova_static,
               function = execute},

    %% TODO! Fix status-code so it's being threated specially
    Tree0 = insert(Host, StatusCode, '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value0, Tree0);
parse_url(Host, [{StatusCode, {Module, Function}, Options}|Tl], Prefix, Value, Tree) when is_integer(StatusCode) ->
    Value0 = Value#nova_handler_value{
               module = Module,
               function = Function},
    Res = lists:foldl(fun(Method, Tree0) ->
                              insert(Host, StatusCode, Method, Value0, Tree0)
                      end, Tree, maps:get(methods, Options, ['_'])),
    parse_url(Host, Tl, Prefix, Value, Res);
parse_url(Host,
          [{RemotePath, LocalPath}|Tl],
          Prefix, Value = #nova_handler_value{app = App, secure = Secure},
          Tree) when is_list(RemotePath), is_list(LocalPath) ->
    %% Static assets - check that the path exists
    PrivPath = filename:join(code:lib_dir(App, priv), LocalPath),

    Payload =
        case {filelib:is_dir(LocalPath), filelib:is_dir(PrivPath)} of
            {false, false} ->
                %% No directory - check if it's a file
                case {filelib:is_file(LocalPath), filelib:is_file(PrivPath)} of
                    {false, false} ->
                        %% No dir nor file
                        logger:warning(#{"reason" => "Could not find local path for the given resource",
                                         "local_path" => LocalPath, "remote_path" => RemotePath}),
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

    Value0 = #cowboy_handler_value{
                app = App,
                handler = cowboy_static,
                arguments = Payload,
                plugins = Value#nova_handler_value.plugins,
                secure = Secure
               },
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, Tree0);
parse_url(Host,
          [{RemotePath, LocalPath}|Tl],
          Prefix,
          Value = #nova_handler_value{app = App, secure = Secure}, Tree)
          when is_list(RemotePath), is_list(LocalPath) ->
    %% Static assets - check that the path exists
    PrivPath = filename:join(code:lib_dir(App, priv), LocalPath),

    Payload =
        case {filelib:is_dir(LocalPath), filelib:is_dir(PrivPath)} of
            {false, false} ->
                %% No directory - check if it's a file
                case {filelib:is_file(LocalPath), filelib:is_file(PrivPath)} of
                    {false, false} ->
                        %% No dir nor file
                        logger:warning(#{"reason" => "Could not find local path for the given resource",
                                         "local_path" => LocalPath, "remote_path" => RemotePath}),
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
    Value0 = #cowboy_handler_value{
                app = App,
                handler = cowboy_static,
                arguments = Payload,
                plugins = Value#nova_handler_value.plugins,
                secure = Secure
               },

    logger:debug(#{"action" => "Adding route", "route" => string:concat(Prefix, RemotePath), "app" => App}),
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, Tree0);

parse_url(Host,
          [{Path, {Mod, Func}, Options}|Tl],
          Prefix,
          Value = #nova_handler_value{app = App, secure = _Secure},
          Tree) ->
    RealPath = case Path of
                   _ when is_list(Path) -> string:concat(Prefix, Path);
                   _ when is_integer(Path) -> Path;
                   _ -> throw({unknown_path, Path})
               end,
    Methods = maps:get(methods, Options, ['_']),

    CompiledPaths =
        lists:foldl(
          fun(Method, Tree0) ->
                  BinMethod = method_to_binary(Method),
                  Value0 =
                      case maps:get(protocol, Options, http) of
                          http ->
                              Value#nova_handler_value{
                                module = Mod,
                                function = Func
                               }
                      end,
                  logger:debug(#{"action" => "Adding route", "route" => RealPath, "app" => App}),
                  insert(Host, RealPath, BinMethod, Value0, Tree0)
          end, Tree, Methods),
    parse_url(Host, Tl, Prefix, Value, CompiledPaths);
parse_url(Host,
          [{Path, Mod, #{protocol := ws}} | Tl],
          Prefix, #nova_handler_value{app = App, secure = Secure} = Value,
          Tree) ->
     Value0 =  #cowboy_handler_value{
                                    app = App,
                                    handler = nova_ws_handler,
                                    arguments = #{module => Mod},
                                    plugins = Value#nova_handler_value.plugins,
                                    secure = Secure},
    logger:debug(#{"action" => "Adding route", "protocol" => "ws", "route" => Path, "app" => App}),
    RealPath = string:concat(Prefix, Path),
    CompiledPaths = insert(Host, RealPath, '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, CompiledPaths);
parse_url(Host, [{Path, {Mod, Func}}|Tl], Prefix, Value, Tree) ->
    parse_url(Host, [{Path, {Mod, Func}, #{}}|Tl], Prefix, Value, Tree).

-spec render_status_page(StatusCode :: integer(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Req) ->
    render_status_page(StatusCode, #{}, Req).

-spec render_status_page(StatusCode :: integer(), Data :: map(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Data, Req) ->
    Dispatch = persistent_get(nova_dispatch),
    render_status_page('_', StatusCode, Data, Req, #{dispatch => Dispatch}).

-spec render_status_page(Host :: binary() | atom(),
                         StatusCode :: integer(),
                         Data :: map(),
                         Req :: cowboy_req:req(),
                         Env :: map()) -> {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(Host, StatusCode, Data, Req, Env) ->
    Dispatch = persistent_get(nova_dispatch, Env),
    Env0 =
        case routing_tree:lookup(Host, StatusCode, '_', Dispatch) of
            {error, _} ->
                %% Render nova page if exists - We need to determine where to find this path?
                Env#{app => nova,
                     module => nova_error_controller,
                     function => status_code,
                     secure => false,
                     controller_data => #{status => StatusCode, data => Data}};
            {ok, Bindings, #nova_handler_value{app = App,
                                               module = Module,
                                               function = Function,
                                               secure = Secure,
                                               extra_state = ExtraState}} ->
                Env#{app => App,
                     module => Module,
                     function => Function,
                     secure => Secure,
                     controller_data => #{status => StatusCode, data => Data},
                     bindings => Bindings,
                     extra_state => ExtraState}

        end,
    {ok, Req#{resp_status_code => StatusCode}, Env0}.


insert(Host, Path, Combinator, Value, Tree) ->
    try routing_tree:insert(Host, Path, Combinator, Value, Tree) of
        Tree0 -> Tree0
    catch
        throw:Exception ->
            logger:error(#{"reason" => "Error when inserting route", "route" => Path, "combinator" => Combinator}),
            throw(Exception);
        Type:Exception ->
            logger:error(#{"reason" => "Unexpected exit", "type" => Type, "exception" => Exception}),
            throw(Exception)
    end.


normalize_plugins(Plugins) ->
    lists:reverse(normalize_plugins(Plugins, [])).

normalize_plugins([], Ack) -> Ack;
normalize_plugins([{Type, PluginName, Options}|Tl], Ack) ->
    ExistingPlugins = proplists:get_value(Type, Ack, []),
    normalize_plugins(Tl, [{Type, [{PluginName, Options}|ExistingPlugins]}|proplists:delete(Type, Ack)]).



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


persistent_put(Key, Value) ->
    case application:get_env(nova, use_persistent_term, true) of
        true ->
            persistent_term:put(Key, Value);
        _ ->
            ok
    end.

persistent_get(Key) ->
    case application:get_env(nova, use_persistent_term, true) of
        true ->
            persistent_term:get(Key);
        _ ->
            logger:warning(#{"reason" => "Called persistent_get/1 without option use_persistent_term set to true"}),
            throw({unsupported_operation, persistent_get})
    end.

persistent_get(Key, Env) ->
    case application:get_env(nova, use_persistent_term, true) of
        true ->
            persistent_term:get(Key);
        _ ->
            maps:get(Key, Env)
    end.

routes(_) ->
 [#{
    routes => [
               {404, { nova_error_controller, not_found }, #{}},
               {500, { nova_error_controller, server_error }, #{}}
              ]
   }].

-ifdef(TEST).
-compile(export_all). %% Export all functions for testing purpose
-include_lib("eunit/include/eunit.hrl").



-endif.
