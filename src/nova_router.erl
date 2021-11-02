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
         route_reader/1,
         lookup_url/1,
         lookup_url/2,
         lookup_url/3,
         render_status_page/2,
         render_status_page/3,
         render_status_page/5
        ]).

-include_lib("nova/include/nova.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").
-include("nova_router.hrl").

-type bindings() :: #{binary() := binary()}.
-export_type([bindings/0]).

-spec compile(Apps :: [atom()]) -> host_tree().
compile(Apps) ->
    Dispatch = compile(Apps, routing_tree:new(#{use_strict => true, convert_to_binary => true}), #{}),
    ok = persistent_term:put(nova_dispatch, Dispatch),
    Dispatch.

-spec execute(Req, Env :: cowboy_middleware:env()) -> {ok, Req, Env0} | {stop, Req}
                                                          when Req::cowboy_req:req(),
                                                               Env0::cowboy_middleware:env().
execute(Req = #{host := Host, path := Path, method := Method}, Env = #{dispatch := Dispatch}) ->
    case routing_tree:lookup(Host, Path, Method, Dispatch) of
        {error, not_found} -> render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {ok, Bindings, #nova_handler_value{app = App, module = Module, function = Function,
                                           secure = Secure, plugins = Plugins, extra_state = ExtraState}} ->
            {ok,
             Req,
             Env#{app => App,
                  module => Module,
                  function => Function,
                  secure => Secure,
                  controller_data => #{},
                  bindings => Bindings,
                  plugins => Plugins,
                  extra_state => ExtraState
                 }
            };
        {ok, _Bindings, #cowboy_handler_value{app = App, handler = Handler, arguments = Args,
                                              plugins = Plugins, secure = Secure}} ->
            {ok,
             Req,
             Env#{app => App,
                  cowboy_handler => Handler,
                  arguments => Args,
                  plugins => Plugins,
                  secure => Secure
                 }
            };
        {ok, _Bindings, #cowboy_handler_value{app = App, handler = Handler, arguments = Args,
                                              plugins = Plugins, secure = Secure}, PathInfo} ->
            {ok,
             Req#{path_info => PathInfo},
             Env#{app => App,
                  cowboy_handler => Handler,
                  arguments => Args,
                  plugins => Plugins,
                  secure => Secure
                 }
            };
        Error ->
            ?ERROR("Got error: ~p", [Error]),
            render_status_page(Host, 404, #{error => Error}, Req, Env)
    end.

-spec route_reader(App :: atom()) -> {ok, Terms :: [term()]} |
                                       {error, Reason :: atom() |
                                       {non_neg_integer(), atom(), atom()}}.

route_reader(App) ->
    RoutePath = filename:join([code:priv_dir(App), erlang:atom_to_list(App) ++ ".routes.erl"]),
    file:consult(RoutePath).

lookup_url(Path) ->
    lookup_url('_', Path).

lookup_url(Host, Path) ->
    lookup_url(Host, Path, '_').

lookup_url(Host, Path, Method) ->
    Dispatch = persistent_term:get(nova_dispatch),
    routing_tree:lookup(Host, Path, Method, Dispatch).

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

-spec compile(Apps :: [atom()], Dispatch :: host_tree(), Options :: map()) -> host_tree().
compile([], Dispatch, _Options) -> Dispatch;
compile([App|Tl], Dispatch, Options) ->
    {M, F} = application:get_env(nova, route_reader, {?MODULE, route_reader}),
    {ok, Routes} = M:F(App),
    Options1 = Options#{app => App},

    {ok, Dispatch1, Options2} = compile_paths(Routes, Dispatch, Options1),
    compile(Tl, Dispatch1, Options2).


compile_paths([], Dispatch, Options) -> {ok, Dispatch, Options};
compile_paths([RouteInfo|Tl], Dispatch, Options) ->
    App = maps:get(app, Options),
    %% Fetch the global plugins
    GlobalPlugins = application:get_env(nova, plugins, []),
    Plugins = maps:get(plugins, RouteInfo, GlobalPlugins),

    Value = #nova_handler_value{secure = maps:get(secure, Options, maps:get(secure, RouteInfo, false)),
                                app = App, plugins = normalize_plugins(Plugins, []),
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
                        ?WARNING("Could not find local path \"~p\" given for path \"~p\"", [LocalPath, RemotePath]),
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
                        ?WARNING("Could not find local path \"~p\" given for path \"~p\"", [LocalPath, RemotePath]),
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

    ?DEBUG("Adding route: ~s for app: ~p", [string:concat(Prefix, RemotePath), App]),
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, Tree0);

parse_url(Host,
          [{Path, {Mod, Func}, Options}|Tl],
          Prefix,
          Value = #nova_handler_value{app = App, secure = Secure},
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
                               };
                          ws ->
                              #cowboy_handler_value{
                                 app = App,
                                 handler = cowboy_websocket,
                                 arguments = #{module => Mod},
                                 plugins = Value#nova_handler_value.plugins,
                                 secure = Secure}
                      end,
                  ?DEBUG("Adding route: ~s for app: ~p", [RealPath, App]),
                  insert(Host, RealPath, BinMethod, Value0, Tree0)
          end, Tree, Methods),
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
    Dispatch = persistent_term:get(nova_dispatch),
    render_status_page('_', StatusCode, Data, Req, #{dispatch => Dispatch}).

-spec render_status_page(Host :: binary() | atom(),
                         StatusCode :: integer(),
                         Data :: map(),
                         Req :: cowboy_req:req(),
                         Env :: map()) -> {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(Host, StatusCode, Data, Req, Env = #{dispatch := Dispatch}) ->
    Env0 =
        case routing_tree:lookup(Host, StatusCode, '_', Dispatch) of
            {error, _} ->
                %% Render nova page if exists - We need to determine where to find this path?
                Env#{app => nova,
                     module => nova_controller,
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
    {ok, Req, Env0}.


insert(Host, Path, Combinator, Value, Tree) ->
    try routing_tree:insert(Host, Path, Combinator, Value, Tree) of
        Tree0 -> Tree0
    catch
        throw:Exception ->
            ?ERROR("Error when inserting route ~s : ~s", [Combinator, Path]),
            ?DEBUG("Was called as: routing_tree:insert(~p, ~p, ~p, ~p, ~p)", [Host, Path, Combinator, Value, Tree]),
            throw(Exception);
        Type:Exception ->
            ?ERROR("Error type ~p with exception ~p", [Type, Exception]),
            throw(Exception)
    end.

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
method_to_binary(path) -> <<"PATCH">>;
method_to_binary(_) -> '_'.

-ifdef(TEST).
-compile(export_all). %% Export all functions for testing purpose
-include_lib("eunit/include/eunit.hrl").



-endif.
