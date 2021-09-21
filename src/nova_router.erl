%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(nova_router).
-behaviour(cowboy_middleware).

-export([
         compile/1,
         execute/2,
         route_reader/1,
         lookup_url/1,
         print_routes/0,

         read_routefile/1,

         render_status_page/2,
         render_status_page/3,
         render_status_page/5
        ]).

-include_lib("nova/include/nova.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").
-include("nova_router.hrl").

%% Route information
-type dispatch_rules() :: [{Host :: '_' | binary(), #node{}}].

-type bindings() :: #{binary() := binary()}.
-export_type([bindings/0]).

-spec compile(Apps :: [atom()]) -> dispatch_rules().
compile(Apps) ->
    Dispatch = compile(Apps, routing_tree:new(#{use_strict => true, convert_to_binary => true}), #{}),
    persistent_term:put(nova_dispatch, Dispatch),
    format_tree(Dispatch),
    Dispatch.

-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
                               when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req = #{host := Host, path := Path, method := Method}, Env = #{dispatch := Dispatch}) ->
    BinList = case binary:split(Path, <<"/">>, [global, trim_all]) of
                  [] ->
                      [<<>>];
                  Res ->
                      Res
              end,
    case routing_tree:lookup(Host, BinList, Method, Dispatch) of
        {error, not_found} -> render_status_page('_', 404, #{error => "Not found in path"}, Req, Env);
        {error, _Type, _Reason} -> render_status_page('_', 500, #{error => "Server error"}, Req, Env);
        {ok, Bindings, #nova_router_value{app = App, module = Module, function = Function,
                                          secure = Secure, extra_state = ExtraState}} ->
            {ok,
             Req,
             Env#{app => App,
                  module => Module,
                  function => Function,
                  secure => Secure,
                  controller_data => #{},
                  bindings => Bindings,
                  extra_state => ExtraState}
            };
        Error ->
            render_status_page(Host, 404, #{error => Error}, Req, Env)
    end.

route_reader(App) ->
    RoutePath = filename:join([code:priv_dir(App), erlang:atom_to_list(App) ++ ".routes.erl"]),
    file:consult(RoutePath).

lookup_url(Path) ->
    Dispatch = persistent_term:get(nova_dispatch),
    routing_tree:lookup('_', Path, '_', Dispatch).

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

-spec compile(Apps :: [atom()], Dispatch :: dispatch_rules(), Options :: map()) -> dispatch_rules().
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

    Value = #nova_router_value{secure = maps:get(secure, Options, maps:get(secure, RouteInfo, false)),
                               app = App,
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


-spec read_routefile(App :: atom()) -> {ok, Terms :: [term()]} | {error, Reason :: atom() | {non_neg_integer(), atom(), atom()}}.
read_routefile(App) ->
    Filepath = filename:join([code:priv_dir(App), erlang:atom_to_list(App) ++ ".routes.erl"]),
    file:consult(Filepath).

parse_url(_Host, [], _Prefix, _Value, Tree) -> {ok, Tree};
parse_url(Host, [{StatusCode, StaticFile}|Tl], Prefix, Value, Tree) when is_integer(StatusCode),
                                                                         is_list(StaticFile) ->
    %% We need to signal that we have a static file here somewhere
    Value0 = Value#nova_router_value{
               module = nova_static,
               function = execute,
               protocol = http},
    %% TODO! Fix status-code so it's being threated specially
    Tree0 = insert(Host, StatusCode, '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, Tree0);
parse_url(Host, [{StatusCode, {Module, Function}, Options}|Tl], Prefix, Value = #nova_router_value{app = App}, Tree) when is_integer(StatusCode) ->
    Value0 = Value#nova_router_value{
               module = Module,
               function = Function,
               protocol = http},
    Res = lists:foldl(fun(Method, Tree0) ->
                              insert(Host, StatusCode, Method, Value0, Tree0)
                      end, Tree, maps:get(methods, Options, ['_'])),
    parse_url(Host, Tl, Prefix, Value, Res);
parse_url(Host, [{RemotePath, LocalPath}|Tl], Prefix, Value = #nova_router_value{app = App}, Tree) when is_list(RemotePath),
                                                                                                        is_list(LocalPath) ->
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

    Value0 = Value#nova_router_value{
               app = cowboy,
               module = cowboy_static,
               function = init,
               extra_state = [Payload]},
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, Tree0);

parse_url(Host, [{RemotePath, LocalPath}|Tl], Prefix, Value = #nova_router_value{app = App}, Tree) when is_list(RemotePath),
                                                                                                        is_list(LocalPath) ->
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

    Value0 = Value#nova_router_value{
               app = cowboy,
               module = cowboy_static,
               function = init,
               extra_state = [Payload]},
    ?DEBUG("Adding route: ~s value: ~p to tree: ~p", [string:concat(Prefix, RemotePath), Value0, Tree]),
    Tree0 = insert(Host, string:concat(Prefix, RemotePath), '_', Value0, Tree),
    parse_url(Host, Tl, Prefix, Value, Tree0);

parse_url(Host, [{Path, {Mod, Func}, Options}|Tl], Prefix, Value, Tree) ->
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
                              Value#nova_router_value{
                                module = Mod,
                                function = Func,
                                protocol = http
                               };
                          ws ->
                              Value#nova_router_value{
                                app = cowboy,
                                module = nova__ws_handler,
                                function = init,
                                extra_state = [], %% Env, NovaState]
                                protocol = ws
                               }
                      end,
                  ?DEBUG("Adding route: ~s value: ~p to tree: ~p", [RealPath, Value0, Tree0]),
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

-spec render_status_page(Host :: binary(), StatusCode :: integer(), Data :: map(), Req :: cowboy_req:req(), Env :: map()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
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
            {ok, #nova_router_value{app = App,
                                    module = Module,
                                    function = Function,
                                    secure = Secure,
                                    extra_state = ExtraState},
             Bindings} ->
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

print_routes() ->
    Dispatch = persistent_term:get(nova_dispatch),
    format_tree(Dispatch).

format_tree([]) -> ok;
format_tree(#host_tree{hosts = Hosts}) ->
    format_tree(Hosts);
format_tree([{Host, #routing_tree{tree = Tree}}|Tl]) ->
    io:format("Host: ~p~n", [Host]),
    format_tree(Tree, 1) ++ format_tree(Tl).

format_tree([], _Depth) -> [];
format_tree([#node{segment = Segment, value = Value, children = Children}|Tl], Depth) ->
    Segment0 =
        case false of
            _ when is_list(Segment) orelse
                   is_binary(Segment) ->
                Segment;
            _ when is_integer(Segment) ->
                erlang:integer_to_list(Segment);
            E ->
                "[...]"
        end,
    Prefix = [ $  || _X <- lists:seq(0, Depth*4) ],
    lists:foreach(fun(#node_comp{comparator = Method, value = #nova_router_value{app = App, module = Mod, function = Func}}) ->
                          io:format("~ts~ts ~ts /~ts (~ts, ~ts:~ts/1)~n", [Prefix, <<226,148,148,226,148,128,32>>, Method, Segment0, App, Mod, Func])
                  end, Value),
    format_tree(Children, Depth+1),
    format_tree(Tl, Depth).

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

ensure_binary(Term) when is_list(Term) ->
    erlang:list_to_binary(Term);
ensure_binary(Term) ->
    Term.

-ifdef(TEST).
-compile(export_all). %% Export all functions for testing purpose
-include_lib("eunit/include/eunit.hrl").



-endif.
