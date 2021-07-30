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
         lookup_url/2,
         lookup_url/3,

         read_routefile/1,
         render_status_page/2,
         render_status_page/3,

         print/0,
         print/1
        ]).

-include_lib("nova/include/nova.hrl").
-include("nova_router.hrl").

-record(options, {
                  use_strict = true :: true | false,
                  bindings = #{} :: map()
                 }).



-record(node, {
               key = <<>> :: binary() | '__ROOT__',
               mmf = [] :: [#mmf{}], %% MMF = Method Module Function
               children = [] :: [#node{}],
               is_binding = false :: boolean()
              }).

%% Route information
-type status_route()      :: {StatusCode :: integer(), {Mod :: atom(), Fun :: atom()}} |
                             {StatusCode :: integer(), StaticFile :: string()}.
-type status_route_opt()  :: {StatusCode :: integer(), {Mod :: atom(), Fun :: atom()}, Options :: map()}.
-type regular_route()     :: {Path :: string(), {Mod :: atom(), Fun :: atom()}}.
-type regular_route_opt() :: {Path :: string(), {Mod :: atom(), Fun :: atom()}, Options :: map()}.
-type static_route()      :: {Path :: string(), DirOrFile :: string()}.
-type static_route_opt()  :: {Path :: string(), DirOrFile :: string(), Options :: map()}.

-type route() :: status_route() |
                 status_route_opt() |
                 regular_route() |
                 regular_route_opt() |
                 static_route() |
                 static_route_opt().


-type route_info() :: #{
                        prefix => string(),
                        security => false | {Mod :: atom(), Fun :: atom()},
                        extra_state => any(),
                        routes => [route()]
                       }.

-type dispatch_rules() :: [{Host :: '_' | binary(), #node{}}].

-type bindings() :: #{binary() := binary()}.
-export_type([bindings/0]).

-spec compile(Apps :: [atom()]) -> dispatch_rules().
compile(Apps) ->
    Dispatch = compile(Apps, [], #{}),
    persistent_term:put(nova_dispatch, Dispatch),
    ?DEBUG("Got dispatchtable:~n~p", [Dispatch]),
    Dispatch.

-spec compile(Apps :: [atom()], Dispatch :: dispatch_rules(), Options :: map()) -> dispatch_rules().
compile([], Dispatch, _Options) -> Dispatch;
compile([App|Tl], Dispatch, Options) ->
    {M, F} = application:get_env(nova, route_reader, {?MODULE, route_reader}),
    {ok, Routes} = M:F(App),

    Options1 = case maps:get(prefix, Options, undefined) of
                   undefined -> Options;
                   Prefix -> Options#{prefix => Prefix}
               end,
    Options2 = case maps:get(secure, Options1, undefined) of
                   undefined -> Options1;
                   Secure -> Options1#{secure => Secure}
               end,
    Options3 = case maps:get(host, Options2, undefined) of
                   undefined -> Options2;
                   Host -> Options2#{host => Host}
               end,

    Options4 = Options3#{app => App},

    {ok, Dispatch1, Options5} = compile_paths(Routes, Dispatch, Options4),
    compile(Tl, Dispatch1, Options5).

-spec lookup_url(Path :: integer() | binary()) -> {error, {Reason :: atom(), Type :: atom()}} |
                                                  {error, Reason :: atom()} |
                                                  {ok, MMF :: #mmf{}, State :: map()}.
lookup_url(Path) when is_binary(Path);
                      is_integer(Path) ->
    lookup_url(Path, '_', '_').

lookup_url(Path, Host) when is_binary(Path);
                            is_integer(Path) ->
    lookup_url(Path, Host, '_').
lookup_url(Path, Host, Method) when is_binary(Path);
                                is_integer(Path) ->
    HostTree = persistent_term:get(nova_dispatch),
    find_endpoint(Host, Path, Method, HostTree).

-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
                               when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req = #{host := Host, path := Path, method := Method}, Env = #{dispatch := Dispatch}) ->
    case find_endpoint(Host, Path, Method, Dispatch) of
        %% TODO! Fix so we can route on HTTP-codes
        {error, {not_found, path}} -> render_status_page(404, Req, Env);
        {error, {not_found, host}} -> render_status_page(400, Req, Env);
        {ok, #mmf{app = App, module = Module, function = Function,
                  secure = Secure, extra_state = ExtraState}, Bindings} ->
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
        _Error ->
            %% TODO! Fix rendering of an error-page
            render_status_page(404, Req, Env)
    end.

route_reader(App) ->
    RoutePath = filename:join([code:priv_dir(App), erlang:atom_to_list(App) ++ ".routes.erl"]),
    file:consult(RoutePath).


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

compile_paths([], Dispatch, Options) -> {ok, Dispatch, Options};
compile_paths([RouteInfo|Tl], Dispatch, Options) ->
    App = maps:get(app, Options),
    MMF = #mmf{secure = maps:get(secure, Options, maps:get(secure, RouteInfo, false)),
               app = App, extra_state = maps:get(extra_state, RouteInfo, #{})},
    Host = maps:get(host, Options, maps:get(host, RouteInfo, '_')),
    MainPrefix = maps:get(prefix, Options, ""),
    RoutePrefix = maps:get(prefix, RouteInfo, ""),
    Prefix = MainPrefix ++ RoutePrefix,
    SubApps = maps:get(apps, RouteInfo, []),
    %% We need to add this app info to nova-env
    NovaEnv = nova:get_env(apps, []),
    NovaEnv0 = [{App, #{prefix => MainPrefix ++ RoutePrefix}}|NovaEnv],
    nova:set_env(apps, NovaEnv0),
    Dispatch1 = lists:foldl(fun({Path, MF}, Tree) when is_list(Path),
                                                       is_tuple(MF) ->
                                    parse_url(Host, {Prefix ++ Path, MF, #{}}, MMF, Tree);
                               ({Path, MF, Opts}, Tree) when is_list(Path),
                                                             is_tuple(MF),
                                                             is_map(Opts) ->
                                    parse_url(Host, {Prefix ++ Path, MF, Opts}, MMF, Tree);
                               ({Path, DirOrFile, Opts}, Tree) when is_list(Path),
                                                                    is_list(DirOrFile),
                                                                    is_map(Opts) ->
                                    %% Static directory or file with opts
                                    parse_url(Host, {Prefix ++ Path, DirOrFile}, MMF, Tree);
                               ({Path, LocalPath}, Tree) when is_list(Path),
                                                              is_list(LocalPath) ->
                                    %% Static file or directory
                                    parse_url(Host, {Prefix ++ Path, LocalPath}, MMF, Tree);
                               ({StatusCode, StaticFile}, Tree) when is_integer(StatusCode),
                                                                     is_list(StaticFile) ->
                                    %% Status code
                                    parse_url(Host, {StatusCode, StaticFile}, MMF, Tree);
                               ({StatusCode, MF, Opts}, Tree) when is_integer(StatusCode),
                                                                   is_tuple(MF),
                                                                   is_map(Opts) ->
                                    parse_url(Host, {StatusCode, MF, Opts}, MMF, Tree);
                               (Path, Tree) ->
                                    ?ERROR("~p~n", [Path]),
                                    Tree
                            end, Dispatch, maps:get(routes, RouteInfo, [])),
    Dispatch2 = compile(SubApps, Dispatch1, Options#{prefix => Prefix}),
    compile_paths(Tl, Dispatch2, Options).


-spec read_routefile(App :: atom()) -> {ok, Terms :: [term()]} | {error, Reason :: atom() | {non_neg_integer(), atom(), atom()}}.
read_routefile(App) ->
    Filepath = filename:join([code:priv_dir(App), erlang:atom_to_list(App) ++ ".routes.erl"]),
    file:consult(Filepath).

parse_url(Host, {StatusCode, StaticFile}, MMF, Tree) when is_integer(StatusCode),
                                                          is_list(StaticFile) ->
    HostTree = proplists:get_value(Host, Tree, new()),
    %% We need to signal that we have a static file here somewhere
    MMF0 = MMF#mmf{method = '_',
                   module = nova_static,
                   function = execute,
                   protocol = http},
    Compiled = insert(StatusCode, <<>>, MMF0, HostTree, #options{use_strict = false}),
    [{Host, Compiled}|proplists:delete(Host, Tree)];
parse_url(Host, {RemotePath, LocalPath}, MMF = #mmf{app = App}, HostsTree) when is_list(RemotePath),
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

    HostTree0 = proplists:get_value(Host, HostsTree, new()),
    Method = '_',

    MMF0 = MMF#mmf{method = '_',
                   app = cowboy,
                   module = cowboy_static,
                   function = init,
                   extra_state = Payload},

    CompiledPaths = insert(erlang:list_to_binary(RemotePath), <<>>, MMF0, HostTree0, #options{use_strict = false}),
    [{Host, CompiledPaths}|proplists:delete(Host, HostsTree)];
parse_url(Host, {Path, {Mod, Func}, Options}, MMF, HostsTree) ->
    RealPath = case Path of
                   _ when is_list(Path) -> erlang:list_to_binary(Path);
                   _ when is_integer(Path) -> Path;
                   _ -> throw({unknown_path, Path})
              end,
    Methods = maps:get(methods, Options, ['_']),
    HostTree0 = proplists:get_value(Host, HostsTree, new()),

    CompiledPaths =
        lists:foldl(
          fun(Method, Tree) ->
                  MMF0 =
                      case maps:get(protocol, Options, http) of
                          http ->
                              BinMethod = method_to_binary(Method),
                              MMF#mmf{method = BinMethod,
                                      module = Mod,
                                      function = Func,
                                      protocol = http
                                     };
                          ws ->
                              BinMethod = method_to_binary(Method),
                              MMF#mmf{method = BinMethod,
                                      app = cowboy,
                                      module = nova__ws_handler,
                                      function = init,
                                      protocol = ws,
                                      extra_state = [] %% Env, NovaState]
                                     }
                      end,
                  insert(RealPath, <<>>, MMF0, Tree, #options{use_strict = false})
          end, HostTree0, Methods),
    [{Host, CompiledPaths}|proplists:delete(Host, HostsTree)].


find_endpoint(Host, Path, Method, HostTree) when is_binary(Path);
                                                 is_integer(Path) ->
    case proplists:lookup(Host, HostTree) of
        none ->
            case proplists:lookup('_', HostTree) of
                none ->
                    {error, {not_found, host}};
                {_, Tree} ->
                    lookup(Path, Method, Tree)
            end;
        {_, Tree} ->
            lookup(Path, Method, Tree)
    end.

lookup(Path, Method, Tree) when is_binary(Method);
                                Method =:= '_' ->
    lookup(Path, Method, <<>>, Tree, #{bindings => #{}});
lookup(Path, Method, Tree) ->
    lookup(Path, method_to_binary(Method), Tree).



%%%%%%%%%%%%%%%%%%%%%%%%
%% TREE FUNCTIONS     %%
%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
    #node{key = '__ROOT__'}.

lookup(StatusCode, Method, _Acc, #node{children = Children}, _State) when is_integer(StatusCode) ->
    case find_node(StatusCode, Method, Children, []) of
        {ok, path, {_Node, MMF}} ->
            {ok, MMF, #{}};
        {ok, binding, Bindings} ->
            %% TODO: Fix this
            {ok, undefined, #{}};
        {error, _Reason} = E -> E
    end;
lookup(<<>>, Method, Acc, #node{children = Children}, #{bindings := Bindings} = State) ->
    case find_node(Acc, Method, Children, []) of
        {ok, path, {_Node, MMF}} ->
            {ok, MMF, Bindings};
        {ok, binding, {#node{key = Key}, MMF}} ->
            {ok, MMF, State#{bindings => Bindings#{Key => Acc}}};
        {error, _Reason} = E -> E
    end;
lookup(<<$/, Rest/binary>>, Method, <<>>, Tree, Options) ->
    %% Ignore since double /
    lookup(Rest, Method, <<>>, Tree, Options);
lookup(<<$/, Rest/binary>>, Method, Acc, #node{children = Children}, #{bindings := Bindings} = State) ->
    case find_node(Acc, false, Children, []) of
        {error, _ClassOrTuple} = Err ->
            Err;
        {ok, path, {Node, _MMF}} ->
            lookup(Rest, Method, <<>>, Node, State);
        {ok, binding, {#node{key = Key}=Node, undefined}} ->
            lookup(Rest, Method, <<>>, Node, State#{bindings => Bindings#{Key => Acc}})
    end;
lookup(<<X, Rest/binary>>, Method, Acc, Tree, State) ->
    lookup(Rest, Method, <<Acc/binary, X>>, Tree, State).


insert(HTTPCode, _Acc, MMF, #node{children = Children} = T, _Options) when is_integer(HTTPCode) ->
    case lists:keyfind(HTTPCode, #node.key, Children) of
        false ->
            #node{key = HTTPCode,
                  mmf = [MMF]};
        _ ->
            ?WARNING("An HTTP code is already defined in the route table. Skipping"),
            T
    end;
insert(<<>>, Acc, MMF, #node{children = Children}, _Options) ->
    case lists:keyfind(Acc, #node.key, Children) of
        false ->
            #node{key = Acc,
                  mmf = [MMF]};
        Result ->
            %% We need to check all the methods of the results with all the methods given my this path
            case find_method(MMF#mmf.method, [Result]) of
                {error, not_found} ->
                    Result#node{mmf = [MMF|Result#node.mmf]};
                _Node ->
                    %% We have a conflict
                    ?ERROR("Duplicated paths detected using the same method. Check your route-file"),
                    throw({duplicated_paths})
            end
    end;
insert(<<$/, Rest/binary>>, <<>>, MMF, #node{key = '__ROOT__'} = N, Options) ->
    Child= insert(Rest, <<>>, MMF, N, Options),
    N#node{children = [Child|without_child(Child, N#node.children)]};
insert(<<$/, Rest/binary>>, <<>>, MMF, PrevNode, Options) ->
    %% Ignore since this was either the root or a double /
    insert(Rest, <<>>, MMF, PrevNode, Options);
insert(<<$/, Rest/binary>>, Acc, MMF, #node{children = Children}, Options) ->
    warn_if_bindings(Children),
    case lists:keyfind(Acc, #node.key, Children) of
        false ->
            #node{key = Acc,
                  children = [insert(Rest, <<>>, MMF, new(), Options)]};
        Subtree ->
            %% Already exist so just go down in tree
            Child = insert(Rest, <<>>, MMF, Subtree, Options),
            Subtree#node{children = [Child|without_child(Child, Subtree#node.children)]}
    end;
insert(<<$:, Rest/binary>>, _Acc, MMF, #node{children = Children}, Options) ->
    %% No worries here, just continue
    case parse_binding(Rest, <<>>) of
        {Binding, <<>>} ->
            case lists:search(fun(#node{key = Key, is_binding = true}) when Key =:= Binding -> true; (_) -> false end, Children) of
                false ->
                    #node{key = Binding,
                          is_binding = true,
                          mmf = [MMF]};
                {value, Element} ->
                    Element#node{is_binding = true,
                                 mmf = [MMF|Element#node.mmf]}
            end;
        {Binding, Rest0} ->
            case lists:search(fun(#node{key = Key, is_binding = true}) when Key =:= Binding -> true; (_) -> false end, Children) of
                false ->
                    #node{key = Binding,
                          is_binding = true,
                          children = [insert(Rest0, <<>>, MMF, new(), Options)]};
                {value, Element} ->
                    Child = insert(Rest0, <<>>, MMF, Element, Options),
                    Element#node{key = Binding, is_binding = true, children = [Child|without_child(Child, Element#node.children)]}
            end
    end;
insert(<<$[, $., $., $., $]>>, _Acc, MMF, PrevNode, Options) ->
    %% This is a catch-all-route - needs to be the last thing in a route
    %% TODO! Implement!
    #node{key = <<"*">>, is_binding = true, mmf = [MMF]};
insert(<<X, Rest/bits>>, Acc, MMF, PrevNode, Options) ->
    insert(Rest, <<Acc/binary, X>>, MMF, PrevNode, Options).



-spec render_status_page(StatusCode :: integer(), Req :: cowboy_req:req()) ->
                                {ok, Req0 :: cowboy_req:req(), Env :: map()}.
render_status_page(StatusCode, Req) ->
    Dispatch = persistent_term:get(nova_dispatch),
    render_status_page(StatusCode, Req, #{dispatch => Dispatch}).

render_status_page(StatusCode, Req, Env = #{dispatch := Dispatch}) ->
    Env0 =
        case find_endpoint(StatusCode, <<>>, '_', Dispatch) of
            {error, _} ->
                %% Render nova page if exists - We need to determine where to find this path?
                Env#{app => nova,
                     module => nova_controller,
                     function => status_code,
                     secure => false,
                     controller_data => #{status => StatusCode}};
            {ok, #mmf{app = App, module = Module, function = Function, secure = Secure, extra_state = ExtraState},
             Bindings} ->
                Env#{app => App,
                     module => Module,
                     function => Function,
                     secure => Secure,
                     controller_data => #{},
                     bindings => Bindings,
                     extra_state => ExtraState}

        end,
    {ok, Req, Env0}.


parse_binding(<<>>, Acc) ->
    {Acc, <<>>};
parse_binding(<<$/, Rest/bits>>, Acc) ->
    {Acc, <<Rest/binary>>};
parse_binding(<<C, Rest/bits>>, Acc) ->
    parse_binding(Rest, << Acc/binary, C >>).


warn_if_bindings(Nodes) ->
    case has_bindings(Nodes) of
        false -> ok;
        _ ->
            ?WARNING("There's a potential issue in your routes. You are trying to set a fixed route in the same space as a binding. If this is intentional you can skip this warning!"),
            ok
    end.

has_bindings([]) -> false;
has_bindings([#node{is_binding = true}|_Tl]) -> true;
has_bindings([_|Tl]) -> has_bindings(Tl).

find_node(_, _, [], []) -> {error, {not_found, path}};
find_node(_, _, [], Binding) -> {ok, binding, Binding};
find_node(Key, false, [#node{is_binding = true}=N|Children], _Binding) ->
    find_node(Key, false, Children, {N, undefined});
find_node(Key, Method, [#node{is_binding = true, mmf=MMFs}|Children], Binding) ->
    case find_method(Method, MMFs) of
        {error, not_found} -> find_node(Key, Method, Children, Binding);
        {Node, MMF} -> find_node(Key, Method, Children, {Node, MMF})
    end;
find_node(Key, false, [#node{key = Key}=N|_Children], _Binding) ->
    {ok, path, {N, undefined}};
find_node(Key, Method, [#node{key = Key, mmf = MMFs}|_Children], _Binding) ->
    case find_method(Method, MMFs) of
        {error, not_found} -> {error, {not_found, method}};
        {Node, MMF} -> {ok, path, {Node, MMF}}
    end;
find_node(Key, Method, [_Hd|Tl], Bindings) ->
    find_node(Key, Method, Tl, Bindings).

find_method(_Method, []) -> {error, not_found};
find_method('_', [Elem|_Tl]) ->
    {undefined, Elem};
find_method(Method, [#mmf{method = Method} = MMF|_Tl]) ->
    {undefined, MMF};
find_method(_, [#mmf{method = '_'} = MMF|_Tl]) ->
    {undefined, MMF};
find_method(Method, [_|Tl]) ->
    find_method(Method, Tl).


without_child(_Node, []) -> [];
without_child(#node{key = Key}, [#node{key = Key}|Children]) ->
    Children;
without_child(Node, [Hd|Tl]) ->
    [Hd|without_child(Node, Tl)].


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


print() ->
    [{_, Dispatch}|_] = persistent_term:get(nova_dispatch),
    print(Dispatch).

print(#node{key = '__ROOT__', children = Children}) ->
    print(Children, 0).

print([], _Level) -> ok;
print([#node{key = Key, mmf = MMF, children = Children}|Tl], Level) ->
    Indent = [ $  || _X <- lists:seq(0, Level) ],
    io:format("~s", [Indent]),
    io:format("/~s~n", [Key]),
    ExtraLength = length(erlang:binary_to_list(Key)),
    ExtraIndent = [ $  || _X <- lists:seq(0, ExtraLength) ],
    lists:foreach(fun(#mmf{method = Method, module = Module, function = Function}) ->
                          io:format("~s~s", [Indent,ExtraIndent]),
                          io:format("<~s>(~s:~s)~n", [Method, Module, Function])
                  end, MMF),
    print(Children, Level+ExtraLength),
    print(Tl, Level).

-ifdef(TEST).
-compile(export_all). %% Export all functions for testing purpose
-include_lib("eunit/include/eunit.hrl").

basic_insert_test() ->
    Routes = [#{
                routes => [{"/messages", {dummy_mod, dummy_func}, #{methods => [post]}}]
               }],

    ?assertMatch([{'_', #node{key = '__ROOT__', mmf = [],
                              children = [#node{key = <<"messages">>,
                                                mmf = [#mmf{method = <<"POST">>, module = dummy_mod,
                                                            function = dummy_func}]}]}}], compile(Routes)).

basic_root_insertion() ->
    Routes = [#{
                routes => [{"/", {dummy_mod, dummy_func}, #{methods => [post]}}]
               }],
    ?assertMatch([{'_', #node{key = '__ROOT__', mmf = [#mmf{method = <<"POST">>, module = dummy_mod,
                                                            function = dummy_func}]}}], compile(Routes)).

duplicated_paths_test() ->
    Routes = [#{
                routes => [
                           {"/user/test", {dummy_mod, dummy_func}, #{method => [get]}},
                           {"/user/test", {dummy_mod2, dummy_func2}, #{method => [get]}}
                          ]
               }],
    ?assertException(throw, {duplicated_paths}, compile(Routes)).

insert_complex_test() ->
    Routes = [#{
                routes => [
                           {"/messages", {dummy_mod, dummy_func}, #{methods => [get]}},
                           {"/messages/dummy", {dummy_mod, dummy_func}, #{methods => [post]}},
                           {"/messages", {dummy_mod, dummy_func}, #{methods => [post]}},
                           {"/messages/dummy/:id", {dummy_mod, dummy_func}, #{methods => [post]}}
                          ]
               }],
    Expected =
        [{'_',#node{key = '__ROOT__',mmf = [],
                    children = [#node{key = <<"messages">>,
                                      mmf = [#mmf{method = <<"POST">>,module = dummy_mod,
                                                  function = dummy_func,secure = false,extra_state = #{}},
                                             #mmf{method = <<"GET">>,module = dummy_mod,
                                                  function = dummy_func,secure = false,extra_state = #{}}],
                                      children = [#node{key = <<"dummy">>,
                                                        mmf = [#mmf{method = <<"POST">>,module = dummy_mod,
                                                                    function = dummy_func,secure = false,extra_state = #{}}],
                                                        children = [#node{key = <<"id">>,
                                                                          mmf = [#mmf{method = <<"POST">>,module = dummy_mod,
                                                                                      function = dummy_func,secure = false,extra_state = #{}}],
                                                                          children = [],is_binding = true}],
                                                        is_binding = false}],
                                      is_binding = false}],
                    is_binding = false}}],

    ?assertMatch(Expected, compile(Routes)).

basic_route_root_lookup_test() ->
    Routes = [{'_',{node,'__ROOT__',[],
                    [{node,<<>>,
                      [{mmf,<<"GET">>,test_main_controller,index,false,
                        #{}}],
                      [],false}],
                    false}}],
    ?assertMatch({node,<<>>,
                  [{mmf,<<"GET">>,test_main_controller,index,false,
                    #{}}],
                  [],false}, find_endpoint(<<"localhost">>, <<"/">>, <<"GET">>, Routes)).


-endif.
