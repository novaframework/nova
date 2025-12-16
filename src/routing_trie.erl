-module(routing_trie).

-export([
         new/0,
         new/1,

         %% Exports that does not use methods or host (Defaults to 'all' and '_')
         insert/2,
         insert/3,
         member/2,
         match/2,
         find/2,

         %% Method-aware APIs (host defaults to '_')
         insert/4,
         member/3,
         match/3,
         find/3,

         %% Host + Method-aware APIs
         insert/5,    %% Method, Host, Path, Trie, Opts
         member/4,    %% Method, Host, Path, Trie
         match/4,     %% Method, Host, Path, Trie
         find/4,      %% Method, Host, Path, Trie

         to_list/1
        ]).

%% Host-aware trie:
%%  - 'trie()' is the host root
%%  - each host key maps to a per-host trie_node() (routing tree)
-opaque trie() :: #{
                    options := map(),                     %% global/root options
                    hosts   := #{ host_key() => trie_node() }
                   }.

-opaque trie_node() :: #{
                         options          := map(),         %% options for this node
                         children         := #{ child_key() => trie_node() },
                         terminal_methods := methods_set()  %% set of methods for which this node is terminal
                        }.

-type child_key() :: binary() | {wild, binary()}.

-type host_key() :: '_' | binary().
-type host_in()  :: '_' | binary() | list() | atom().

-type method() :: get | post | put | delete | patch | options | all.
-type methods_set() :: #{ method() => true }.
-type method_in() :: method() | binary() | list() | atom().

-type conflict() :: #{reason := atom(),
                      at := [child_key()],
                      existing := child_key() | undefined,
                      incoming := child_key() | undefined,
                      conflicts_with := binary(),
                      incoming_path := binary(),
                      method := method(),
                      existing_methods := [method()]}.

-export_type([trie/0, trie_node/0, method/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty host-root trie.
%% @end
%%--------------------------------------------------------------------
-spec new() -> trie().
new() ->
    new(#{}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty host-root trie with options.
%% Options:
%%  - any key/value pairs to be stored in the root options map
%% @end
%%--------------------------------------------------------------------
-spec new(map()) -> trie().
new(Opts) when is_map(Opts) ->
    #{options => Opts, hosts => #{}}.

%% Internal: create a new routing node (per-host trie root or child)
-spec new_node() -> trie_node().
new_node() ->
    new_node(#{}).

-spec new_node(map()) -> trie_node().
new_node(Opts) when is_map(Opts) ->
    #{children => #{}, terminal_methods => #{}, options => Opts}.

%%--------------------------------------------------------------------
%% Host helpers
%%--------------------------------------------------------------------

-spec norm_host(host_in()) -> host_key().
norm_host('_') ->
    '_';
norm_host(Host) when is_binary(Host) ->
    Host;
norm_host(Host) when is_list(Host) ->
    list_to_binary(Host);
norm_host(Host) when is_atom(Host) ->
    list_to_binary(atom_to_list(Host)).

-spec get_host_trie(host_key(), trie()) -> trie_node() | undefined.
get_host_trie(Host, Trie) ->
    Hosts = maps:get(hosts, Trie, #{}),
    maps:get(Host, Hosts, undefined).


%%--------------------------------------------------------------------
%% @doc
%% Inserts a path into the trie. Same as calling
%% `insert(all, '_', Path, Trie, #{})`
%% @end
%%--------------------------------------------------------------------
-spec insert(iodata(), trie()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Path, Trie) ->
    insert(all, '_', Path, Trie, #{}).

%%--------------------------------------------------------------------
%% @doc
%% Inserts a path into the trie with options.
%% Same as calling `insert(all, '_', Path, Trie, Opts)`.
%% Options:
%%  - strict: boolean() (default: false)
%%    If true, performs a "safe" insert that checks for conflicts
%%    (wildcard name conflicts, duplicate patterns, etc).
%%  - overwrite: boolean() (default: false)
%%    If true, will overwrite existing method at the given path if exists.
%% @end
%%--------------------------------------------------------------------
-spec insert(iodata(), trie(), map()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Path, Trie, Opts) ->
    insert(all, '_', Path, Trie, Opts).

%%--------------------------------------------------------------------
%% @doc
%% Inserts a method+path into the trie with options, using catch-all host '_'.
%% If a node already exists for the given path and method does not exist
%% already, the node is added. Otherwise it will return an error with
%% information about the conflict if not `overwrite` option is set to true.
%% @end
%%--------------------------------------------------------------------
-spec insert(method_in(), iodata(), trie(), map()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Method, Path, Trie, Opts) ->
    insert(Method, '_', Path, Trie, Opts).

%%--------------------------------------------------------------------
%% @doc
%% Inserts a method+host+path into the trie with options.
%% Host:
%%  - Concrete host (binary/list/atom) => that host only
%%  - '_' (atom) => catch-all host, used as fallback
%% Options:
%%  - strict: boolean() (default: false)
%%  - overwrite: boolean() (default: false)
%% @end
%%--------------------------------------------------------------------
-spec insert(method_in(), host_in(), iodata(), trie(), map()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Method, HostIn, Path, Trie0 = #{options := RootOpts, hosts := Hosts}, Opts) ->
    %% Merge options for this
    RootOpts1 = maps:merge(RootOpts, Opts),
    Trie1 = Trie0#{options := RootOpts1},

    HostTrie =
        case maps:get(HostIn, Hosts, undefined) of
            undefined ->
                %% Just create a new host trie with opts
                new_node(Opts);
            HostTrie0 ->
                HostTrie0
        end,
    case insert_inner(Method, Path, HostTrie, RootOpts1) of
        {ok, HostTrie1} ->
            Hosts0 = Hosts#{HostIn => HostTrie1},
            {ok, Trie1#{hosts => Hosts0}};
        {error, conflict, Conf} ->
            {error, conflict, Conf};
        HostTrie1 ->
            Hosts0 = Hosts#{HostIn => HostTrie1},
            {ok, Trie1#{hosts => Hosts0}}
    end.

%% Internal: insert into a single host's routing trie
-spec insert_inner(method_in(), iodata(), trie_node(), map()) ->
          {ok, trie_node()} | {error, conflict, conflict()}.
insert_inner(Method, Path, TrieNode, Opts) ->
    Opts0 = maps:get(options, TrieNode, #{}),
    Opts1 = maps:merge(Opts0, Opts),
    Trie1 = TrieNode#{options := Opts1},

    M    = norm_method(Method),
    Segs = segs_for_insert(Path),

    case maps:get(strict, Opts1, false) of
        true ->
            %% Call safe insert which checks for conflicts
            safe_insert_segs(M, Segs, Trie1, [], Segs);
        _ ->
            insert_segs(M, Segs, Trie1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Checks if a path exists in the trie.
%% Same as calling `member(all, Path, Trie)` (host '_').
%% @end
%%--------------------------------------------------------------------
-spec member(iodata(), trie()) -> boolean().
member(Path, Trie) ->
    member(all, Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Checks if a method+path exists in the trie (host '_').
%% @end
%%--------------------------------------------------------------------
-spec member(method_in(), iodata(), trie()) -> boolean().
member(Method0, Path, Trie) ->
    member(Method0, '_', Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Checks if a host+method+path exists in the trie.
%% Host:
%%  - Concrete host => only that host
%%  - '_'           => catch-all host
%% @end
%%--------------------------------------------------------------------
-spec member(method_in(), host_in(), iodata(), trie()) -> boolean().
member(Method0, HostIn, Path, Trie) ->
    case find(Method0, HostIn, Path, Trie) of
        {ok, _Node} -> true;
        error       -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Matches a concrete path against the trie, returning bindings if
%% matched. Same as calling `match(all, Path, Trie)` (host '_').
%% @end
%%--------------------------------------------------------------------
-spec match(iodata(), trie()) ->
          {ok, #{binary() => binary()}} | error.
match(Path, Trie) ->
    match(all, Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Matches a concrete method+path against the trie (host '_'),
%% returning bindings if matched.
%% @end
%%--------------------------------------------------------------------
-spec match(method_in(), iodata(), trie()) ->
          {ok, #{binary() => binary()}} | error.
match(Method0, Path, Trie) ->
    match(Method0, '_', Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Matches a concrete host+method+path against the trie, returning bindings
%% if matched.
%%
%% Host:
%%  - Concrete host => tries that host first, falls back to '_' if not found
%%  - '_'           => only routes stored under '_' host
%% @end
%%--------------------------------------------------------------------
-spec match(method_in(), host_in(), iodata(), trie()) ->
          {ok, #{binary() => binary()}} | error.
match(Method0, HostIn, Path, Trie) ->
    Host = norm_host(HostIn),
    HostTrie =
        case get_host_trie(Host, Trie) of
            undefined when Host =/= '_' ->
                get_host_trie('_', Trie);
            T ->
                T
        end,
    case HostTrie of
        undefined ->
            error;
        L ->
            M    = norm_method(Method0),
            Segs = segs_for_match(Path),
            case do_match(Segs, L, #{}) of
                {ok, Node, Binds} ->
                    case method_member(M, Node) of
                        true -> {ok, Binds};
                        _    -> error
                    end;
                _ ->
                    error
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Finds the node for a given path, regardless of method (host '_').
%% @end
%%--------------------------------------------------------------------
-spec find(iodata(), trie()) -> {ok, trie_node()} | error.
find(Path, Trie) ->
    find(all, '_', Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Finds the node for a given method+path (host '_').
%% Returns {ok, Node} only if Method exists at that node.
%% @end
%%--------------------------------------------------------------------
-spec find(method_in(), iodata(), trie()) -> {ok, trie_node()} | error.
find(Method0, Path, Trie) ->
    find(Method0, '_', Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Finds the node for a given method+host+path.
%% Host resolution:
%%  - Exact host map
%%  - else, if Host =/= '_' and '_' exists, fall back to '_'
%%  - else error
%% @end
%%--------------------------------------------------------------------
-spec find(method_in(), host_in(), iodata(), trie()) -> {ok, trie_node()} | error.
find(Method0, HostIn, Path, Trie) ->
    Host = norm_host(HostIn),
    HostTrie =
        case get_host_trie(Host, Trie) of
            undefined when Host =/= '_' ->
                get_host_trie('_', Trie);
            T ->
                T
        end,
    case HostTrie of
        undefined ->
            error;
        L ->
            Segs = segs_for_insert(Path),
            case descend_pattern(Segs, L) of
                undefined ->
                    error;
                Node ->
                    M = norm_method(Method0),
                    case method_member(M, Node) of
                        true  -> {ok, Node};
                        false -> error
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all method+path combinations in the trie
%% (ignores host in the output; host dimension is flattened).
%% @end
%%--------------------------------------------------------------------
-spec to_list(trie()) -> [binary()].
to_list(Trie) ->
    Hosts = maps:get(hosts, Trie, #{}),
    maps:fold(
      fun(_Host, HostTrie, Acc) ->
              gather(HostTrie, [], Acc)
      end, [], Hosts).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
norm_method(M) when is_atom(M) ->
    norm_method(atom_to_list(M));
norm_method(M) when is_list(M) ->
    norm_method(list_to_binary(string:uppercase(M)));
norm_method(<<M/binary>>) ->
    case M of
        <<"GET">>     -> get;
        <<"POST">>    -> post;
        <<"PUT">>     -> put;
        <<"DELETE">>  -> delete;
        <<"PATCH">>   -> patch;
        <<"OPTIONS">> -> options;
        _             -> all
    end.

segs_for_insert(Path) when is_list(Path) ->
    segs_for_insert(list_to_binary(Path));
segs_for_insert(Path) when is_binary(Path) ->
    Parts = [S || S <- binary:split(Path, <<"/">>, [global, trim]), S =/= <<>>],
    [<<"/">> | [ to_key(S) || S <- Parts ]].

segs_for_match(Path) when is_list(Path) ->
    segs_for_match(list_to_binary(Path));
segs_for_match(Path) when is_binary(Path) ->
    Parts = [S || S <- binary:split(Path, <<"/">>, [global, trim]), S =/= <<>>],
    [<<"/">> | Parts].

to_key(<<":", Rest/binary>>) -> {wild, Rest};
to_key(Bin)                  -> Bin.

render_key({wild, Name}) -> <<":", Name/binary>>;
render_key(Bin)          -> Bin.

render_path(Segs) ->
    case Segs of
        []               -> <<"/">>;
        [<<"/">>]        -> <<"/">>;
        [<<"/">> | Rest] ->
            Seg = iolist_to_binary(lists:join(<<"/">>, [render_key(S) || S <- Rest])),
            <<"/", (Seg)/binary>>;
        _ ->
            iolist_to_binary(lists:join(<<"/">>, [render_key(S) || S <- Segs]))
    end.

render_method(M) ->
    case M of
        get     -> <<"GET">>;
        post    -> <<"POST">>;
        put     -> <<"PUT">>;
        delete  -> <<"DELETE">>;
        patch   -> <<"PATCH">>;
        options -> <<"OPTIONS">>;
        all     -> <<"ALL">>
    end.

terminal_add(M, Node0=#{terminal_methods := Ms0}) ->
    Node0#{terminal_methods := Ms0#{ M => true }}.

method_member(M, #{terminal_methods := Ms}) ->
    case M of
        all -> maps:is_key(all, Ms) orelse (maps:size(Ms) > 0);
        _   -> maps:is_key(M, Ms) orelse maps:is_key(all, Ms)
    end.

methods_list(#{terminal_methods := Ms}) ->
    [K || {K, true} <- maps:to_list(Ms)].

%% Insert without conflict checking (single-host trie)
insert_segs(M, [], N0) ->
    terminal_add(M, N0);
insert_segs(M, [K | Rest], N0) ->
    Cs0 = maps:get(children, N0),
    Child0 = maps:get(K, Cs0, new_node()),
    Child1 = insert_segs(M, Rest, Child0),
    N0#{children := maps:put(K, Child1, Cs0)}.

descend_pattern([], N) ->
    N;
descend_pattern([K | Rest], N) ->
    Cs = maps:get(children, N),
    case maps:find(K, Cs) of
        error       -> undefined;
        {ok, Child} -> descend_pattern(Rest, Child)
    end.

%% Safe insert with conflict checking (single-host trie)
safe_insert_segs(M, [], N0, Prefix, Full) ->
    Ms = methods_list(N0),
    case lists:member(M, Ms) of
        true ->
            {error, conflict, #{
                                reason => duplicate_pattern,
                                at => Prefix,
                                existing => undefined,
                                incoming => undefined,
                                conflicts_with => render_path(Prefix),
                                incoming_path  => render_path(Full),
                                method => M,
                                existing_methods => Ms
                               }};
        _ ->
            case lists:member(all, Ms) of
                true when M =/= all ->
                    {error, conflict, #{
                                        reason => duplicate_due_to_all,
                                        at => Prefix,
                                        existing => undefined,
                                        incoming => undefined,
                                        conflicts_with => render_path(Prefix),
                                        incoming_path  => render_path(Full),
                                        method => M,
                                        existing_methods => Ms
                                       }};
                _ when M =:= all, Ms =/= [] ->
                    {error, conflict, #{
                                        reason => duplicate_due_to_existing_methods,
                                        at => Prefix,
                                        existing => undefined,
                                        incoming => undefined,
                                        conflicts_with => render_path(Prefix),
                                        incoming_path  => render_path(Full),
                                        method => all,
                                        existing_methods => Ms
                                       }};
                _ ->
                    {ok, terminal_add(M, N0)}
            end
    end;

safe_insert_segs(M, [K = {wild, NewVar} | Rest], N0, Prefix, Full) ->
    Cs0 = maps:get(children, N0),
    ExistingWild = find_wild_child(Cs0),
    case ExistingWild of
        none ->
            Child0 = new_node(),
            case safe_insert_segs(M, Rest, Child0, Prefix ++ [K], Full) of
                {ok, Child1} ->
                    {ok, N0#{children := maps:put(K, Child1, Cs0)}};
                {error, conflict, Info} ->
                    {error, conflict, Info}
            end;
        {wild, ExistingVar, ChildN} ->
            if ExistingVar =:= NewVar ->
                    case safe_insert_segs(M, Rest, ChildN, Prefix ++ [K], Full) of
                        {ok, Child1} ->
                            {ok, N0#{children := maps:put({wild, ExistingVar}, Child1, Cs0)}};
                        {error, conflict, Info} ->
                            {error, conflict, Info}
                    end;
               true ->
                    {error, conflict, #{
                                        reason => wildcard_name_conflict,
                                        at => Prefix,
                                        existing => {wild, ExistingVar},
                                        incoming => {wild, NewVar},
                                        conflicts_with => render_path(Prefix ++ [{wild, ExistingVar}]),
                                        incoming_path  => render_path(Prefix ++ [K] ++ Rest),
                                        method => M,
                                        existing_methods => methods_list(ChildN)
                                       }}
            end
    end;

safe_insert_segs(M, [K | Rest], N0, Prefix, Full) ->
    Cs0 = maps:get(children, N0),
    Child0 = maps:get(K, Cs0, new_node()),
    case safe_insert_segs(M, Rest, Child0, Prefix ++ [K], Full) of
        {ok, Child1} ->
            {ok, N0#{children := maps:put(K, Child1, Cs0)}};
        {error, conflict, Info} ->
            {error, conflict, Info}
    end.

find_wild_child(Cs) ->
    maps:fold(
      fun
          ({wild, Var}, Child, none) -> {wild, Var, Child};
          (_K, _Child, Acc)          -> Acc
             end, none, Cs).

do_match([], N, Binds) ->
    {ok, N, Binds};
do_match([Seg | Rest], N, Binds0) ->
    Cs = maps:get(children, N),

    %% 1) Exact first
    Exact = case maps:find(Seg, Cs) of
                {ok, C} ->
                    case do_match(Rest, C, Binds0) of
                        error -> error;
                        Ok    -> Ok
                    end;
                error -> error
            end,
    case Exact of
        {ok, _, _} -> Exact;
        error ->
            %% 2) Wildcard fallback
            case find_wild_child(Cs) of
                none -> error;
                {wild, VarName, C0} ->
                    do_match(Rest, C0, Binds0#{ VarName => Seg })
            end
    end.

gather(N, AccSegs, AccOut) ->
    Ms = methods_list(N),
    AccOut1 =
        case Ms of
            [] -> AccOut;
            _  ->
                Path = render_path(AccSegs),
                MethodLines =
                    [<< (render_method(M))/binary, " ", Path/binary >> || M <- Ms],
                MethodLines ++ AccOut
        end,
    Cs = maps:get(children, N),
    maps:fold(
      fun(K, Child, Out) ->
              gather(Child, AccSegs ++ [K], Out)
      end, AccOut1, Cs).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Basic insert / match on default host '_'
%%--------------------------------------------------------------------

default_host_insert_and_match_test() ->
    T0 = routing_trie:new(),
    {ok, T1} = routing_trie:insert(get, <<"/users">>, T0, #{}),

    ?assert(routing_trie:member(get, <<"/users">>, T1)),
    ?assertMatch({ok, #{}},
                 routing_trie:match(get, <<"/users">>, T1)).

%%--------------------------------------------------------------------
%% Wildcard path ("/users/:id") on default host
%%--------------------------------------------------------------------

wildcard_path_match_test() ->
    T0 = routing_trie:new(),
    {ok, T1} = routing_trie:insert(get, <<"/users/:id">>, T0, #{strict => true}),

    ?assertMatch({ok, #{<<"id">> := <<"42">>}},
                 routing_trie:match(get, <<"/users/42">>, T1)),
    ?assertMatch({ok, #{<<"id">> := <<"abc">>}},
                 routing_trie:match(get, <<"/users/abc">>, T1)).

%%--------------------------------------------------------------------
%% Method filtering via find/3 (host = '_')
%%--------------------------------------------------------------------

find_with_method_test() ->
    T0 = routing_trie:new(),
    {ok, T1} = routing_trie:insert(post, <<"/users">>, T0, #{}),

    ?assertMatch({ok, _},
                 routing_trie:find(post, <<"/users">>, T1)),
    ?assertEqual(error,
                 routing_trie:find(get, <<"/users">>, T1)).

%%--------------------------------------------------------------------
%% Host-specific route only – no '_' fallback
%%--------------------------------------------------------------------

host_specific_only_test() ->
    T0   = routing_trie:new(),
    Host = <<"http://api.example.com">>,

    {ok, T1} = routing_trie:insert(get, Host, <<"/users">>, T0, #{}),

    ?assertMatch({ok, #{}},
                 routing_trie:match(get, Host, <<"/users">>, T1)),
    ?assertEqual(error,
                 routing_trie:match(get,
                                    <<"http://other.example.com">>,
                                    <<"/users">>, T1)).

%%--------------------------------------------------------------------
%% Host fallback to catch-all '_' when specific host is missing
%%--------------------------------------------------------------------

host_fallback_to_catchall_test() ->
    T0 = routing_trie:new(),
    %% insert only on '_' host
    {ok, T1} = routing_trie:insert(get, '_', <<"/users">>, T0, #{}),

    %% should match when querying with another host due to fallback
    ?assertMatch({ok, #{}},
                 routing_trie:match(get,
                                    <<"http://api.example.com">>,
                                    <<"/users">>, T1)).

%%--------------------------------------------------------------------
%% find/4: host + method aware
%%--------------------------------------------------------------------

host_and_method_find_test() ->
    T0   = routing_trie:new(),
    Host = <<"http://api.example.com">>,

    {ok, T1} = routing_trie:insert(post, Host, <<"/users">>, T0, #{}),

    ?assertMatch({ok, _},
                 routing_trie:find(post, Host, <<"/users">>, T1)),
    ?assertEqual(error,
                 routing_trie:find(get, Host, <<"/users">>, T1)),
    %% and also check that another host falls back to '_' only if '_' exists
    ?assertEqual(error,
                 routing_trie:find(post,
                                   <<"http://other.example.com">>,
                                   <<"/users">>, T1)).

%%--------------------------------------------------------------------
%% Strict conflict detection for duplicate pattern+method
%%--------------------------------------------------------------------

strict_conflict_duplicate_pattern_test() ->
    T0 = routing_trie:new(),
    {ok, T1} =
        routing_trie:insert(get, <<"/users/:id">>, T0, #{strict => true}),

    {error, conflict, Conf} =
        routing_trie:insert(get, <<"/users/:id">>, T1, #{strict => true}),

    ?assertEqual(duplicate_pattern, maps:get(reason, Conf)),
    ?assertEqual(get, maps:get(method, Conf)).

%%--------------------------------------------------------------------
%% to_list/1 sanity check
%%--------------------------------------------------------------------

to_list_simple_test() ->
    T0 = routing_trie:new(),
    {ok, T1} = routing_trie:insert(get, <<"/users/:id">>, T0, #{}),
    Lines = routing_trie:to_list(T1),

    %% We expect "GET /users/:id" in the list
    ?assert(lists:member(<<"GET /users/:id">>, Lines)).

-endif.
