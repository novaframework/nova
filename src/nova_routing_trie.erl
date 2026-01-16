-module(nova_routing_trie).

-export([
         new/0,
         new/1,

         %% Transform / rebuild from routes
         foldl/2,

         %% Insert
         insert/4,
         insert/5,
         insert/6,

         %% Membership
         member/2,
         member/3,
         member/4,

         %% Unified lookup
         lookup/2,
         lookup/3,
         lookup/4,

         to_list/1,
         from_list/1
        ]).

%% Host-aware trie:
%%  - 'trie()' is the host root
%%  - each host key maps to a per-host trie_node() (routing tree)
-opaque trie() :: #{
                    options := map(),                     %% global/root options
                    hosts   := #{ host_key() => trie_node() }
                   }.

%% Per-host routing tree node
%%  - children: nested segments
%%  - terminal: map MethodBin => Payload
-opaque trie_node() :: #{
                         options  := map(),         %% options for this node
                         children := #{ child_key() => trie_node() },
                         plugins := #{
                                      pre_request  := [module()],
                                      post_request := [module()]
                                     },
                         terminal := #{ method() => payload() }
                        }.

-type child_key() :: binary() | {wild, binary()}.

-type host_key() :: '_' | binary().
-type host_in()  :: '_' | binary() | list() | atom().

%% Stored method representation: uppercase binary, e.g. <<"GET">>, <<"ALL">>
-type method() :: binary().

%% External method input: atom (get), binary (<<"GET">>), or string
-type method_in() :: method() | atom() | list().

-type payload() :: term().

%% A "route" is the canonical unit we can rebuild the trie from.
%%
%% Supported input shapes:
%%   {Path, Method, Payload}
%%   {Path, Method, Payload, Opts}
%%   {Host, Path, Method, Payload}
%%   {Host, Path, Method, Payload, Opts}
%%
%% Where:
%%   - Host is optional (defaults to '_')
%%   - Opts is optional (defaults to #{}) and is merged into trie options for that insertion
-type route() ::
        {iodata(), method_in(), payload()} |
        {iodata(), method_in(), payload(), map()} |
        {host_in(), iodata(), method_in(), payload()} |
        {host_in(), iodata(), method_in(), payload(), map()}.

-type conflict() :: #{
                      reason          := atom(),
                      at              := [child_key()],
                      existing        := child_key() | undefined,
                      incoming        := child_key() | undefined,
                      conflicts_with  := binary(),
                      incoming_path   := binary(),
                      method          := method(),
                      existing_methods:= [method()]
                     }.

-export_type([trie/0, trie_node/0, method/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Constructors
%%--------------------------------------------------------------------

-spec new() -> trie().
new() ->
    new(#{}).

-spec new(map()) -> trie().
new(Opts) when is_map(Opts) ->
    #{options => Opts, hosts => #{}}.

%% Internal node constructor (per-host trees)
-spec new_node() -> trie_node().
new_node() ->
    new_node(#{}).

-spec new_node(map()) -> trie_node().
new_node(Opts) when is_map(Opts) ->
    #{children => #{}, terminal => #{}, options => Opts, plugins => #{pre_request => [],
                                                                      post_request => []}}.

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

%%--------------------------------------------------------------------
%% Insert
%%
%% Canonical full form:
%%   insert(Host, Path, Method, Payload, Trie, Opts)
%%
%% Convenience:
%%   insert(Path, Method, Payload, Trie).        % Host='_', Opts=#{}
%%   insert(Host, Path, Method, Payload, Trie).  % Opts=#{}
%%--------------------------------------------------------------------

-spec insert(iodata(), method_in(), payload(), trie()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Path, Method, Payload, Trie) ->
    insert('_', Path, Method, Payload, Trie, #{}).

-spec insert(host_in(), iodata(), method_in(), payload(), trie()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(HostIn, Path, Method, Payload, Trie) ->
    insert(HostIn, Path, Method, Payload, Trie, #{}).

-spec insert(host_in(), iodata(), method_in(), payload(), trie(), map()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(HostIn, Path, Method, Payload, Trie0, Opts) ->
    Host = norm_host(HostIn),
    RootOpts0 = maps:get(options, Trie0, #{}),
    RootOpts1 = maps:merge(RootOpts0, Opts),
    Trie1 = Trie0#{options => RootOpts1},
    Hosts = maps:get(hosts, Trie1, #{}),
    {Trie2, HostTrie0} =
        case maps:get(Host, Hosts, undefined) of
            undefined ->
                HostTrie = new_node(RootOpts1),
                Hosts1 = Hosts#{Host => HostTrie},
                {Trie1#{hosts := Hosts1}, HostTrie};
            HostTrie ->
                {Trie1, HostTrie}
        end,
    case insert_inner(Method, Path, Payload, HostTrie0, RootOpts1) of
        {ok, HostTrie1} ->
            Hosts2 = maps:get(hosts, Trie2, #{}),
            Hosts3 = Hosts2#{Host => HostTrie1},
            {ok, Trie2#{hosts := Hosts3}};
        {error, conflict, Conf} ->
            {error, conflict, Conf}
    end.

%% Internal: insert into a single host's routing trie
-spec insert_inner(method_in(), iodata(), payload(), trie_node(), map()) ->
          {ok, trie_node()} | {error, conflict, conflict()}.
insert_inner(Method, Path, Payload, TrieNode, Opts) ->
    Opts0 = maps:get(options, TrieNode, #{}),
    Opts1 = maps:merge(Opts0, Opts),
    Trie1 = TrieNode#{options := Opts1},

    M = norm_method(Method),
    Segs = segs_for_insert(Path),
    Strict = maps:get(strict, Opts1, false),

    case Strict of
        true ->
            %% Strict: conflicts become errors
            safe_insert_segs(M, Payload, Segs, Trie1, [], Segs);
        false ->
            %% Non-strict: run safe_insert_segs only for detection,
            %% then always perform regular insert_segs. Conflicts -> warnings.
            case safe_insert_segs(M, Payload, Segs, Trie1, [], Segs) of
                {error, conflict, Conf} ->
                    warn_conflict(Conf),
                    {ok, insert_segs(M, Payload, Segs, Trie1)};
                {ok, _TmpNode} ->
                    {ok, insert_segs(M, Payload, Segs, Trie1)}
            end
    end.

%%--------------------------------------------------------------------
%% Warnings (non-strict conflicts)
%%--------------------------------------------------------------------
warn_conflict(Conf) ->
    Reason        = maps:get(reason, Conf),
    Method        = maps:get(method, Conf),
    IncomingPath  = maps:get(incoming_path, Conf),
    ConflictsWith = maps:get(conflicts_with, Conf),
    io:format(
      "routing_trie warning (~p): ~s ~s conflicts with ~s~n",
      [Reason, render_method(Method), IncomingPath, ConflictsWith]
     ).

%%--------------------------------------------------------------------
%% Membership (uses lookup)
%%--------------------------------------------------------------------

-spec member(iodata(), trie()) -> boolean().
member(Path, Trie) ->
    member(<<"ALL">>, Path, Trie).

-spec member(method_in(), iodata(), trie()) -> boolean().
member(Method0, Path, Trie) ->
    member(Method0, '_', Path, Trie).

-spec member(method_in(), host_in(), iodata(), trie()) -> boolean().
member(Method0, HostIn, Path, Trie) ->
    case lookup(Method0, HostIn, Path, Trie) of
        {ok, _Node, _Payload, _Binds} -> true;
        error                         -> false
    end.

%%--------------------------------------------------------------------
%% Unified lookup
%%
%%  - Performs host selection (with '_' fallback)
%%  - Performs path + wildcard matching
%%  - Chooses payload according to Method (method-specific or <<"ALL">>)
%%  - Returns:
%%      error
%%      | {ok, Node, Payload, Bindings}
%%--------------------------------------------------------------------

-spec lookup(iodata(), trie()) ->
          {ok, trie_node(), term(), #{binary() => binary()}} | error.
lookup(Path, Trie) ->
    lookup(<<"ALL">>, '_', Path, Trie).

-spec lookup(method_in(), iodata(), trie()) ->
          {ok, trie_node(), term(), #{binary() => binary()}} | error.
lookup(Method0, Path, Trie) ->
    lookup(Method0, '_', Path, Trie).

-spec lookup(method_in(), host_in(), iodata(), trie()) ->
          {ok, trie_node(), term(), #{binary() => binary()}} | error.
lookup(Method0, HostIn, Path, Trie = #{hosts := Hosts}) ->
    Host = norm_host(HostIn),
    case maps:get(Host, Hosts, undefined) of
        undefined when Host =/= '_' ->
            lookup(Method0, '_', Path, Trie);
        undefined ->
            error;
        HostTrie ->
            M    = norm_method(Method0),
            Segs = segs_for_match(Path),
            case do_match(Segs, HostTrie, #{}) of
                {ok, Node, Binds} ->
                    case method_payload(M, Node) of
                        {ok, Payload} -> {ok, Node, Payload, Binds};
                        error         -> error
                    end;
                _ ->
                    error
            end
    end.


%%--------------------------------------------------------------------
%% to_list: flattened host view (ignores payloads)
%%--------------------------------------------------------------------

-spec to_list(trie()) -> [binary()].
to_list(Trie) ->
    Hosts = maps:get(hosts, Trie, #{}),
    maps:fold(
      fun(_Host, HostTrie, Acc) ->
              gather(HostTrie, [], Acc)
      end, [], Hosts).

%%--------------------------------------------------------------------
%% from_list: build a routing trie from a list of routes
%%--------------------------------------------------------------------

-spec from_list([route()]) -> {ok, trie()} | {error, conflict, conflict()}.
from_list(Routes) when is_list(Routes) ->
    from_list(Routes, #{}).

%%--------------------------------------------------------------------
%% foldl: rebuild the trie by transforming its extracted routes
%%
%% Function :: fun(([route()]) -> [route()])
%%
%% 1) Extracts routes from the trie (including payloads)
%% 2) Calls Function(Routes)
%% 3) Rebuilds a new trie from the returned routes
%%--------------------------------------------------------------------

-spec foldl(trie(), fun(([route()]) -> [route()])) ->
          {ok, trie()} | {error, conflict, conflict()}.
foldl(Trie0, Fun) when is_map(Trie0), is_function(Fun, 1) ->
    Routes0 = routes(Trie0),
    Routes1 = Fun(Routes0),
    case is_list(Routes1) of
        true ->
            from_list(Routes1, maps:get(options, Trie0, #{}));
        false ->
            erlang:error({badreturn, {foldl, Fun, Routes1}})
    end.

%%====================================================================
%% Internal functions (methods, segments, conflicts, matching)
%%====================================================================

%%--------------------------------------------------------------------
%% from_list helpers
%%--------------------------------------------------------------------

-spec from_list([route()], map()) -> {ok, trie()} | {error, conflict, conflict()}.
from_list(Routes, RootOpts) when is_list(Routes), is_map(RootOpts) ->
    lists:foldl(
      fun
          (Route, {ok, TrieAcc}) ->
              insert_route(Route, TrieAcc);
          (_Route, Err={error, conflict, _Conf}) ->
              Err
      end,
      {ok, new(RootOpts)},
      Routes
     ).

-spec insert_route(route(), trie()) -> {ok, trie()} | {error, conflict, conflict()}.
insert_route({Path, Method, Payload}, Trie) ->
    insert(Path, Method, Payload, Trie);
insert_route({Path, Method, Payload, Opts}, Trie) when is_map(Opts) ->
    insert('_', Path, Method, Payload, Trie, Opts);
insert_route({Host, Path, Method, Payload}, Trie) ->
    insert(Host, Path, Method, Payload, Trie);
insert_route({Host, Path, Method, Payload, Opts}, Trie) when is_map(Opts) ->
    insert(Host, Path, Method, Payload, Trie, Opts);
insert_route(Other, _Trie) ->
    erlang:error({bad_route, Other}).

%%--------------------------------------------------------------------
%% Route extraction (used by foldl/2)
%%--------------------------------------------------------------------

-spec routes(trie()) -> [route()].
routes(Trie) ->
    Hosts = maps:get(hosts, Trie, #{}),
    maps:fold(
      fun(Host, HostTrie, Acc) ->
              gather_routes(HostTrie, [], Host, Acc)
      end,
      [],
      Hosts
     ).

gather_routes(Node, AccSegs, Host, Acc0) ->
    Term = maps:get(terminal, Node, #{}),
    Acc1 =
        maps:fold(
          fun(M, Payload, A) ->
                  Path = render_path(AccSegs),
                  [{Host, Path, M, Payload} | A]
          end,
          Acc0,
          Term
         ),
    Cs = maps:get(children, Node, #{}),
    maps:fold(
      fun(K, Child, A) ->
              gather_routes(Child, AccSegs ++ [K], Host, A)
      end,
      Acc1,
      Cs
     ).

%% Methods

-spec norm_method(method_in()) -> method().
norm_method(M) when is_binary(M) ->
    %% Assume already normalized (e.g. <<"GET">>, <<"POST">>, <<"ALL">>)
    M;
norm_method(M) when is_atom(M) ->
    list_to_binary(string:uppercase(atom_to_list(M)));
norm_method(M) when is_list(M) ->
    list_to_binary(string:uppercase(M)).

terminal_add(M, Payload, Node0=#{terminal := Term0}) ->
    Node0#{terminal := Term0#{ M => Payload }}.

%% Resolve payload for a given method at a terminal node
-spec method_payload(method(), trie_node()) -> {ok, term()} | error.
method_payload(M, #{terminal := Term}) ->
    case M of
        <<"ALL">> ->
            case maps:find(<<"ALL">>, Term) of
                {ok, Payload} ->
                    {ok, Payload};
                error ->
                    case maps:to_list(Term) of
                        []           -> error;
                        [{_, P} | _] -> {ok, P}
                    end
            end;
        _ ->
            case maps:find(M, Term) of
                {ok, Payload} ->
                    {ok, Payload};
                error ->
                    case maps:find(<<"ALL">>, Term) of
                        {ok, Payload} -> {ok, Payload};
                        error         -> error
                    end
            end
    end.

%% method_member(M, Node) ->
%%     case method_payload(M, Node) of
%%         {ok, _} -> true;
%%         error   -> false
%%     end.

methods_list(#{terminal := Term}) ->
    [K || {K, _} <- maps:to_list(Term)].

render_method(M) when is_binary(M) ->
    M;
render_method(M) ->
    norm_method(M).

%% Segments and paths

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

%% Older-Erlang-safe binary join
-spec join_with_sep([binary()], binary()) -> binary().
join_with_sep([], _Sep) ->
    <<>>;
join_with_sep([First | Rest], Sep) ->
    iolist_to_binary(
      lists:foldl(
        fun(Bin, Acc) -> [Acc, Sep, Bin] end,
        First,
        Rest
       )).

render_path(Segs) ->
    case Segs of
        []               -> <<"/">>;
        [<<"/">>]        -> <<"/">>;
        [<<"/">> | Rest] ->
            RestBins = [render_key(S) || S <- Rest],
            <<"/", (join_with_sep(RestBins, <<"/">>))/binary>>;
        _ ->
            join_with_sep([render_key(S) || S <- Segs], <<"/">>)
    end.

%% Insert without conflict checking (single-host trie)

insert_segs(M, Payload, [], N0) ->
    terminal_add(M, Payload, N0);
insert_segs(M, Payload, [K | Rest], N0) ->
    Cs0 = maps:get(children, N0),
    Child0 = maps:get(K, Cs0, new_node()),
    Child1 = insert_segs(M, Payload, Rest, Child0),
    N0#{children := maps:put(K, Child1, Cs0)}.

%% Conflict helpers

%% Find any wildcard child
find_wild_child(Cs) ->
    maps:fold(
      fun
          ({wild, Var}, Child, none) -> {wild, Var, Child};
          (_K, _Child, Acc)          -> Acc
             end, none, Cs).

%% Find static child that will be affected by new wildcard at same depth
find_static_overshadow_child(Cs, M) ->
    maps:fold(
      fun
          ({wild, _}, _Child, Acc) ->
                     Acc;
          (K, Child, none) ->
                     Ms = methods_list(Child),
                     case overshadow_methods(M, Ms) of
                         true  -> {found, {K, Child, Ms}};
                         false -> none
                     end;
          (_K, _Child, Acc) ->
                     Acc
             end, none, Cs).

%% Does method M conflict (overlap) with existing methods?
overshadow_methods(M, ExistingMs) ->
    case M of
        <<"ALL">> ->
            ExistingMs =/= [];
        _ ->
            lists:member(M, ExistingMs) orelse
                lists:member(<<"ALL">>, ExistingMs)
    end.

%% Safe insert with conflict checking (single-host trie)
safe_insert_segs(M, Payload, [], N0, Prefix, Full) ->
    Ms = methods_list(N0),
    case lists:member(M, Ms) of
        true ->
            {error, conflict, #{
                                reason          => duplicate_pattern,
                                at              => Prefix,
                                existing        => undefined,
                                incoming        => undefined,
                                conflicts_with  => render_path(Prefix),
                                incoming_path   => render_path(Full),
                                method          => M,
                                existing_methods=> Ms
                               }};
        false ->
            case lists:member(<<"ALL">>, Ms) of
                true when M =/= <<"ALL">> ->
                    {error, conflict, #{
                                        reason          => duplicate_due_to_all,
                                        at              => Prefix,
                                        existing        => undefined,
                                        incoming        => undefined,
                                        conflicts_with  => render_path(Prefix),
                                        incoming_path   => render_path(Full),
                                        method          => M,
                                        existing_methods=> Ms
                                       }};
                _ when M =:= <<"ALL">>, Ms =/= [] ->
                    {error, conflict, #{
                                        reason          => duplicate_due_to_existing_methods,
                                        at              => Prefix,
                                        existing        => undefined,
                                        incoming        => undefined,
                                        conflicts_with  => render_path(Prefix),
                                        incoming_path   => render_path(Full),
                                        method          => M,
                                        existing_methods=> Ms
                                       }};
                _ ->
                    {ok, terminal_add(M, Payload, N0)}
            end
    end;

%% Branch when we insert a wildcard segment
safe_insert_segs(M, Payload, [K = {wild, NewVar} | Rest], N0, Prefix, Full) ->
    Cs0 = maps:get(children, N0),

    %% 1) Wildcard overshadowing existing static terminal at same depth
    case Rest of
        [] ->
            case find_static_overshadow_child(Cs0, M) of
                {found, {ExistingKey, _ChildN, ExistingMs}} ->
                    {error, conflict, #{
                                        reason          => overshadowing_route,
                                        at              => Prefix,
                                        existing        => ExistingKey,
                                        incoming        => K,
                                        conflicts_with  => render_path(Prefix ++ [ExistingKey]),
                                        incoming_path   => render_path(Full),
                                        method          => M,
                                        existing_methods=> ExistingMs
                                       }};
                none ->
                    wildcard_name_insert(M, Payload, NewVar, Rest,
                                         N0, Prefix, Full, Cs0)
            end;
        _ ->
            wildcard_name_insert(M, Payload, NewVar, Rest,
                                 N0, Prefix, Full, Cs0)
    end;

%% Branch when we insert a static segment
safe_insert_segs(M, Payload, [K | Rest], N0, Prefix, Full) ->
    Cs0 = maps:get(children, N0),

    %% Static overshadowing existing wildcard terminal at same depth
    case Rest of
        [] ->
            case find_wild_child(Cs0) of
                {wild, ExistingVar, ChildN} ->
                    ExistingMs = methods_list(ChildN),
                    case overshadow_methods(M, ExistingMs) of
                        true ->
                            {error, conflict, #{
                                                reason          => overshadowing_route,
                                                at              => Prefix,
                                                existing        => {wild, ExistingVar},
                                                incoming        => K,
                                                conflicts_with  => render_path(Prefix ++ [{wild, ExistingVar}]),
                                                incoming_path   => render_path(Full),
                                                method          => M,
                                                existing_methods=> ExistingMs
                                               }};
                        false ->
                            static_continue(M, Payload, K, Rest,
                                            N0, Prefix, Full, Cs0)
                    end;
                none ->
                    static_continue(M, Payload, K, Rest,
                                    N0, Prefix, Full, Cs0)
            end;
        _ ->
            static_continue(M, Payload, K, Rest,
                            N0, Prefix, Full, Cs0)
    end.

%% Helper: continue wildcard insertion after overshadow/name checks
wildcard_name_insert(M, Payload, NewVar, Rest, N0, Prefix, Full, Cs0) ->
    ExistingWild = find_wild_child(Cs0),
    case ExistingWild of
        none ->
            Child0 = new_node(),
            case safe_insert_segs(M, Payload, Rest, Child0,
                                  Prefix ++ [{wild, NewVar}], Full) of
                {ok, Child1} ->
                    {ok, N0#{children := maps:put({wild, NewVar}, Child1, Cs0)}};
                {error, conflict, Info} ->
                    {error, conflict, Info}
            end;
        {wild, ExistingVar, ChildN} ->
            if ExistingVar =:= NewVar ->
                    case safe_insert_segs(M, Payload, Rest, ChildN,
                                          Prefix ++ [{wild, NewVar}], Full) of
                        {ok, Child1} ->
                            {ok, N0#{children := maps:put({wild, ExistingVar}, Child1, Cs0)}};
                        {error, conflict, Info} ->
                            {error, conflict, Info}
                    end;
               true ->
                    {error, conflict, #{
                                        reason          => wildcard_name_conflict,
                                        at              => Prefix,
                                        existing        => {wild, ExistingVar},
                                        incoming        => {wild, NewVar},
                                        conflicts_with  => render_path(Prefix ++ [{wild, ExistingVar}]),
                                        incoming_path   => render_path(Prefix ++ [{wild, NewVar}] ++ Rest),
                                        method          => M,
                                        existing_methods=> methods_list(ChildN)
                                       }}
            end
    end.

%% Helper: continue static insertion after overshadow check
static_continue(M, Payload, K, Rest, N0, Prefix, Full, Cs0) ->
    Child0 = maps:get(K, Cs0, new_node()),
    case safe_insert_segs(M, Payload, Rest, Child0,
                          Prefix ++ [K], Full) of
        {ok, Child1} ->
            {ok, N0#{children := maps:put(K, Child1, Cs0)}};
        {error, conflict, Info} ->
            {error, conflict, Info}
    end.

%%--------------------------------------------------------------------
%% Matching (runtime lookup)
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% to_list helpers
%%--------------------------------------------------------------------

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

%%====================================================================
%% EUnit tests
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

default_host_insert_and_lookup_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/users">>, get, payload1, T0),

    ?assert(member(get, <<"/users">>, T1)),
    ?assertMatch({ok, _Node, payload1, #{}},
                 lookup(get, <<"/users">>, T1)).

binary_method_insert_and_lookup_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/users">>, <<"GET">>, payload1, T0),

    %% lookup using binary method
    ?assertMatch({ok, _Node, payload1, #{}},
                 lookup(<<"GET">>, <<"/users">>, T1)),
    %% lookup using atom method
    ?assertMatch({ok, _Node, payload1, #{}},
                 lookup(get, <<"/users">>, T1)).

wildcard_path_lookup_test() ->
    T0 = new(),
    {ok, T1} = insert('_', <<"/users/:id">>, get, payload1, T0,
                      #{strict => true}),

    ?assertMatch({ok, _Node, payload1, #{<<"id">> := <<"42">>}},
                 lookup(get, <<"/users/42">>, T1)),
    ?assertMatch({ok, _Node, payload1, #{<<"id">> := <<"abc">>}},
                 lookup(get, <<"/users/abc">>, T1)).

method_filtering_lookup_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/users">>, post, payload_post, T0),

    ?assertMatch({ok, _Node, payload_post, #{}},
                 lookup(post, <<"/users">>, T1)),
    ?assertEqual(error,
                 lookup(get, <<"/users">>, T1)).

host_specific_only_test() ->
    T0   = new(),
    Host = <<"http://api.example.com">>,

    {ok, T1} = insert(Host, <<"/users">>, get, payload_host, T0),

    ?assertMatch({ok, _Node, payload_host, #{}},
                 lookup(get, Host, <<"/users">>, T1)),
    ?assertEqual(error,
                 lookup(get,
                        <<"http://other.example.com">>,
                        <<"/users">>, T1)).

host_fallback_to_catchall_test() ->
    T0 = new(),
    %% insert only on '_' host
    {ok, T1} = insert('_', <<"/users">>, get, payload_all, T0, #{}),

    %% should match when querying with another host due to fallback
    ?assertMatch({ok, _Node, payload_all, #{}},
                 lookup(get,
                        <<"http://api.example.com">>,
                        <<"/users">>, T1)).

host_and_method_lookup_test() ->
    T0   = new(),
    Host = <<"http://api.example.com">>,

    {ok, T1} = insert(Host, <<"/users">>, post, payload_post, T0),

    ?assertMatch({ok, _Node, payload_post, #{}},
                 lookup(post, Host, <<"/users">>, T1)),
    ?assertEqual(error,
                 lookup(get, Host, <<"/users">>, T1)),
    %% and also check that another host falls back to '_' only if '_' exists
    ?assertEqual(error,
                 lookup(post,
                        <<"http://other.example.com">>,
                        <<"/users">>, T1)).

strict_conflict_duplicate_pattern_test() ->
    T0 = new(),
    {ok, T1} =
        insert('_', <<"/users/:id">>, get, payload1, T0,
               #{strict => true}),

    {error, conflict, Conf} =
        insert('_', <<"/users/:id">>, get, payload2, T1,
               #{strict => true}),

    ?assertEqual(duplicate_pattern, maps:get(reason, Conf)),
    ?assertEqual(<<"GET">>, maps:get(method, Conf)).

overshadow_strict_conflict_static_then_wild_test() ->
    T0 = new(),
    {ok, T1} = insert('_', <<"/user/my_user">>, get, payload_static, T0,
                      #{strict => true}),

    {error, conflict, Conf} =
        insert('_', <<"/user/:user_id">>, get, payload_wild, T1,
               #{strict => true}),

    ?assertEqual(overshadowing_route, maps:get(reason, Conf)),
    ?assertEqual(<<"GET">>, maps:get(method, Conf)),
    ?assertEqual(<<"/user/my_user">>, maps:get(conflicts_with, Conf)),
    ?assertEqual(<<"/user/:user_id">>, maps:get(incoming_path, Conf)).

overshadow_strict_conflict_wild_then_static_test() ->
    T0 = new(),
    {ok, T1} = insert('_', <<"/user/:user_id">>, get, payload_wild, T0,
                      #{strict => true}),

    {error, conflict, Conf} =
        insert('_', <<"/user/my_user">>, get, payload_static, T1,
               #{strict => true}),

    ?assertEqual(overshadowing_route, maps:get(reason, Conf)),
    ?assertEqual(<<"GET">>, maps:get(method, Conf)),
    ?assertEqual(<<"/user/:user_id">>, maps:get(conflicts_with, Conf)),
    ?assertEqual(<<"/user/my_user">>, maps:get(incoming_path, Conf)).

overshadow_non_strict_warning_static_then_wild_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/user/my_user">>, get, payload_static, T0),

    %% This should only warn, not error
    {ok, T2} = insert(<<"/user/:user_id">>, get, payload_wild, T1),

    %% /user/my_user should still match the static route
    ?assertMatch({ok, _Node, payload_static, #{}},
                 lookup(get, <<"/user/my_user">>, T2)),
    %% and /user/other should match the wildcard route
    ?assertMatch({ok, _Node, payload_wild,
                  #{<<"user_id">> := <<"other">>}},
                 lookup(get, <<"/user/other">>, T2)).

overshadow_non_strict_warning_wild_then_static_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/user/:user_id">>, get, payload_wild, T0),

    %% This should only warn, not error
    {ok, T2} = insert(<<"/user/my_user">>, get, payload_static, T1),

    %% /user/my_user should match the static route
    ?assertMatch({ok, _Node, payload_static, #{}},
                 lookup(get, <<"/user/my_user">>, T2)),
    %% and /user/other should match the wildcard route
    ?assertMatch({ok, _Node, payload_wild,
                  #{<<"user_id">> := <<"other">>}},
                 lookup(get, <<"/user/other">>, T2)).

to_list_simple_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/users/:id">>, get, payload1, T0),
    Lines = to_list(T1),

    %% We expect "GET /users/:id" in the list
    ?assert(lists:member(<<"GET /users/:id">>, Lines)).

lookup_returns_node_and_bindings_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"localhost">>, <<"/user/:id">>, get, payload1, T0,
                      #{strict => true}),
    {ok, Node, Payload, Binds} = lookup(get, <<"localhost">>, <<"/user/42">>, T1),
    ?assert(is_map(Node)),
    ?assertEqual(payload1, Payload),
    ?assertMatch(#{<<"id">> := <<"42">>}, Binds).

foldl_can_filter_routes_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/a">>, get, payload_a, T0),
    {ok, T2} = insert(<<"/b">>, get, payload_b, T1),

    {ok, T3} =
        foldl(
          T2,
          fun(Routes0) ->
                  [R || R = {_Host, Path, _M, _P} <- Routes0,
                        Path =/= <<"/b">>]
          end
         ),

    ?assertMatch({ok, _Node, payload_a, #{}},
                 lookup(get, <<"/a">>, T3)),
    ?assertEqual(error,
                 lookup(get, <<"/b">>, T3)).


foldl_can_rewrite_payloads_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/a">>, get, payload_a, T0),
    {ok, T2} = insert(<<"/b">>, get, payload_b, T1),

    %% Byt payload för /a men låt /b vara oförändrad
    {ok, T3} =
        foldl(
          T2,
          fun(Routes0) ->
              [case R of
                   {Host, <<"/a">>, <<"GET">>, payload_a} ->
                       {Host, <<"/a">>, <<"GET">>, payload_a_v2};
                   _ ->
                       R
               end || R <- Routes0]
          end
        ),

    ?assertMatch({ok, _NodeA, payload_a_v2, #{}},
                 lookup(get, <<"/a">>, T3)),
    ?assertMatch({ok, _NodeB, payload_b, #{}},
                 lookup(get, <<"/b">>, T3)).

foldl_can_rewrite_methods_test() ->
    T0 = new(),
    {ok, T1} = insert(<<"/a">>, get, payload_a, T0),

    %% Flytta routen från GET till POST (vi skickar 'post' som atom för att
    %% samtidigt testa att from_list/insert normaliserar method korrekt)
    {ok, T2} =
        foldl(
          T1,
          fun(Routes0) ->
              [case R of
                   {Host, <<"/a">>, <<"GET">>, Payload} ->
                       {Host, <<"/a">>, post, Payload};
                   _ ->
                       R
               end || R <- Routes0]
          end
        ),

    %% GET ska inte längre matcha
    ?assertEqual(error,
                 lookup(get, <<"/a">>, T2)),
    %% POST ska matcha och ge samma payload
    ?assertMatch({ok, _Node, payload_a, #{}},
                 lookup(post, <<"/a">>, T2)).

-endif.
