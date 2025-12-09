%%%-------------------------------------------------------------------
%%% Path trie with wildcards (":var"), HTTP method comparator,
%%% binding capture, and rich safe_insert/3 conflicts.
%%%-------------------------------------------------------------------
-module(routing_trie).

-export([
         new/0,
         new/1,

         %% Method-agnostic APIs (Using method set to `all`)
         insert/2,
         member/2,
         match/2,

         %% Method-aware APIs
         insert/3,
         insert/4,
         member/3,
         match/3,

         find/2,
         to_list/1
        ]).

-opaque trie() :: trie_node().
-opaque trie_node() :: #{
                         options := map(),          %% options for this node
                         children := #{ child_key() => trie_node() },
                         terminal_methods := methods_set()    %% set of methods for which this node is terminal
                        }.
-type child_key() :: binary() | {wild, binary()}.

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
%% Creates a new empty trie.
%% @end
%%--------------------------------------------------------------------
-spec new() -> trie().
new() ->
    new(#{}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty trie with options.
%% Options:
%%  - any key/value pairs to be stored in the root node options map
%% @end
%%--------------------------------------------------------------------
-spec new(map()) -> trie().
new(Opts) when is_map(Opts) ->
    #{children => #{}, terminal_methods => #{}, options => Opts}.


%%--------------------------------------------------------------------
%% @doc
%% Inserts a path into the trie. Same as calling `insert(all, Path, Trie, #{})`
%% @end
%%--------------------------------------------------------------------
-spec insert(iodata(), trie()) -> {ok, trie()} | {error, conflict, conflict()}.
insert(Path, Trie) ->
    Opts = maps:get(options, Trie, #{}),
    insert(all, Path, Trie, Opts).

%%--------------------------------------------------------------------
%% @doc
%% Inserts a path into the trie with options. Same as calling
%% `insert(all, Path, Trie, Opts)`.
%% Options:
%%  - strict: boolean() (default: false)
%%    If true, performs a "safe" insert that checks for conflicts
%%    (wildcard name conflicts, duplicate patterns, etc).
%% @end
-spec insert(iodata(), trie(), map()) -> {ok, trie()} | {error, conflict, conflict()}.
insert(Path, Trie, Opts) ->
    Opts0 = maps:get(options, Trie, #{}),
    Opts1 = maps:merge(Opts0, Opts),
    insert(all, Path, Trie, Opts1).

%%--------------------------------------------------------------------
%% @doc
%% Inserts a method+path into the trie with options. If a node already
%% exists for the given path and method not exists already, the node is added.
%% Otherwise it will return an error with information about the conflict if
%% not `overwrite` option is set to true.
%% Options:
%%  - strict: boolean() (default: false)
%%    If true, performs a "safe" insert that checks for conflicts
%%    (wildcard name conflicts, duplicate patterns, etc).
%%  - overwrite: boolean() (default: false)
%%    If true, will overwrite existing method at the given path if exists.
%% @end
-spec insert(method_in(), iodata(), trie(), map()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Method, Path, Trie, Opts) ->
    Opts0 = maps:get(options, Trie, #{}),
    Opts1 = maps:merge(Opts0, Opts),
    Trie0 = Trie#{options => Opts1},

    M = norm_method(Method),
    Segs = segs_for_insert(Path),

    case maps:get(strict, Opts1, false) of
        true ->
            %% Call safe insert which checks for conflicts
            safe_insert_segs(M, Segs, Trie0, [], Segs);
        _ ->
            insert_segs(M, Segs, Trie0)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks if a path exists in the trie. Same as calling `member(all, Path, Trie)`.
%% @end
%%--------------------------------------------------------------------
-spec member(iodata(), trie()) -> boolean().
member(Path, Trie) ->
    member(all, Path, Trie).


%%--------------------------------------------------------------------
%% @doc
%% Checks if a method+path exists in the trie.
%% @end
%%--------------------------------------------------------------------
-spec member(method_in(), iodata(), trie()) -> boolean().
member(Method0, Path, Trie) ->
    M = norm_method(Method0),
    case find(Path, Trie) of
        {ok, Node} -> method_member(M, Node);
        error      -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Matches a concrete path against the trie, returning bindings if
%% matched. Same as calling `match(all, Path, Trie)`.
%% @end
%%--------------------------------------------------------------------
-spec match(iodata(), trie()) -> {ok, #{binary() => binary()}} | error.
match(Path, Trie) ->
    match(all, Path, Trie).

%%--------------------------------------------------------------------
%% @doc
%% Matches a concrete method+path against the trie, returning bindings if
%% matched.
%% @end
%%--------------------------------------------------------------------
-spec match(method_in(), iodata(), trie()) -> {ok, #{binary() => binary()}} | error.
match(Method0, Path, Trie) ->
    M = norm_method(Method0),
    Segs = segs_for_match(Path),
    case do_match(Segs, Trie, #{}) of
        {ok, Node, Binds} ->
            case  method_member(M, Node) of
                true ->
                    {ok, Binds};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Finds the node for a given path, regardless of method.
%% @end
%%--------------------------------------------------------------------
-spec find(iodata(), trie()) -> {ok, trie_node()} | error.
find(Path, Trie) ->
    Segs = segs_for_insert(Path),
    case descend_pattern(Segs, Trie) of
        undefined -> error;
        Node      -> {ok, Node}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all method+path combinations in the trie.
%% @end
%%--------------------------------------------------------------------
-spec to_list(trie()) -> [binary()].
to_list(Trie) ->
    gather(Trie, [], []).

%%--------------------------------------------------------------------
%% Internals
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

%% --- Segment normalization -----------------------------------------

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
to_key(Bin)                   -> Bin.

render_key({wild, Name}) -> <<":", Name/binary>>;
render_key(Bin)          -> Bin.

render_path(Segs) ->
    case Segs of
        []               -> <<"/">>;
        [<<"/">>]        -> <<"/">>;
        [<<"/">> | Rest] -> <<"/", (binary:join([render_key(S) || S <- Rest], <<"/">>))/binary>>;
        _                -> binary:join([render_key(S) || S <- Segs], <<"/">>)
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

%% --- Terminal helpers ----------------------------------------------

terminal_add(M, Node0=#{terminal_methods := Ms0}) ->
    Node0#{terminal_methods := Ms0#{ M => true }}.

method_member(M, #{terminal_methods := Ms}) ->
    case M of
        all -> maps:is_key(all, Ms) orelse (maps:size(Ms) > 0);
        _   -> maps:is_key(M, Ms) orelse maps:is_key(all, Ms)
    end.

methods_list(#{terminal_methods := Ms}) ->
    [K || {K, true} <- maps:to_list(Ms)].

%% --- Permissive insert (kept for convenience) ----------------------

insert_segs(M, [], N0) ->
    terminal_add(M, N0);
insert_segs(M, [K | Rest], N0) ->
    Cs0 = maps:get(children, N0),
    Child0 = maps:get(K, Cs0, new()),
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

%% --- Safe insert with method-aware conflicts -----------------------

%% Args:
%%  - M: method()
%%  - SegsToInsert, N0, Prefix, Full same as before
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
            Child0 = new(),
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
    Child0 = maps:get(K, Cs0, new()),
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

%% --- Matching with precedence & backtracking -----------------------

do_match([], N, Binds) ->
    {ok, N, Binds};
do_match([Seg | Rest], N, Binds0) ->
    Cs = maps:get(children, N),

    %% 1) Exact first
    Exact = case maps:find(Seg, Cs) of
                {ok, C} -> case do_match(Rest, C, Binds0) of
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

%% --- Enumeration ----------------------------------------------------

gather(N, AccSegs, AccOut) ->
    Ms = methods_list(N),
    AccOut1 =
        case Ms of
            [] -> AccOut;
            _  ->
                Path = render_path(AccSegs),
                MethodLines = [<< (render_method(M))/binary, " ", Path/binary >> || M <- Ms],
                MethodLines ++ AccOut
        end,
    Cs = maps:get(children, N),
    maps:fold(
      fun(K, Child, Out) ->
              gather(Child, AccSegs ++ [K], Out)
      end, AccOut1, Cs).
