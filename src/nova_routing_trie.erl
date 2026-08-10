%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Host-aware routing trie. This is Nova's dispatch table: a map from
%%% host to a tree of path segments, where each terminal node holds one
%%% payload per comparator (HTTP method).
%%%
%%% Paths are declared with three kinds of segment:
%%%
%%% <ul>
%%%   <li>`"/users"' - a literal segment.</li>
%%%   <li>`"/users/:id"' - a binding. Matches one segment and binds it
%%%       under `&lt;&lt;"id"&gt;&gt;' in the returned bindings map.</li>
%%%   <li>`"/assets/[...]"' - a catch-all. Matches zero or more trailing
%%%       segments, which are returned as `PathInfo'. Only valid as the
%%%       last segment of a path.</li>
%%% </ul>
%%%
%%% A path may also be an integer, in which case it denotes an HTTP status
%%% code rather than a URL. Nova uses this to register error pages.
%%%
%%% Matching is deterministic: at each depth a literal segment is tried
%%% first, then each binding in name order, then the catch-all. Matching
%%% backtracks, so `"/a/:x/c"' still matches `"/a/b/c"' even when a
%%% `"/a/b/d"' route exists.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_routing_trie).

-export([
         new/0,
         new/1,

         insert/4,
         insert/5,
         insert/6,

         find/4,

         member/3,
         member/4,

         routes/1,
         to_list/1,
         from_list/1,
         from_list/2,
         foldl/2
        ]).

-include_lib("kernel/include/logger.hrl").

-define(ROOT, <<"/">>).

-opaque trie() :: #{
                    options := options(),
                    hosts   := #{host_key() => trie_node()}
                   }.

-opaque trie_node() :: #{
                         children := #{child_key() => trie_node()},
                         terminal := #{comparator() => payload()}
                        }.

-type options() :: #{
                     strict       := boolean(),
                     on_duplicate := keep_first | overwrite,
                     atom()       => term()
                    }.

-type child_key() :: binary() | {binding, binary()} | '...' | integer().

-type host_key() :: '_' | binary().
-type host_in()  :: '_' | binary() | list() | atom().

%% A path is either a URL or an HTTP status code.
-type path() :: binary() | list() | integer().

%% Stored comparator: '_' matches any, otherwise an uppercase method binary.
-type comparator()    :: '_' | binary().
-type comparator_in() :: comparator() | atom() | list().

-type payload()  :: term().
-type bindings() :: #{binary() => binary()}.

%% The canonical unit the trie can be rebuilt from. Note that the host form
%% is unambiguous: a route is always a 4-tuple unless per-insert options are
%% supplied, in which case it is a 5-tuple.
-type route() ::
        {path(), comparator_in(), payload()} |
        {host_in(), path(), comparator_in(), payload()} |
        {host_in(), path(), comparator_in(), payload(), map()}.

-type conflict() :: #{
                      reason           := atom(),
                      at               := [child_key()],
                      existing         := child_key() | undefined,
                      incoming         := child_key() | undefined,
                      conflicts_with   := path(),
                      incoming_path    := path(),
                      comparator       := comparator(),
                      existing_methods := [comparator()]
                     }.

-export_type([trie/0, trie_node/0, route/0, bindings/0, comparator/0, conflict/0]).

%%====================================================================
%% API
%%====================================================================

-spec new() -> trie().
new() ->
    new(#{}).

%%--------------------------------------------------------------------
%% @doc
%% Create an empty trie. Recognised options are `strict' (also accepted
%% as `use_strict' for compatibility) and `on_duplicate', which is either
%% `keep_first' (the default) or `overwrite'.
%% @end
%%--------------------------------------------------------------------
-spec new(map()) -> trie().
new(Opts) when is_map(Opts) ->
    #{options => norm_options(Opts), hosts => #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Insert a route. `Path' is a URL or an HTTP status code, `Comparator'
%% is an HTTP method or `'_'' to match any method.
%%
%% Returns `{error, conflict, Conflict}' when the trie is in strict mode
%% and the route clashes with an existing one. In non-strict mode a clash
%% is logged and resolved according to the `on_duplicate' option.
%% @end
%%--------------------------------------------------------------------
-spec insert(path(), comparator_in(), payload(), trie()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(Path, Comparator, Payload, Trie) ->
    insert('_', Path, Comparator, Payload, Trie, #{}).

-spec insert(host_in(), path(), comparator_in(), payload(), trie()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(HostIn, Path, Comparator, Payload, Trie) ->
    insert(HostIn, Path, Comparator, Payload, Trie, #{}).

-spec insert(host_in(), path(), comparator_in(), payload(), trie(), map()) ->
          {ok, trie()} | {error, conflict, conflict()}.
insert(HostIn, Path, ComparatorIn, Payload, Trie = #{options := RootOpts, hosts := Hosts}, Opts) ->
    Host = norm_host(HostIn),
    Comparator = norm_comparator(ComparatorIn),
    Options = norm_options(maps:merge(RootOpts, Opts)),
    Segments = parse_path(Path),
    HostTrie = maps:get(Host, Hosts, new_node()),
    case do_insert(Segments, Comparator, Payload, HostTrie, Options, [], Segments) of
        {ok, HostTrie0} ->
            {ok, Trie#{hosts := Hosts#{Host => HostTrie0}}};
        {error, conflict, _Conflict} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Look up a route.
%%
%% A host-specific tree is consulted when one exists for `Host', and the
%% `'_'' tree otherwise. `PathInfo' is only returned when the match was
%% made by a `[...]' catch-all and there were trailing segments to report.
%%
%% `{error, comparator_not_found, AllowedMethods}' means the path matched
%% but the method did not, which is what Nova turns into a 405.
%% @end
%%--------------------------------------------------------------------
-spec find(host_in(), path(), comparator_in(), trie()) ->
          {ok, bindings(), payload()} |
          {ok, bindings(), payload(), PathInfo :: [binary()]} |
          {error, not_found} |
          {error, comparator_not_found, [comparator()]}.
find(HostIn, Path, ComparatorIn, Trie) ->
    case host_trie(norm_host(HostIn), Trie) of
        error ->
            {error, not_found};
        {ok, HostTrie} ->
            case match(parse_lookup_path(Path), HostTrie, #{}) of
                error ->
                    {error, not_found};
                {ok, #{terminal := Terminal}, Bindings, PathInfo} ->
                    case resolve(norm_comparator(ComparatorIn), Terminal) of
                        {ok, Payload} when PathInfo =:= [] ->
                            {ok, Bindings, Payload};
                        {ok, Payload} ->
                            {ok, Bindings, Payload, PathInfo};
                        Error ->
                            Error
                    end
            end
    end.

-spec member(host_in(), path(), trie()) -> boolean().
member(HostIn, Path, Trie) ->
    member(HostIn, Path, '_', Trie).

-spec member(host_in(), path(), comparator_in(), trie()) -> boolean().
member(HostIn, Path, Comparator, Trie) ->
    case find(HostIn, Path, Comparator, Trie) of
        {ok, _Bindings, _Payload}           -> true;
        {ok, _Bindings, _Payload, _PathInfo} -> true;
        _                                   -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Every route in the trie, as `{Host, Path, Comparator, Payload}'. This
%% is the supported way to introspect a dispatch table; the trie itself is
%% opaque. The result can be fed straight back into {@link from_list/1}.
%% @end
%%--------------------------------------------------------------------
-spec routes(trie()) -> [route()].
routes(#{hosts := Hosts}) ->
    gather_routes(maps:to_list(Hosts), []).

%%--------------------------------------------------------------------
%% @doc
%% A flat, human-readable listing of the routing table, one entry per
%% method and path. Payloads are not included - use {@link routes/1} when
%% you need them.
%% @end
%%--------------------------------------------------------------------
-spec to_list(trie()) -> [binary()].
to_list(Trie) ->
    [render_route(Comparator, Path) || {_Host, Path, Comparator, _Payload} <- routes(Trie)].

-spec from_list([route()]) -> {ok, trie()} | {error, conflict, conflict()}.
from_list(Routes) ->
    from_list(Routes, #{}).

-spec from_list([route()], map()) -> {ok, trie()} | {error, conflict, conflict()}.
from_list(Routes, RootOpts) when is_list(Routes), is_map(RootOpts) ->
    insert_routes(Routes, new(RootOpts)).

%%--------------------------------------------------------------------
%% @doc
%% Rebuild the trie from a transformation of its routes. `Fun' is handed
%% every route in the table and returns the routes the new table should
%% contain, which makes it the way to filter or rewrite the dispatch table
%% wholesale.
%% @end
%%--------------------------------------------------------------------
-spec foldl(trie(), fun(([route()]) -> [route()])) ->
          {ok, trie()} | {error, conflict, conflict()}.
foldl(Trie = #{options := Options}, Fun) when is_function(Fun, 1) ->
    case Fun(routes(Trie)) of
        Routes when is_list(Routes) ->
            from_list(Routes, Options);
        Other ->
            erlang:error({badreturn, {foldl, Fun, Other}})
    end.

%%====================================================================
%% Internal functions - construction
%%====================================================================

-spec new_node() -> trie_node().
new_node() ->
    #{children => #{}, terminal => #{}}.

-spec norm_options(map()) -> options().
norm_options(Opts) ->
    Strict = maps:get(strict, Opts, maps:get(use_strict, Opts, false)) =:= true,
    OnDuplicate =
        case maps:get(on_duplicate, Opts, keep_first) of
            overwrite -> overwrite;
            _         -> keep_first
        end,
    Opts#{strict => Strict, on_duplicate => OnDuplicate}.

-spec norm_host(host_in()) -> host_key().
norm_host('_')                       -> '_';
norm_host(Host) when is_binary(Host) -> Host;
norm_host(Host) when is_list(Host)   -> unicode:characters_to_binary(Host);
norm_host(Host) when is_atom(Host)   -> atom_to_binary(Host, utf8).

-spec norm_comparator(comparator_in()) -> comparator().
norm_comparator('_')                    -> '_';
norm_comparator(C) when is_binary(C)    -> string:uppercase(C);
norm_comparator(C) when is_atom(C)      -> string:uppercase(atom_to_binary(C, utf8));
norm_comparator(C) when is_list(C)      -> string:uppercase(unicode:characters_to_binary(C)).

%%====================================================================
%% Internal functions - path parsing
%%====================================================================

%% Parse a declared route path into trie keys.
-spec parse_path(path()) -> [child_key()].
parse_path(StatusCode) when is_integer(StatusCode) ->
    [StatusCode];
parse_path(Path) when is_list(Path) ->
    parse_path(unicode:characters_to_binary(Path));
parse_path(Path) when is_binary(Path) ->
    [?ROOT | to_keys(split(Path), [])];
parse_path(Path) ->
    throw({error, {badly_formed_route, Path}}).

to_keys([], Acc) ->
    lists:reverse(Acc);
to_keys([<<"[...]">>], Acc) ->
    lists:reverse(['...' | Acc]);
to_keys([<<"[...]">> | _Tl], _Acc) ->
    throw({bad_routingfile, wildcard_not_last_in_path});
to_keys([<<":", Name/binary>> | Tl], Acc) ->
    to_keys(Tl, [{binding, Name} | Acc]);
to_keys([Segment | Tl], Acc) ->
    to_keys(Tl, [Segment | Acc]).

%% Parse an incoming request path into segments to match against.
-spec parse_lookup_path(path()) -> [binary() | integer()].
parse_lookup_path(StatusCode) when is_integer(StatusCode) ->
    [StatusCode];
parse_lookup_path(Path) when is_binary(Path) ->
    [?ROOT | canonicalise(split(Path), [])];
parse_lookup_path(Path) when is_list(Path) ->
    case lists:all(fun erlang:is_integer/1, Path) of
        true ->
            %% A flat string.
            parse_lookup_path(unicode:characters_to_binary(Path));
        false ->
            %% Already-split segments.
            Segments = [seg_to_binary(S) || S <- Path],
            [?ROOT | canonicalise([S || S <- Segments, S =/= <<>>], [])]
    end.

seg_to_binary(S) when is_binary(S) -> S;
seg_to_binary(S) when is_list(S)   -> unicode:characters_to_binary(S);
seg_to_binary(S) when is_atom(S)   -> atom_to_binary(S, utf8).

split(Path) ->
    [S || S <- binary:split(strip_query(Path), <<"/">>, [global]), S =/= <<>>].

strip_query(Path) ->
    case binary:match(Path, [<<"?">>, <<"#">>]) of
        nomatch    -> Path;
        {Pos, _Len} -> binary:part(Path, 0, Pos)
    end.

%% Resolve "." and ".." within a request path, clamped at the root so a
%% request can never traverse above it.
canonicalise([], Acc) ->
    lists:reverse(Acc);
canonicalise([<<".">> | Tl], Acc) ->
    canonicalise(Tl, Acc);
canonicalise([<<"..">> | Tl], []) ->
    canonicalise(Tl, []);
canonicalise([<<"..">> | Tl], [_Popped | Acc]) ->
    canonicalise(Tl, Acc);
canonicalise([Segment | Tl], Acc) ->
    canonicalise(Tl, [Segment | Acc]).

%%====================================================================
%% Internal functions - insertion
%%====================================================================

do_insert([], Comparator, Payload, Node = #{terminal := Terminal}, Options, Prefix, Full) ->
    case duplicate_reason(Comparator, Terminal) of
        none ->
            {ok, Node#{terminal := Terminal#{Comparator => Payload}}};
        Reason ->
            Conflict = conflict(Reason, Prefix, undefined, undefined, render_path(Prefix),
                                render_path(Full), Comparator, maps:keys(Terminal)),
            case Options of
                #{strict := true} ->
                    {error, conflict, Conflict};
                #{on_duplicate := overwrite} ->
                    warn_conflict(Conflict),
                    {ok, Node#{terminal := Terminal#{Comparator => Payload}}};
                _ ->
                    warn_conflict(Conflict),
                    {ok, Node}
            end
    end;
do_insert([Key | Rest], Comparator, Payload, Node = #{children := Children}, Options, Prefix, Full) ->
    case ambiguity(Key, Rest, Comparator, Children, Prefix, Full, Options) of
        {error, conflict, _Conflict} = Error ->
            Error;
        ok ->
            Child = maps:get(Key, Children, new_node()),
            case do_insert(Rest, Comparator, Payload, Child, Options, Prefix ++ [Key], Full) of
                {ok, Child0} ->
                    {ok, Node#{children := Children#{Key => Child0}}};
                Error ->
                    Error
            end
    end.

%% A terminal already carrying this comparator is a duplicate route, and so is
%% a concrete method arriving at a terminal that already answers every method.
%%
%% The reverse - '_' arriving where concrete methods already exist - is not a
%% duplicate: both are kept and resolve/2 gives the concrete method priority.
duplicate_reason(Comparator, Terminal) ->
    case maps:is_key(Comparator, Terminal) of
        true ->
            duplicate_pattern;
        false when Comparator =/= '_' ->
            case maps:is_key('_', Terminal) of
                true  -> duplicate_due_to_all;
                false -> none
            end;
        false ->
            none
    end.

%% Overlapping literal/binding routes at the same depth are ambiguous to a
%% reader even though matching resolves them deterministically. Only report
%% that under strict mode - `/users/new' alongside `/users/:id' is an
%% entirely ordinary thing to write.
ambiguity(_Key, _Rest, _Comparator, _Children, _Prefix, _Full, #{strict := false}) ->
    ok;
ambiguity(_Key, Rest, _Comparator, _Children, _Prefix, _Full, _Options) when Rest =/= [] ->
    ok;
ambiguity({binding, Name}, _Rest, Comparator, Children, Prefix, Full, _Options) ->
    case overshadowed_literal(Comparator, Children) of
        {ok, Literal, Methods} ->
            {error, conflict, conflict(overshadowing_route, Prefix, Literal, {binding, Name},
                                       render_path(Prefix ++ [Literal]), render_path(Full),
                                       Comparator, Methods)};
        none ->
            case [N || {binding, N} <- maps:keys(Children), N =/= Name] of
                [Existing | _] ->
                    {error, conflict, conflict(binding_name_conflict, Prefix, {binding, Existing},
                                               {binding, Name},
                                               render_path(Prefix ++ [{binding, Existing}]),
                                               render_path(Full), Comparator,
                                               methods(maps:get({binding, Existing}, Children)))};
                [] ->
                    ok
            end
    end;
ambiguity(Key, _Rest, Comparator, Children, Prefix, Full, _Options) ->
    case overshadowed_binding(Comparator, Children) of
        {ok, Binding, Methods} ->
            {error, conflict, conflict(overshadowing_route, Prefix, Binding, Key,
                                       render_path(Prefix ++ [Binding]), render_path(Full),
                                       Comparator, Methods)};
        none ->
            ok
    end.

overshadowed_literal(Comparator, Children) ->
    overshadowed(Comparator, [{K, C} || {K, C} <- maps:to_list(Children), is_binary(K)]).

overshadowed_binding(Comparator, Children) ->
    overshadowed(Comparator, [{K, C} || K = {binding, _} := C <- Children]).

overshadowed(_Comparator, []) ->
    none;
overshadowed(Comparator, [{Key, Child} | Tl]) ->
    Methods = methods(Child),
    case overlaps(Comparator, Methods) of
        true  -> {ok, Key, Methods};
        false -> overshadowed(Comparator, Tl)
    end.

overlaps('_', Methods) ->
    Methods =/= [];
overlaps(Comparator, Methods) ->
    lists:member(Comparator, Methods) orelse lists:member('_', Methods).

methods(#{terminal := Terminal}) ->
    maps:keys(Terminal).

conflict(Reason, At, Existing, Incoming, ConflictsWith, IncomingPath, Comparator, Methods) ->
    #{reason           => Reason,
      at               => At,
      existing         => Existing,
      incoming         => Incoming,
      conflicts_with   => ConflictsWith,
      incoming_path    => IncomingPath,
      comparator       => Comparator,
      existing_methods => Methods}.

warn_conflict(#{reason := Reason, comparator := Comparator, incoming_path := IncomingPath,
                conflicts_with := ConflictsWith}) ->
    ?LOG_WARNING(#{msg => <<"Conflicting route">>,
                   reason => Reason,
                   method => Comparator,
                   route => IncomingPath,
                   conflicts_with => ConflictsWith}).

insert_routes([], Trie) ->
    {ok, Trie};
insert_routes([Route | Tl], Trie) ->
    case insert_route(Route, Trie) of
        {ok, Trie0}                        -> insert_routes(Tl, Trie0);
        {error, conflict, _Conflict} = Err -> Err
    end.

insert_route({Host, Path, Comparator, Payload, Opts}, Trie) when is_map(Opts) ->
    insert(Host, Path, Comparator, Payload, Trie, Opts);
insert_route({Host, Path, Comparator, Payload}, Trie) ->
    insert(Host, Path, Comparator, Payload, Trie);
insert_route({Path, Comparator, Payload}, Trie) ->
    insert(Path, Comparator, Payload, Trie);
insert_route(Other, _Trie) ->
    erlang:error({bad_route, Other}).

%%====================================================================
%% Internal functions - matching
%%====================================================================

host_trie(Host, #{hosts := Hosts}) ->
    case maps:find(Host, Hosts) of
        {ok, HostTrie} ->
            {ok, HostTrie};
        error when Host =/= '_' ->
            maps:find('_', Hosts);
        error ->
            error
    end.

%% A node only counts as a match when it actually carries a payload, so a
%% partially-matching literal branch falls back to a binding sibling rather
%% than dead-ending.
match([], Node, Bindings) ->
    case is_terminal(Node) of
        true  -> {ok, Node, Bindings, []};
        false -> match_catch_all([], Node, Bindings)
    end;
match([Segment | Rest], Node = #{children := Children}, Bindings) ->
    case match_literal(Segment, Rest, Children, Bindings) of
        {ok, _Node, _Bindings, _PathInfo} = Ok ->
            Ok;
        error ->
            case match_bindings(Segment, Rest, Children, Bindings) of
                {ok, _Node, _Bindings, _PathInfo} = Ok ->
                    Ok;
                error ->
                    match_catch_all([Segment | Rest], Node, Bindings)
            end
    end.

match_literal(Segment, Rest, Children, Bindings) ->
    case maps:find(Segment, Children) of
        {ok, Child} -> match(Rest, Child, Bindings);
        error       -> error
    end.

match_bindings(Segment, Rest, Children, Bindings) when is_binary(Segment) ->
    Names = lists:sort([Name || {binding, Name} := _Child <- Children]),
    try_bindings(Names, Segment, Rest, Children, Bindings);
match_bindings(_Segment, _Rest, _Children, _Bindings) ->
    error.

try_bindings([], _Segment, _Rest, _Children, _Bindings) ->
    error;
try_bindings([Name | Tl], Segment, Rest, Children, Bindings) ->
    Child = maps:get({binding, Name}, Children),
    case match(Rest, Child, Bindings#{Name => Segment}) of
        {ok, _Node, _Bindings, _PathInfo} = Ok ->
            Ok;
        error ->
            try_bindings(Tl, Segment, Rest, Children, Bindings)
    end.

match_catch_all(Remaining, #{children := Children}, Bindings) ->
    case maps:find('...', Children) of
        {ok, Child} ->
            case is_terminal(Child) of
                true  -> {ok, Child, Bindings, Remaining};
                false -> error
            end;
        error ->
            error
    end.

is_terminal(#{terminal := Terminal}) ->
    maps:size(Terminal) > 0.

%% An exact comparator wins, then the catch-all '_'. Anything else is a
%% method-not-allowed, and the caller needs the list for the Allow header.
resolve(Comparator, Terminal) ->
    case maps:find(Comparator, Terminal) of
        {ok, Payload} ->
            {ok, Payload};
        error ->
            case maps:find('_', Terminal) of
                {ok, Payload} -> {ok, Payload};
                error         -> {error, comparator_not_found, lists:sort(maps:keys(Terminal))}
            end
    end.

%%====================================================================
%% Internal functions - traversal
%%====================================================================

gather_routes([], Acc) ->
    Acc;
gather_routes([{Host, HostTrie} | Tl], Acc) ->
    gather_routes(Tl, gather_node(HostTrie, [], Host, Acc)).

gather_node(Node = #{children := Children}, Segments, Host, Acc) ->
    Path = render_path(Segments),
    Acc0 = [{Host, Path, Comparator, Payload}
            || Comparator := Payload <- maps:get(terminal, Node)] ++ Acc,
    gather_children(maps:to_list(Children), Segments, Host, Acc0).

gather_children([], _Segments, _Host, Acc) ->
    Acc;
gather_children([{Key, Child} | Tl], Segments, Host, Acc) ->
    gather_children(Tl, Segments, Host, gather_node(Child, Segments ++ [Key], Host, Acc)).

render_route(Comparator, Path) when is_integer(Path) ->
    <<(render_comparator(Comparator))/binary, " ", (integer_to_binary(Path))/binary>>;
render_route(Comparator, Path) ->
    <<(render_comparator(Comparator))/binary, " ", Path/binary>>.

render_comparator('_') -> <<"'_'">>;
render_comparator(C)   -> C.

render_path([StatusCode]) when is_integer(StatusCode) ->
    StatusCode;
render_path([?ROOT | Rest]) ->
    <<"/", (join([render_key(Key) || Key <- Rest], <<"/">>))/binary>>;
render_path(Segments) ->
    join([render_key(Key) || Key <- Segments], <<"/">>).

render_key({binding, Name})           -> <<":", Name/binary>>;
render_key('...')                     -> <<"[...]">>;
render_key(Key) when is_binary(Key)   -> Key;
render_key(Key) when is_integer(Key)  -> integer_to_binary(Key).

join([], _Sep) ->
    <<>>;
join([Bin], _Sep) ->
    Bin;
join([Bin | Tl], Sep) ->
    <<Bin/binary, Sep/binary, (join(Tl, Sep))/binary>>.
