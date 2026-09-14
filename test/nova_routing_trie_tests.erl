%%% Parity suite for nova_routing_trie.
%%%
%%% The first half of this file is a port of routing_tree's own EUnit suite,
%%% which is the specification of the behaviour nova_routing_trie replaces.
%%% The second half covers behaviour that is new or deliberately different.
-module(nova_routing_trie_tests).

-include_lib("eunit/include/eunit.hrl").

-import(nova_routing_trie, [new/0, new/1, insert/4, insert/5, insert/6, find/4,
                            member/3, member/4, routes/1, to_list/1, from_list/1,
                            foldl/2]).

%% Insert that asserts success, to keep the tests readable.
ins(Host, Path, Comparator, Payload, Trie) ->
    {ok, Trie0} = insert(Host, Path, Comparator, Payload, Trie),
    Trie0.

%%====================================================================
%% Ported from routing_tree - basic insert and lookup
%%====================================================================

simple_string_lookup_test() ->
    T = ins('_', "/my/route", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my/route">>, "GET", T)).

simple_binary_lookup_test() ->
    T = ins('_', <<"/my/route">>, <<"GET">>, "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my/route">>, <<"GET">>, T)).

list_of_segments_lookup_test() ->
    T = ins('_', "/my/route", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, [<<"my">>, <<"route">>], "GET", T)).

root_path_lookup_test() ->
    T = ins('_', "/", "GET", "ROOT", new()),
    ?assertEqual({ok, #{}, "ROOT"}, find('_', <<"/">>, "GET", T)).

bindings_lookup_test() ->
    T = ins('_', "/my/:route", "GET", "ONE", new()),
    ?assertEqual({ok, #{<<"route">> => <<"monkey">>}, "ONE"},
                 find(<<"my_host">>, <<"/my/monkey">>, "GET", T)).

complex_lookup_test() ->
    T0 = ins('_', "/my/:route", "GET", "ONE", new()),
    T1 = ins('_', "/my/inbox/:message", "POST", "TWO", T0),
    T2 = ins('_', "/my/inbox/:message", "GET", "THREE", T1),
    T3 = ins('_', "/my/inbox", "GET", "FOUR", T2),
    T4 = ins('_', "/", "GET", "FIVE", T3),

    ?assertEqual({ok, #{<<"message">> => <<"hello">>}, "THREE"},
                 find(<<"my_host">>, <<"/my/inbox/hello">>, "GET", T4)),
    ?assertEqual({ok, #{}, "FOUR"}, find(<<"my_host">>, <<"/my/inbox">>, "GET", T4)),
    ?assertEqual({ok, #{}, "FIVE"}, find(<<"my_host">>, <<"/">>, "GET", T4)).

any_comparator_matches_concrete_method_test() ->
    T = ins('_', "/my/route", '_', "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my/route">>, "PUT", T)),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my/route">>, <<"DELETE">>, T)).

dash_in_path_test() ->
    T = ins('_', "/my-test-route", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my-test-route">>, "GET", T)).

trailing_slash_test() ->
    T = ins('_', "/my_app/", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my_app">>, "GET", T)),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my_app/">>, "GET", T)).

double_slash_test() ->
    T = ins('_', "/my/route", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"//my//route">>, "GET", T)).

not_found_test() ->
    T = ins('_', "/my/route", "GET", "ONE", new()),
    ?assertEqual({error, not_found}, find(<<"my_host">>, <<"/nope">>, "GET", T)),
    ?assertEqual({error, not_found}, find(<<"my_host">>, <<"/my">>, "GET", T)).

%%====================================================================
%% Ported from routing_tree - the [...] catch-all
%%====================================================================

catch_all_with_trailing_segments_test() ->
    T = ins('_', "/my/route/[...]", '_', "ONE", new()),
    ?assertEqual({ok, #{}, "ONE", [<<"is">>, <<"amazing">>]},
                 find(<<"my_host">>, <<"/my/route/is/amazing">>, "PUT", T)).

catch_all_with_one_trailing_segment_test() ->
    T = ins('_', "/my/assets/[...]", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE", [<<"logo.png">>]},
                 find(<<"my_host">>, <<"/my/assets/logo.png">>, "GET", T)).

%% routing_tree returned the 3-tuple when the catch-all consumed nothing.
catch_all_with_no_trailing_segments_test() ->
    T = ins('_', "/my/assets/[...]", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find(<<"my_host">>, <<"/my/assets">>, "GET", T)).

catch_all_not_last_in_path_test() ->
    ?assertThrow({bad_routingfile, wildcard_not_last_in_path},
                 insert('_', "/my/assets/[...]/not/working", "GET", "ONE", new())).

%% A literal route below the same prefix still wins over the catch-all.
catch_all_does_not_shadow_literal_test() ->
    T0 = ins('_', "/assets/[...]", "GET", "STATIC", new()),
    T1 = ins('_', "/assets/manifest.json", "GET", "MANIFEST", T0),
    ?assertEqual({ok, #{}, "MANIFEST"}, find('_', <<"/assets/manifest.json">>, "GET", T1)),
    ?assertEqual({ok, #{}, "STATIC", [<<"img">>, <<"logo.png">>]},
                 find('_', <<"/assets/img/logo.png">>, "GET", T1)).

%%====================================================================
%% Ported from routing_tree - status codes
%%====================================================================

status_code_insert_and_lookup_test() ->
    T = ins('_', 404, '_', "NOT_FOUND", new()),
    ?assertEqual({ok, #{}, "NOT_FOUND"}, find('_', 404, '_', T)),
    ?assertEqual({error, not_found}, find('_', 500, '_', T)).

status_codes_do_not_collide_with_paths_test() ->
    T0 = ins('_', 404, '_', "STATUS", new()),
    T1 = ins('_', "/404", "GET", "PATH", T0),
    ?assertEqual({ok, #{}, "STATUS"}, find('_', 404, '_', T1)),
    ?assertEqual({ok, #{}, "PATH"}, find('_', <<"/404">>, "GET", T1)).

%%====================================================================
%% Ported from routing_tree - hosts
%%====================================================================

host_specific_route_test() ->
    Host = <<"api.example.com">>,
    T = ins(Host, "/users", "GET", "HOST", new()),
    ?assertEqual({ok, #{}, "HOST"}, find(Host, <<"/users">>, "GET", T)),
    ?assertEqual({error, not_found}, find(<<"other.example.com">>, <<"/users">>, "GET", T)).

host_falls_back_to_catchall_host_test() ->
    T = ins('_', "/users", "GET", "ANY", new()),
    ?assertEqual({ok, #{}, "ANY"}, find(<<"api.example.com">>, <<"/users">>, "GET", T)).

%% A host-specific tree is used on its own; there is no cascade into '_'.
%% This mirrors routing_tree and is relied on by host-scoped routers.
host_specific_tree_does_not_cascade_test() ->
    Host = <<"api.example.com">>,
    T0 = ins('_', "/shared", "GET", "ANY", new()),
    T1 = ins(Host, "/only-here", "GET", "HOST", T0),
    ?assertEqual({ok, #{}, "HOST"}, find(Host, <<"/only-here">>, "GET", T1)),
    ?assertEqual({error, not_found}, find(Host, <<"/shared">>, "GET", T1)).

%%====================================================================
%% Ported from routing_tree - duplicates and strict mode
%%====================================================================

%% routing_tree kept the first insert and ignored the second.
duplicate_route_keeps_first_test() ->
    T0 = ins('_', "/profile", "GET", "ONE", new()),
    T1 = ins('_', "/profile", "GET", "TWO", T0),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/profile">>, "GET", T1)).

duplicate_route_overwrite_option_test() ->
    T0 = ins('_', "/profile", "GET", "ONE", new(#{on_duplicate => overwrite})),
    T1 = ins('_', "/profile", "GET", "TWO", T0),
    ?assertEqual({ok, #{}, "TWO"}, find('_', <<"/profile">>, "GET", T1)).

strict_duplicate_route_errors_test() ->
    T0 = ins('_', "/profile", "GET", "ONE", new(#{strict => true})),
    {error, conflict, Conflict} = insert('_', "/profile", "GET", "TWO", T0),
    ?assertEqual(duplicate_pattern, maps:get(reason, Conflict)),
    ?assertEqual(<<"GET">>, maps:get(comparator, Conflict)),
    ?assertEqual(<<"/profile">>, maps:get(incoming_path, Conflict)).

%% use_strict is accepted as an alias, so a routing_tree options map works.
strict_accepts_use_strict_alias_test() ->
    T0 = ins('_', "/profile", "GET", "ONE", new(#{use_strict => true, convert_to_binary => true})),
    ?assertMatch({error, conflict, _}, insert('_', "/profile", "GET", "TWO", T0)).

strict_literal_then_binding_errors_test() ->
    T0 = ins('_', "/user/my_user", "GET", "STATIC", new(#{strict => true})),
    {error, conflict, Conflict} = insert('_', "/user/:user_id", "GET", "BINDING", T0),
    ?assertEqual(overshadowing_route, maps:get(reason, Conflict)),
    ?assertEqual(<<"/user/my_user">>, maps:get(conflicts_with, Conflict)),
    ?assertEqual(<<"/user/:user_id">>, maps:get(incoming_path, Conflict)).

strict_binding_then_literal_errors_test() ->
    T0 = ins('_', "/user/:user_id", "GET", "BINDING", new(#{strict => true})),
    {error, conflict, Conflict} = insert('_', "/user/my_user", "GET", "STATIC", T0),
    ?assertEqual(overshadowing_route, maps:get(reason, Conflict)),
    ?assertEqual(<<"/user/:user_id">>, maps:get(conflicts_with, Conflict)),
    ?assertEqual(<<"/user/my_user">>, maps:get(incoming_path, Conflict)).

strict_conflicting_binding_names_error_test() ->
    T0 = ins('_', "/user/:id", "GET", "ONE", new(#{strict => true})),
    {error, conflict, Conflict} = insert('_', "/user/:user_id", "POST", "TWO", T0),
    ?assertEqual(binding_name_conflict, maps:get(reason, Conflict)).

%% Overlapping literal and binding routes are ordinary REST, so they must not
%% error - or even warn - outside strict mode.
non_strict_literal_and_binding_coexist_test() ->
    T0 = ins('_', "/users/new", "GET", "NEW", new()),
    T1 = ins('_', "/users/:id", "GET", "SHOW", T0),
    ?assertEqual({ok, #{}, "NEW"}, find('_', <<"/users/new">>, "GET", T1)),
    ?assertEqual({ok, #{<<"id">> => <<"42">>}, "SHOW"}, find('_', <<"/users/42">>, "GET", T1)).

non_strict_binding_then_literal_coexist_test() ->
    T0 = ins('_', "/users/:id", "GET", "SHOW", new()),
    T1 = ins('_', "/users/new", "GET", "NEW", T0),
    ?assertEqual({ok, #{}, "NEW"}, find('_', <<"/users/new">>, "GET", T1)),
    ?assertEqual({ok, #{<<"id">> => <<"42">>}, "SHOW"}, find('_', <<"/users/42">>, "GET", T1)).

%%====================================================================
%% Method resolution and 405
%%====================================================================

method_specific_routes_test() ->
    T0 = ins('_', "/users", "GET", "LIST", new()),
    T1 = ins('_', "/users", "POST", "CREATE", T0),
    ?assertEqual({ok, #{}, "LIST"}, find('_', <<"/users">>, "GET", T1)),
    ?assertEqual({ok, #{}, "CREATE"}, find('_', <<"/users">>, "POST", T1)).

method_not_allowed_test() ->
    T0 = ins('_', "/users", "GET", "LIST", new()),
    T1 = ins('_', "/users", "PUT", "REPLACE", T0),
    ?assertEqual({error, comparator_not_found, [<<"GET">>, <<"PUT">>]},
                 find('_', <<"/users">>, "DELETE", T1)).

%% A path that exists at all is a 405, not a 404 - that distinction is the
%% whole reason find/4 has two error shapes.
method_not_allowed_is_not_not_found_test() ->
    T = ins('_', "/users", "GET", "LIST", new()),
    ?assertMatch({error, comparator_not_found, _}, find('_', <<"/users">>, "POST", T)),
    ?assertEqual({error, not_found}, find('_', <<"/nope">>, "POST", T)).

%% '_' added after a concrete method: both are kept, and the concrete method
%% still wins for its own verb.
exact_method_beats_any_test() ->
    T0 = ins('_', "/users", "GET", "SPECIFIC", new()),
    T1 = ins('_', "/users", '_', "ANY", T0),
    ?assertEqual({ok, #{}, "SPECIFIC"}, find('_', <<"/users">>, "GET", T1)),
    ?assertEqual({ok, #{}, "ANY"}, find('_', <<"/users">>, "POST", T1)).

%% The reverse order is a duplicate, because '_' already answers GET. Like
%% routing_tree, the first route registered wins.
concrete_method_after_any_is_a_duplicate_test() ->
    T0 = ins('_', "/users", '_', "ANY", new()),
    T1 = ins('_', "/users", "GET", "SPECIFIC", T0),
    ?assertEqual({ok, #{}, "ANY"}, find('_', <<"/users">>, "GET", T1)),
    ?assertEqual({ok, #{}, "ANY"}, find('_', <<"/users">>, "POST", T1)).

lowercase_method_is_normalised_test() ->
    T = ins('_', "/users", get, "LIST", new()),
    ?assertEqual({ok, #{}, "LIST"}, find('_', <<"/users">>, <<"GET">>, T)),
    ?assertEqual({ok, #{}, "LIST"}, find('_', <<"/users">>, "get", T)).

%%====================================================================
%% Matching improvements over routing_tree
%%====================================================================

%% routing_tree committed to the first matching sibling and never backtracked,
%% so this returned not_found.
backtracks_from_literal_to_binding_test() ->
    T0 = ins('_', "/a/b/d", "GET", "LITERAL", new()),
    T1 = ins('_', "/a/:x/c", "GET", "BINDING", T0),
    ?assertEqual({ok, #{<<"x">> => <<"b">>}, "BINDING"}, find('_', <<"/a/b/c">>, "GET", T1)).

%% A literal branch that exists but carries no payload must not dead-end.
backtracks_past_non_terminal_literal_test() ->
    T0 = ins('_', "/a/b/c", "GET", "DEEP", new()),
    T1 = ins('_', "/a/:x", "GET", "BINDING", T0),
    ?assertEqual({ok, #{<<"x">> => <<"b">>}, "BINDING"}, find('_', <<"/a/b">>, "GET", T1)).

%% routing_tree kept whichever binding it happened to visit first, leaving the
%% other permanently unreachable.
multiple_binding_siblings_are_all_reachable_test() ->
    T0 = ins('_', "/p/:id/picture", "GET", "PICTURE", new()),
    T1 = ins('_', "/p/:user_id/name", "GET", "NAME", T0),
    ?assertEqual({ok, #{<<"id">> => <<"7">>}, "PICTURE"}, find('_', <<"/p/7/picture">>, "GET", T1)),
    ?assertEqual({ok, #{<<"user_id">> => <<"7">>}, "NAME"}, find('_', <<"/p/7/name">>, "GET", T1)).

falls_back_to_catch_all_when_binding_dead_ends_test() ->
    T0 = ins('_', "/a/:x/c", "GET", "BINDING", new()),
    T1 = ins('_', "/a/[...]", "GET", "CATCHALL", T0),
    ?assertEqual({ok, #{<<"x">> => <<"b">>}, "BINDING"}, find('_', <<"/a/b/c">>, "GET", T1)),
    ?assertEqual({ok, #{}, "CATCHALL", [<<"b">>, <<"z">>]}, find('_', <<"/a/b/z">>, "GET", T1)).

%%====================================================================
%% Path canonicalisation
%%====================================================================

dot_segments_are_resolved_test() ->
    T = ins('_', "/a/b", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/a/./b">>, "GET", T)),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/a/c/../b">>, "GET", T)).

%% Traversal above the root is clamped rather than escaping it.
dotdot_is_clamped_at_root_test() ->
    T = ins('_', "/a", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/../a">>, "GET", T)),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/../../a">>, "GET", T)),
    ?assertEqual({error, not_found}, find('_', <<"/..">>, "GET", T)).

query_string_is_ignored_test() ->
    T = ins('_', "/search", "GET", "ONE", new()),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/search?q=erlang">>, "GET", T)),
    ?assertEqual({ok, #{}, "ONE"}, find('_', <<"/search#frag">>, "GET", T)).

%%====================================================================
%% member/3,4
%%====================================================================

member_test() ->
    T = ins('_', "/users/:id", "GET", "ONE", new()),
    ?assert(member('_', <<"/users/42">>, "GET", T)),
    ?assertNot(member('_', <<"/users/42">>, "POST", T)),
    ?assertNot(member('_', <<"/nope">>, "GET", T)).

%%====================================================================
%% routes/1, to_list/1, from_list/1, foldl/2
%%====================================================================

routes_round_trip_test() ->
    T0 = ins('_', "/a", "GET", payload_a, new()),
    T1 = ins('_', "/b/:id", "POST", payload_b, T0),
    T2 = ins('_', "/assets/[...]", '_', payload_c, T1),
    T3 = ins('_', 404, '_', payload_d, T2),

    {ok, Rebuilt} = from_list(routes(T3)),

    ?assertEqual({ok, #{}, payload_a}, find('_', <<"/a">>, "GET", Rebuilt)),
    ?assertEqual({ok, #{<<"id">> => <<"1">>}, payload_b}, find('_', <<"/b/1">>, "POST", Rebuilt)),
    ?assertEqual({ok, #{}, payload_c, [<<"x">>]}, find('_', <<"/assets/x">>, "GET", Rebuilt)),
    ?assertEqual({ok, #{}, payload_d}, find('_', 404, '_', Rebuilt)).

%% Map payloads must survive the round trip; an earlier clause ordering
%% reinterpreted them as per-insert options.
routes_round_trip_with_map_payload_test() ->
    T = ins('_', "/a", "GET", #{app => my_app}, new()),
    {ok, Rebuilt} = from_list(routes(T)),
    ?assertEqual({ok, #{}, #{app => my_app}}, find('_', <<"/a">>, "GET", Rebuilt)).

routes_preserves_host_test() ->
    T0 = ins(<<"api.example.com">>, "/a", "GET", host_payload, new()),
    T1 = ins('_', "/a", "GET", any_payload, T0),
    {ok, Rebuilt} = from_list(routes(T1)),
    ?assertEqual({ok, #{}, host_payload}, find(<<"api.example.com">>, <<"/a">>, "GET", Rebuilt)),
    ?assertEqual({ok, #{}, any_payload}, find(<<"other.com">>, <<"/a">>, "GET", Rebuilt)).

to_list_test() ->
    T0 = ins('_', "/users/:id", "GET", payload, new()),
    T1 = ins('_', "/assets/[...]", '_', payload, T0),
    Lines = to_list(T1),
    ?assert(lists:member(<<"GET /users/:id">>, Lines)),
    ?assert(lists:member(<<"'_' /assets/[...]">>, Lines)).

foldl_can_filter_routes_test() ->
    T0 = ins('_', "/a", "GET", payload_a, new()),
    T1 = ins('_', "/b", "GET", payload_b, T0),
    {ok, T2} = foldl(T1, fun(Routes) ->
                                 [R || R = {_Host, Path, _C, _P} <- Routes, Path =/= <<"/b">>]
                         end),
    ?assertEqual({ok, #{}, payload_a}, find('_', <<"/a">>, "GET", T2)),
    ?assertEqual({error, not_found}, find('_', <<"/b">>, "GET", T2)).

foldl_can_rewrite_payloads_test() ->
    T0 = ins('_', "/a", "GET", payload_a, new()),
    T1 = ins('_', "/b", "GET", payload_b, T0),
    {ok, T2} = foldl(T1, fun(Routes) ->
                                 [case R of
                                      {Host, <<"/a">>, C, payload_a} -> {Host, <<"/a">>, C, payload_a_v2};
                                      _ -> R
                                  end || R <- Routes]
                         end),
    ?assertEqual({ok, #{}, payload_a_v2}, find('_', <<"/a">>, "GET", T2)),
    ?assertEqual({ok, #{}, payload_b}, find('_', <<"/b">>, "GET", T2)).

foldl_can_rewrite_methods_test() ->
    T0 = ins('_', "/a", "GET", payload_a, new()),
    {ok, T1} = foldl(T0, fun(Routes) ->
                                 [{Host, Path, post, P} || {Host, Path, _C, P} <- Routes]
                         end),
    ?assertEqual({error, comparator_not_found, [<<"POST">>]}, find('_', <<"/a">>, "GET", T1)),
    ?assertEqual({ok, #{}, payload_a}, find('_', <<"/a">>, "POST", T1)).

foldl_preserves_options_test() ->
    T0 = ins('_', "/a", "GET", payload_a, new(#{strict => true})),
    {ok, T1} = foldl(T0, fun(Routes) -> Routes end),
    ?assertMatch({error, conflict, _}, insert('_', "/a", "GET", payload_b, T1)).

foldl_badreturn_test() ->
    T = ins('_', "/a", "GET", payload_a, new()),
    ?assertError({badreturn, _}, foldl(T, fun(_Routes) -> not_a_list end)).

from_list_bad_route_test() ->
    ?assertError({bad_route, _}, from_list([{"/a"}])).

%%====================================================================
%% Empty trie
%%====================================================================

empty_trie_test() ->
    ?assertEqual({error, not_found}, find('_', <<"/anything">>, "GET", new())),
    ?assertEqual([], routes(new())),
    ?assertEqual([], to_list(new())).
