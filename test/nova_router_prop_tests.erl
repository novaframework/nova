-module(nova_router_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Generators
%%====================================================================

http_method() ->
    oneof([get, post, put, delete, options, head, connect, trace, patch]).

any_method() ->
    oneof([http_method(), foobar, undefined, '_']).

path_string() ->
    ?LET(Parts,
         list(oneof(["/", non_empty(list(range($a, $z)))])),
         lists:flatten(Parts)).

%%====================================================================
%% Properties
%%====================================================================

%% method_to_binary always returns a binary for known methods, or '_' for unknown
prop_method_to_binary_returns_binary_or_wildcard() ->
    ?FORALL(Method, any_method(),
            begin
                Result = nova_router:method_to_binary(Method),
                is_binary(Result) orelse Result =:= '_'
            end).

%% Known HTTP methods always produce a binary (never '_')
prop_known_method_always_binary() ->
    ?FORALL(Method, http_method(),
            is_binary(nova_router:method_to_binary(Method))).

%% concat_strings with two lists produces a list whose length is the sum
prop_concat_strings_length() ->
    ?FORALL({P1, P2}, {path_string(), path_string()},
            length(nova_router:concat_strings(P1, P2)) =:= length(P1) + length(P2)).

%% concat_strings binary path1 gives same result as list path1
prop_concat_strings_binary_path1_equiv() ->
    ?FORALL({P1, P2}, {path_string(), path_string()},
            nova_router:concat_strings(list_to_binary(P1), P2) =:=
            nova_router:concat_strings(P1, P2)).

%% concat_strings binary path2 gives same result as list path2
prop_concat_strings_binary_path2_equiv() ->
    ?FORALL({P1, P2}, {path_string(), path_string()},
            nova_router:concat_strings(P1, list_to_binary(P2)) =:=
            nova_router:concat_strings(P1, P2)).

%%====================================================================
%% EUnit wrappers for PropEr
%%====================================================================

proper_test_() ->
    Opts = [{numtests, 100}, {to_file, user}],
    [
     {"method_to_binary returns binary or '_'",
      ?_assert(proper:quickcheck(prop_method_to_binary_returns_binary_or_wildcard(), Opts))},
     {"known method always binary",
      ?_assert(proper:quickcheck(prop_known_method_always_binary(), Opts))},
     {"concat_strings length property",
      ?_assert(proper:quickcheck(prop_concat_strings_length(), Opts))},
     {"concat_strings binary path1 equivalence",
      ?_assert(proper:quickcheck(prop_concat_strings_binary_path1_equiv(), Opts))},
     {"concat_strings binary path2 equivalence",
      ?_assert(proper:quickcheck(prop_concat_strings_binary_path2_equiv(), Opts))}
    ].
