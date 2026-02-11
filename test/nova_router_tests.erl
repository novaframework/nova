-module(nova_router_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% method_to_binary/1
%%====================================================================

method_to_binary_get_test() ->
    ?assertEqual(<<"GET">>, nova_router:method_to_binary(get)).

method_to_binary_post_test() ->
    ?assertEqual(<<"POST">>, nova_router:method_to_binary(post)).

method_to_binary_put_test() ->
    ?assertEqual(<<"PUT">>, nova_router:method_to_binary(put)).

method_to_binary_delete_test() ->
    ?assertEqual(<<"DELETE">>, nova_router:method_to_binary(delete)).

method_to_binary_options_test() ->
    ?assertEqual(<<"OPTIONS">>, nova_router:method_to_binary(options)).

method_to_binary_head_test() ->
    ?assertEqual(<<"HEAD">>, nova_router:method_to_binary(head)).

method_to_binary_connect_test() ->
    ?assertEqual(<<"CONNECT">>, nova_router:method_to_binary(connect)).

method_to_binary_trace_test() ->
    ?assertEqual(<<"TRACE">>, nova_router:method_to_binary(trace)).

method_to_binary_patch_test() ->
    ?assertEqual(<<"PATCH">>, nova_router:method_to_binary(patch)).

method_to_binary_wildcard_test() ->
    ?assertEqual('_', nova_router:method_to_binary('_')).

method_to_binary_unknown_atom_test() ->
    ?assertEqual('_', nova_router:method_to_binary(foobar)).

%%====================================================================
%% concat_strings/2
%%====================================================================

concat_strings_two_lists_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings("/api", "/users")).

concat_strings_empty_prefix_test() ->
    ?assertEqual("/users", nova_router:concat_strings("", "/users")).

concat_strings_empty_path_test() ->
    ?assertEqual("/api", nova_router:concat_strings("/api", "")).

concat_strings_both_empty_test() ->
    ?assertEqual("", nova_router:concat_strings("", "")).

concat_strings_binary_path1_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings(<<"/api">>, "/users")).

concat_strings_binary_path2_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings("/api", <<"/users">>)).

concat_strings_both_binary_test() ->
    ?assertEqual("/api/users", nova_router:concat_strings(<<"/api">>, <<"/users">>)).

concat_strings_integer_path2_test() ->
    ?assertEqual(404, nova_router:concat_strings("/api", 404)).
