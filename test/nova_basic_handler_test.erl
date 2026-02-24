-module(nova_basic_handler_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% maybe_inject_csrf_token/2 tests
%%====================================================================

inject_token_into_proplist_test() ->
    Vars = [{title, <<"Home">>}],
    Req = #{csrf_token => <<"tok123">>},
    Result = nova_basic_handler:maybe_inject_csrf_token(Vars, Req),
    ?assertEqual(<<"tok123">>, proplists:get_value(csrf_token, Result)).

inject_token_into_proplist_preserves_existing_test() ->
    Vars = [{title, <<"Home">>}, {user, <<"alice">>}],
    Req = #{csrf_token => <<"tok123">>},
    Result = nova_basic_handler:maybe_inject_csrf_token(Vars, Req),
    ?assertEqual(<<"Home">>, proplists:get_value(title, Result)),
    ?assertEqual(<<"alice">>, proplists:get_value(user, Result)).

inject_token_into_map_test() ->
    Vars = #{title => <<"Home">>},
    Req = #{csrf_token => <<"tok123">>},
    Result = nova_basic_handler:maybe_inject_csrf_token(Vars, Req),
    ?assertEqual(<<"tok123">>, maps:get(csrf_token, Result)).

inject_token_into_map_preserves_existing_test() ->
    Vars = #{title => <<"Home">>, user => <<"alice">>},
    Req = #{csrf_token => <<"tok123">>},
    Result = nova_basic_handler:maybe_inject_csrf_token(Vars, Req),
    ?assertEqual(<<"Home">>, maps:get(title, Result)),
    ?assertEqual(<<"alice">>, maps:get(user, Result)).

no_token_in_req_leaves_proplist_unchanged_test() ->
    Vars = [{title, <<"Home">>}],
    Req = #{method => <<"GET">>},
    Result = nova_basic_handler:maybe_inject_csrf_token(Vars, Req),
    ?assertEqual(Vars, Result).

no_token_in_req_leaves_map_unchanged_test() ->
    Vars = #{title => <<"Home">>},
    Req = #{method => <<"GET">>},
    Result = nova_basic_handler:maybe_inject_csrf_token(Vars, Req),
    ?assertEqual(Vars, Result).

inject_token_into_empty_proplist_test() ->
    Result = nova_basic_handler:maybe_inject_csrf_token([], #{csrf_token => <<"tok">>}),
    ?assertEqual([{csrf_token, <<"tok">>}], Result).

inject_token_into_empty_map_test() ->
    Result = nova_basic_handler:maybe_inject_csrf_token(#{}, #{csrf_token => <<"tok">>}),
    ?assertEqual(#{csrf_token => <<"tok">>}, Result).
