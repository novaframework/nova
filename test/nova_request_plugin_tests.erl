-module(nova_request_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> nova_test_helper:setup_nova_env() end).
-define(CLEANUP, fun(Prev) -> nova_test_helper:cleanup_nova_env(Prev) end).

%%====================================================================
%% should_read_body/1
%%====================================================================

should_read_body_empty_test() ->
    ?assertNot(nova_request_plugin:should_read_body([])).

should_read_body_decode_json_test() ->
    ?assert(nova_request_plugin:should_read_body([{decode_json_body, true}])).

should_read_body_urlencoded_test() ->
    ?assert(nova_request_plugin:should_read_body([{read_urlencoded_body, true}])).

should_read_body_unknown_test() ->
    ?assertNot(nova_request_plugin:should_read_body([{parse_qs, true}])).

should_read_body_mixed_test() ->
    ?assert(nova_request_plugin:should_read_body([{parse_qs, true}, {decode_json_body, true}])).

%%====================================================================
%% modulate_state/3 — skip JSON decode for GET/DELETE
%%====================================================================

modulate_state_skip_json_get_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
         {ok, _Req1, state} = nova_request_plugin:modulate_state(
             Req, [{decode_json_body, true}], state)
     end}.

modulate_state_skip_json_delete_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"DELETE">>, <<"/">>),
         {ok, _Req1, state} = nova_request_plugin:modulate_state(
             Req, [{decode_json_body, true}], state)
     end}.

%%====================================================================
%% modulate_state/3 — skip unknown options
%%====================================================================

modulate_state_skip_unknown_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
    {ok, Req1, state} = nova_request_plugin:modulate_state(
        Req, [{unknown_option, true}], state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% modulate_state/3 — empty options
%%====================================================================

modulate_state_empty_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, state} = nova_request_plugin:modulate_state(Req, [], state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% modulate_state/3 — parse_qs
%%====================================================================

modulate_state_parse_qs_true_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Req1 = Req#{qs => <<"foo=bar&baz=qux">>},
    {ok, Req2, state} = nova_request_plugin:modulate_state(
        Req1, [{parse_qs, true}], state),
    Parsed = maps:get(parsed_qs, Req2),
    ?assertEqual(<<"bar">>, maps:get(<<"foo">>, Parsed)),
    ?assertEqual(<<"qux">>, maps:get(<<"baz">>, Parsed)).

modulate_state_parse_qs_list_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Req1 = Req#{qs => <<"a=1&b=2">>},
    {ok, Req2, state} = nova_request_plugin:modulate_state(
        Req1, [{parse_qs, list}], state),
    Parsed = maps:get(parsed_qs, Req2),
    ?assert(is_list(Parsed)),
    ?assertEqual(2, length(Parsed)).

%%====================================================================
%% modulate_state/3 — decode_json_body on POST with body
%%====================================================================

modulate_state_decode_json_post_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
         Req1 = nova_test_helper:with_json_body(#{<<"name">> => <<"test">>}, Req),
         {ok, Req2, state} = nova_request_plugin:modulate_state(
             Req1, [{decode_json_body, true}], state),
         JSON = maps:get(json, Req2),
         ?assertEqual(<<"test">>, maps:get(<<"name">>, JSON))
     end}.

%%====================================================================
%% modulate_state/3 — decode_json_body on POST with empty body -> stop
%%====================================================================

modulate_state_decode_json_empty_body_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
         Req1 = nova_test_helper:with_content_type(<<"application/json">>, Req),
         Req2 = Req1#{body => <<>>},
         {stop, _, state} = nova_request_plugin:modulate_state(
             Req2, [{decode_json_body, true}], state)
     end}.

%%====================================================================
%% plugin_info/0
%%====================================================================

plugin_info_test() ->
    Info = nova_request_plugin:plugin_info(),
    ?assertEqual(<<"Nova body plugin">>, maps:get(title, Info)),
    ?assert(is_list(maps:get(options, Info))).
