-module(nova_csrf_plugin_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Token generation tests
%%====================================================================

generate_token_is_binary_test() ->
    Token = nova_csrf_plugin:generate_token(),
    ?assert(is_binary(Token)).

generate_token_correct_length_test() ->
    %% 32 bytes base64-encoded = 44 characters
    Token = nova_csrf_plugin:generate_token(),
    ?assertEqual(44, byte_size(Token)).

generate_token_unique_test() ->
    Token1 = nova_csrf_plugin:generate_token(),
    Token2 = nova_csrf_plugin:generate_token(),
    ?assertNotEqual(Token1, Token2).

%%====================================================================
%% constant_time_compare tests
%%====================================================================

constant_time_compare_equal_test() ->
    ?assert(nova_csrf_plugin:constant_time_compare(<<"abc">>, <<"abc">>)).

constant_time_compare_different_test() ->
    ?assertNot(nova_csrf_plugin:constant_time_compare(<<"abc">>, <<"def">>)).

constant_time_compare_different_length_test() ->
    ?assertNot(nova_csrf_plugin:constant_time_compare(<<"abc">>, <<"abcd">>)).

constant_time_compare_undefined_test() ->
    ?assertNot(nova_csrf_plugin:constant_time_compare(<<"abc">>, undefined)).

constant_time_compare_both_undefined_test() ->
    ?assertNot(nova_csrf_plugin:constant_time_compare(undefined, undefined)).

%%====================================================================
%% is_safe_method tests
%%====================================================================

is_safe_method_get_test() ->
    ?assert(nova_csrf_plugin:is_safe_method(<<"GET">>)).

is_safe_method_head_test() ->
    ?assert(nova_csrf_plugin:is_safe_method(<<"HEAD">>)).

is_safe_method_options_test() ->
    ?assert(nova_csrf_plugin:is_safe_method(<<"OPTIONS">>)).

is_safe_method_post_test() ->
    ?assertNot(nova_csrf_plugin:is_safe_method(<<"POST">>)).

is_safe_method_put_test() ->
    ?assertNot(nova_csrf_plugin:is_safe_method(<<"PUT">>)).

is_safe_method_patch_test() ->
    ?assertNot(nova_csrf_plugin:is_safe_method(<<"PATCH">>)).

is_safe_method_delete_test() ->
    ?assertNot(nova_csrf_plugin:is_safe_method(<<"DELETE">>)).

%%====================================================================
%% is_excluded_path tests
%%====================================================================

is_excluded_path_match_test() ->
    ?assert(nova_csrf_plugin:is_excluded_path(<<"/api/webhooks/stripe">>, [<<"/api/webhooks">>])).

is_excluded_path_no_match_test() ->
    ?assertNot(nova_csrf_plugin:is_excluded_path(<<"/admin/settings">>, [<<"/api/webhooks">>])).

is_excluded_path_empty_list_test() ->
    ?assertNot(nova_csrf_plugin:is_excluded_path(<<"/anything">>, [])).

is_excluded_path_exact_match_test() ->
    ?assert(nova_csrf_plugin:is_excluded_path(<<"/api">>, [<<"/api">>])).

is_excluded_path_ends_at_a_segment_test() ->
    ?assertNot(nova_csrf_plugin:is_excluded_path(<<"/apikeys">>, [<<"/api">>])),
    ?assertNot(nova_csrf_plugin:is_excluded_path(<<"/api-admin/users">>, [<<"/api">>])),
    ?assert(nova_csrf_plugin:is_excluded_path(<<"/api/keys">>, [<<"/api/">>])).

is_excluded_path_resolves_dot_segments_test() ->
    ?assertNot(nova_csrf_plugin:is_excluded_path(<<"/api/../admin">>, [<<"/api">>])),
    ?assertNot(nova_csrf_plugin:is_excluded_path(<<"/api/x/../../admin">>, [<<"/api">>])),
    ?assert(nova_csrf_plugin:is_excluded_path(<<"//api/./hook">>, [<<"/api">>])),
    ?assert(nova_csrf_plugin:is_excluded_path(<<"/admin/../api/hook">>, [<<"/api">>])).

is_excluded_path_string_prefix_test() ->
    ?assert(nova_csrf_plugin:is_excluded_path(<<"/api/hook">>, ["/api"])).

is_excluded_path_multiple_prefixes_test() ->
    ?assert(nova_csrf_plugin:is_excluded_path(<<"/health">>, [<<"/api">>, <<"/health">>])).

%%====================================================================
%% get_submitted_token tests
%%====================================================================

get_submitted_token_from_header_test() ->
    Req = #{headers => #{<<"x-csrf-token">> => <<"header-token">>}},
    ?assertEqual(<<"header-token">>,
                 nova_csrf_plugin:get_submitted_token(Req, <<"_csrf_token">>, <<"x-csrf-token">>)).

get_submitted_token_from_params_test() ->
    %% No header present — falls through to params
    Req = #{headers => #{}, params => #{<<"_csrf_token">> => <<"form-token">>}},
    ?assertEqual(<<"form-token">>,
                 nova_csrf_plugin:get_submitted_token(Req, <<"_csrf_token">>, <<"x-csrf-token">>)).

get_submitted_token_header_priority_test() ->
    Req = #{headers => #{<<"x-csrf-token">> => <<"header-token">>},
            params => #{<<"_csrf_token">> => <<"form-token">>}},
    ?assertEqual(<<"header-token">>,
                 nova_csrf_plugin:get_submitted_token(Req, <<"_csrf_token">>, <<"x-csrf-token">>)).

get_submitted_token_missing_test() ->
    Req = #{headers => #{}},
    ?assertEqual(undefined,
                 nova_csrf_plugin:get_submitted_token(Req, <<"_csrf_token">>, <<"x-csrf-token">>)).

%%====================================================================
%% pre_request integration tests (using meck)
%%====================================================================

safe_method_with_existing_session_token_test() ->
    setup_meck([nova_session]),
    try
        Token = <<"existing-token">>,
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {ok, Token} end),
        Req = nova_test_helper:mock_req(~"GET", ~"/"),
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{}),
        ?assertEqual(Token, unmasked(maps:get(csrf_token, Req1)))
    after
        cleanup_meck([nova_session])
    end.

safe_method_generates_token_when_missing_test() ->
    setup_meck([nova_session]),
    try
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {error, not_found} end),
        meck:expect(nova_session, set_provisional, fun(_Req, <<"_csrf_token">>, _Token) -> ok end),
        Req = nova_test_helper:mock_req(~"GET", ~"/"),
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{}),
        ?assert(maps:is_key(csrf_token, Req1)),
        ?assert(is_binary(maps:get(csrf_token, Req1)))
    after
        cleanup_meck([nova_session])
    end.

safe_method_no_session_proceeds_without_token_test() ->
    setup_meck([nova_session]),
    try
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {error, not_found} end),
        meck:expect(nova_session, set_provisional, fun(_Req, <<"_csrf_token">>, _Token) -> {error, session_id_not_set} end),
        Req = nova_test_helper:mock_req(~"GET", ~"/"),
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{}),
        ?assertNot(maps:is_key(csrf_token, Req1))
    after
        cleanup_meck([nova_session])
    end.

unsafe_method_valid_token_from_header_test() ->
    setup_meck([nova_session, nova_req]),
    try
        Token = <<"valid-token">>,
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {ok, Token} end),
        meck:expect(nova_req, header, fun(<<"x-csrf-token">>, _Req) -> Token end),
        Req = nova_test_helper:with_header(~"x-csrf-token", Token, nova_test_helper:mock_req(~"POST", ~"/submit")),
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{}),
        ?assertEqual(Token, unmasked(maps:get(csrf_token, Req1)))
    after
        cleanup_meck([nova_req, nova_session])
    end.

unsafe_method_valid_token_from_params_test() ->
    setup_meck([nova_session, nova_req]),
    try
        Token = <<"valid-token">>,
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {ok, Token} end),
        meck:expect(nova_req, header, fun(<<"x-csrf-token">>, _Req) -> undefined end),
        Req = (nova_test_helper:mock_req(~"POST", ~"/submit"))#{params => #{~"_csrf_token" => Token}},
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{}),
        ?assertEqual(Token, unmasked(maps:get(csrf_token, Req1)))
    after
        cleanup_meck([nova_req, nova_session])
    end.

unsafe_method_invalid_token_test() ->
    setup_meck([nova_session, nova_req]),
    try
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {ok, <<"real-token0">>} end),
        meck:expect(nova_req, header, fun(<<"x-csrf-token">>, _Req) -> <<"wrong-token">> end),
        Req = nova_test_helper:with_header(~"x-csrf-token", ~"wrong-token",
                                           nova_test_helper:mock_req(~"POST", ~"/submit")),
        {stop, {reply, 403, _, _}, _Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{})
    after
        cleanup_meck([nova_req, nova_session])
    end.

unsafe_method_no_session_test() ->
    setup_meck([nova_session]),
    try
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {error, not_found} end),
        Req = nova_test_helper:mock_req(~"POST", ~"/submit"),
        {stop, {reply, 403, _, _}, _Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{})
    after
        cleanup_meck([nova_session])
    end.

unsafe_method_missing_submitted_token_test() ->
    setup_meck([nova_session, nova_req]),
    try
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {ok, <<"real-token">>} end),
        meck:expect(nova_req, header, fun(<<"x-csrf-token">>, _Req) -> undefined end),
        Req = nova_test_helper:mock_req(~"DELETE", ~"/resource/1"),
        {stop, {reply, 403, _, _}, _Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, #{}, #{})
    after
        cleanup_meck([nova_req, nova_session])
    end.

excluded_path_skips_validation_test() ->
    setup_meck([nova_session]),
    try
        meck:expect(nova_session, get, fun(_Req, <<"_csrf_token">>) -> {ok, <<"token">>} end),
        Req = nova_test_helper:mock_req(~"POST", ~"/api/webhooks/stripe"),
        Options = #{excluded_paths => [<<"/api/webhooks">>]},
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, Options, #{}),
        ?assertEqual(~"token", unmasked(maps:get(csrf_token, Req1)))
    after
        cleanup_meck([nova_session])
    end.

custom_field_name_test() ->
    setup_meck([nova_session, nova_req]),
    try
        Token = <<"custom-token">>,
        meck:expect(nova_session, get, fun(_Req, <<"csrf">>) -> {ok, Token} end),
        meck:expect(nova_req, header, fun(<<"x-my-csrf">>, _Req) -> undefined end),
        Req = (nova_test_helper:mock_req(~"POST", ~"/submit"))#{params => #{~"my_csrf" => Token}},
        Options = #{field_name => <<"my_csrf">>,
                    header_name => <<"x-my-csrf">>,
                    session_key => <<"csrf">>},
        {ok, Req1, _State} = nova_csrf_plugin:pre_request(Req, #{}, Options, #{}),
        ?assertEqual(Token, unmasked(maps:get(csrf_token, Req1)))
    after
        cleanup_meck([nova_req, nova_session])
    end.

%%====================================================================
%% Helpers
%%====================================================================

setup_meck(Modules) ->
    [meck:new(M, [passthrough]) || M <- Modules].

cleanup_meck(Modules) ->
    [meck:unload(M) || M <- Modules].

unmasked(Masked) ->
    Decoded = base64:decode(Masked, #{mode => urlsafe, padding => false}),
    Size = byte_size(Decoded) div 2,
    <<Mask:Size/binary, Token:Size/binary>> = Decoded,
    crypto:exor(Mask, Token).

masked_token_is_accepted_test() ->
    setup_meck([nova_session, nova_req]),
    try
        Token = ~"the-token-of-the-session",
        meck:expect(nova_session, get, fun(_Req, ~"_csrf_token") -> {ok, Token} end),
        Get = nova_test_helper:mock_req(~"GET", ~"/"),
        {ok, #{csrf_token := <<Masked/binary>>}, _} = nova_csrf_plugin:pre_request(Get, #{}, #{}, #{}),
        {ok, #{csrf_token := <<Again/binary>>}, _} = nova_csrf_plugin:pre_request(Get, #{}, #{}, #{}),
        ?assertNotEqual(Masked, Again),
        ?assertEqual(nomatch, binary:match(Masked, Token)),
        Post = nova_test_helper:mock_req(~"POST", ~"/submit"),
        [begin
             meck:expect(nova_req, header, fun(~"x-csrf-token", _Req) -> Submitted end),
             ?assertMatch({Submitted, {ok, _, _}}, {Submitted, nova_csrf_plugin:pre_request(Post, #{}, #{}, #{})})
         end || Submitted <- [Masked, Again, Token]],
        [begin
             meck:expect(nova_req, header, fun(~"x-csrf-token", _Req) -> Submitted end),
             ?assertMatch({Submitted, {stop, {reply, 403, _, _}, _, _}},
                          {Submitted, nova_csrf_plugin:pre_request(Post, #{}, #{}, #{})})
         end || Submitted <- [binary:part(Masked, 0, byte_size(Masked) - 1), <<Masked/binary, "A">>,
                              base64:encode(binary:copy(<<0>>, 2 * byte_size(Token)),
                                            #{mode => urlsafe, padding => false}),
                              ~"", undefined]]
    after
        cleanup_meck([nova_req, nova_session])
    end.

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").

%% base64 skips whitespace, so 64 characters can hold fewer bytes than a
%% nonce and its signature are.
signed_token_that_decodes_short_is_refused_test_() ->
    {setup,
     fun() ->
             application:unset_env(nova, environment),
             nova_test_settings:set(secret_key_base, ?SECRET)
     end,
     fun(_) -> nova_test_settings:unset(secret_key_base) end,
     [{Name, ?_assertMatch({stop, {reply, 403, _, _}, _, _}, submitted_with(Token))}
      || {Name, Token} <- [{"spaces inside", <<(binary:copy(~"A", 10))/binary, (binary:copy(~" ", 44))/binary,
                                              (binary:copy(~"A", 10))/binary>>},
                           {"three bytes", <<"AA", (binary:copy(~" ", 60))/binary, "AA">>},
                           {"newlines inside", <<"AA", (binary:copy(~"\n", 60))/binary, "AA">>},
                           {"nothing but spaces", binary:copy(~" ", 64)},
                           {"nothing but tabs", binary:copy(~"\t", 64)},
                           {"a nonce and a signature that is too short",
                            <<(binary:copy(~"A", 60))/binary, "    ">>},
                           {"not base64", binary:copy(~"!", 64)}]]}.

submitted_with(Token) ->
    Req = nova_test_helper:with_header(~"cookie", ~"__Host-session_id=the-id-of-a-client",
                                       nova_test_helper:with_header(~"x-csrf-token", Token,
                                                                    nova_test_helper:mock_req(~"POST", ~"/submit"))),
    try nova_csrf_plugin:pre_request(Req, #{}, #{}, #{})
    catch Class:Reason -> {raised, Class, Reason}
    end.
