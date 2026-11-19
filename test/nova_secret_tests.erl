-module(nova_secret_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").
-define(OTHER_SECRET, ~"another secret of thirty-two bytes or more, for them").

%% The first block of HKDF-Expand with SHA-256: test case 1 of RFC 5869,
%% appendix A.1, whose output starts with it.
derive_is_the_first_block_of_hkdf_expand_test() ->
    Key = binary:decode_hex(~"077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"),
    Info = binary:decode_hex(~"f0f1f2f3f4f5f6f7f8f9"),
    ?assertEqual(binary:decode_hex(~"3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf"),
                 nova_secret:derive(Key, Info)).

%% A node that is given the secret makes the keys every other node makes.
derived_keys_are_the_same_wherever_they_are_made_test() ->
    ?assertEqual(nova_secret:derived(?SECRET), nova_secret:derived(?SECRET)),
    ?assertEqual(crypto:mac(hmac, sha256, ?SECRET, [~"nova csrf token", 1]),
                 maps:get(csrf_token, nova_secret:derived(?SECRET))).

every_purpose_has_a_key_of_its_own_test() ->
    Keys = maps:values(nova_secret:derived(?SECRET)),
    ?assertEqual(length(Keys), length(lists:usort(Keys))),
    [?assertEqual(32, byte_size(Key)) || Key <- Keys],
    ?assertNot(lists:member(?SECRET, Keys)),
    ?assertEqual([], [Purpose || Purpose := Key <- nova_secret:derived(?SECRET),
                                 Key =:= maps:get(Purpose, nova_secret:derived(?OTHER_SECRET))]).

keys_follow_the_secrets_that_are_set_test() ->
    ?assertEqual([], nova_secret:keys(csrf_token)),
    ok = nova_test_settings:set(secret_key_base, [?OTHER_SECRET, ?SECRET]),
    try
        ?assertEqual([maps:get(csrf_token, nova_secret:derived(?OTHER_SECRET)),
                      maps:get(csrf_token, nova_secret:derived(?SECRET))],
                     nova_secret:keys(csrf_token))
    after
        nova_test_settings:unset(secret_key_base)
    end.
