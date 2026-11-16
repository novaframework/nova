-module(nova_json_tests).
-include_lib("eunit/include/eunit.hrl").

json_lib_test_() ->
    {foreach,
     fun() ->
             Prev = nova_test_helper:setup_nova_env(),
             application:unset_env(nova, json_lib),
             Prev
     end,
     fun(Prev) ->
             application:unset_env(nova, json_lib),
             nova_test_helper:cleanup_nova_env(Prev)
     end,
     [
      fun defaults_to_otp_json/0,
      fun default_encodes_a_proplist_as_an_object/0,
      fun default_leaves_other_lists_as_arrays/0,
      fun default_refuses_what_json_cannot_represent/0,
      fun otp_json_decodes/0,
      fun otp_json_rejects_bad_input/0,
      fun otp_json_encodes/0,
      fun thoas_still_works/0,
      fun read_from_bootstrap_app/0
     ]}.

%% thoas reports an empty body as unexpected_end_of_input.
defaults_to_otp_json() ->
    ?assertEqual({ok, #{~"a" => 1}}, nova_json:decode(~"{\"a\":1}")),
    ?assertEqual({error, unexpected_end}, nova_json:decode(~"")),
    ?assertEqual(~"{\"a\":1}", iolist_to_binary(nova_json:encode(#{a => 1}))).

%% thoas encoded a list of pairs as an object and controllers rely on it.
default_encodes_a_proplist_as_an_object() ->
    ?assertEqual(~"{\"a\":1,\"b\":\"c\"}", iolist_to_binary(nova_json:encode([{a, 1}, {~"b", ~"c"}]))),
    ?assertEqual(
        ~"{\"outer\":{\"inner\":[{\"k\":true}]}}",
        iolist_to_binary(nova_json:encode(#{outer => [{inner, [[{k, true}]]}]}))
    ).

default_leaves_other_lists_as_arrays() ->
    ?assertEqual(~"[]", iolist_to_binary(nova_json:encode([]))),
    ?assertEqual(~"[1,\"a\",null]", iolist_to_binary(nova_json:encode([1, ~"a", null]))).

default_refuses_what_json_cannot_represent() ->
    ?assertError({unsupported_type, {a, b, c}}, nova_json:encode(#{k => {a, b, c}})),
    ?assertError({unsupported_type, 2}, nova_json:encode([{a, 1}, 2])).

otp_json_decodes() ->
    application:set_env(nova, json_lib, json),
    ?assertEqual({ok, #{~"a" => [1, true, null]}}, nova_json:decode(~"{\"a\":[1,true,null]}")).

otp_json_rejects_bad_input() ->
    application:set_env(nova, json_lib, json),
    ?assertMatch({error, _}, nova_json:decode(~"{not json")),
    ?assertMatch({error, _}, nova_json:decode(~"")).

otp_json_encodes() ->
    application:set_env(nova, json_lib, json),
    ?assertEqual(~"{\"a\":1}", iolist_to_binary(nova_json:encode(#{a => 1}))).

thoas_still_works() ->
    application:set_env(nova, json_lib, thoas),
    ?assertEqual({ok, #{~"a" => 1}}, nova_json:decode(~"{\"a\":1}")),
    ?assertMatch({error, _}, nova_json:decode(~"{not json")).

%% The bootstrap application's env is where the guide puts it, and wins over
%% the nova env.
read_from_bootstrap_app() ->
    meck:new(fake_json_lib, [non_strict]),
    meck:expect(fake_json_lib, decode, fun(_) -> {ok, from_fake} end),
    application:set_env(nova, bootstrap_application, nova_json_tests_app),
    application:set_env(nova_json_tests_app, json_lib, fake_json_lib),
    application:set_env(nova, json_lib, thoas),
    try
        ?assertEqual({ok, from_fake}, nova_json:decode(~"{}")),
        application:unset_env(nova_json_tests_app, json_lib),
        ?assertEqual({ok, #{}}, nova_json:decode(~"{}")),
        application:set_env(nova, json_lib, fake_json_lib),
        ?assertEqual({ok, from_fake}, nova_json:decode(~"{}"))
    after
        application:unset_env(nova_json_tests_app, json_lib),
        meck:unload(fake_json_lib)
    end.
