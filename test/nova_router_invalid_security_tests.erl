-module(nova_router_invalid_security_tests).

-include_lib("eunit/include/eunit.hrl").

denies(Value) ->
    Callback = nova_router:normalize_secure(Value, "a_router.erl"),
    ?assert(is_function(Callback, 1)),
    ?assertEqual(false, Callback(#{})).

invalid_security_value_denies_test_() ->
    [{"true is not a callback and must not leave the routes open", fun() -> denies(true) end},
     {"an atom is not a callback", fun() -> denies(not_a_callback) end},
     {"a string is not a callback", fun() -> denies("my_auth:check") end},
     {"a fun of the wrong arity is not a callback", fun() -> denies(fun() -> true end) end}].

valid_security_value_is_kept_test_() ->
    Callback = fun(_Req) -> true end,
    [?_assertEqual(false, nova_router:normalize_secure(false, "a_router.erl")),
     ?_assertEqual(Callback, nova_router:normalize_secure(Callback, "a_router.erl")),
     ?_assertEqual(fun lists:reverse/1, nova_router:normalize_secure({lists, reverse}, "a_router.erl"))].
