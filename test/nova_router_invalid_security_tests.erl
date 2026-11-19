-module(nova_router_invalid_security_tests).

-include_lib("eunit/include/eunit.hrl").

refused(Value) ->
    ?assertThrow({error, {invalid_security, Value}}, nova_router:normalize_secure(Value, "a_router.erl")).

invalid_security_value_is_refused_test_() ->
    [{"true is not a callback and must not leave the routes open", fun() -> refused(true) end},
     {"an atom is not a callback", fun() -> refused(not_a_callback) end},
     {"a string is not a callback", fun() -> refused("my_auth:check") end},
     {"a binary is not a callback", fun() -> refused(~"my_auth:check") end},
     {"a fun of the wrong arity is not a callback", fun() -> refused(fun() -> true end) end},
     {"a module and function have to be atoms", fun() -> refused({my_auth, "check"}) end},
     {"a module, function and arity is not a callback", fun() -> refused({my_auth, check, 1}) end}].

valid_security_value_is_kept_test_() ->
    Callback = fun(_Req) -> true end,
    [?_assertEqual(false, nova_router:normalize_secure(false, "a_router.erl")),
     ?_assertEqual(Callback, nova_router:normalize_secure(Callback, "a_router.erl")),
     ?_assertEqual(fun lists:reverse/1, nova_router:normalize_secure({lists, reverse}, "a_router.erl"))].
