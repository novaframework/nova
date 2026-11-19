-module(nova_once_tests).
-include_lib("eunit/include/eunit.hrl").

first_test() ->
    Key = {?MODULE, make_ref()},
    Other = {?MODULE, make_ref()},
    try
        ?assert(nova_once:first(Key)),
        ?assertNot(nova_once:first(Key)),
        ?assert(nova_once:first(Other)),
        ?assertNot(nova_once:first(Key))
    after
        nova_once:forget(Key),
        nova_once:forget(Other)
    end.

forget_test() ->
    Key = {?MODULE, make_ref()},
    ?assert(nova_once:first(Key)),
    ok = nova_once:forget(Key),
    ?assert(nova_once:first(Key)),
    ok = nova_once:forget(Key),
    ok = nova_once:forget(Key).
