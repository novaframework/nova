-module(nova_once_tests).
-include_lib("eunit/include/eunit.hrl").

first_is_true_once_for_a_key_test() ->
    Key = {?MODULE, make_ref()},
    Other = {?MODULE, make_ref()},
    try
        ?assert(nova_once:first(Key)),
        ?assertNot(nova_once:first(Key)),
        ?assert(nova_once:first(Other)),
        ?assertNot(nova_once:first(Key))
    after
        persistent_term:erase(Key),
        persistent_term:erase(Other)
    end.
