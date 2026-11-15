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

%% True for the first call and then once an interval, with the number of
%% calls in between that were told false.
per_interval_test() ->
    Key = {?MODULE, make_ref()},
    Other = {?MODULE, make_ref()},
    try
        ?assertEqual({true, 0}, nova_once:per_interval(Key, 40)),
        ?assertEqual(false, nova_once:per_interval(Key, 40)),
        ?assertEqual(false, nova_once:per_interval(Key, 40)),
        ?assertEqual({true, 0}, nova_once:per_interval(Other, 40)),
        timer:sleep(50),
        ?assertEqual({true, 2}, nova_once:per_interval(Key, 40)),
        ?assertEqual(false, nova_once:per_interval(Key, 40))
    after
        persistent_term:erase(Key),
        persistent_term:erase(Other)
    end.
