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
        nova_once:forget(Key),
        nova_once:forget(Other)
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
        nova_once:forget(Key),
        nova_once:forget(Other)
    end.

%% A key is free to be asked about both ways.
first_and_per_interval_keep_apart_what_they_store_test() ->
    Key = {?MODULE, make_ref()},
    try
        ?assert(nova_once:first(Key)),
        ?assertEqual({true, 0}, nova_once:per_interval(Key, 60000)),
        ?assertNot(nova_once:first(Key)),
        ?assertEqual(false, nova_once:per_interval(Key, 60000))
    after
        nova_once:forget(Key)
    end.

forgotten_key_is_first_again_test() ->
    Key = {?MODULE, make_ref()},
    ?assert(nova_once:first(Key)),
    ?assertEqual({true, 0}, nova_once:per_interval(Key, 60000)),
    ok = nova_once:forget(Key),
    ?assert(nova_once:first(Key)),
    ?assertEqual({true, 0}, nova_once:per_interval(Key, 60000)),
    ok = nova_once:forget(Key).
