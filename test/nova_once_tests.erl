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

%% True for the first call and then once an interval, with the number of
%% calls in between that were told false.
per_interval_test() ->
    Key = {?MODULE, make_ref()},
    Other = {?MODULE, make_ref()},
    Interval = 60000,
    try
        ?assertEqual({true, 0}, nova_once:per_interval(Key, Interval)),
        ?assertEqual(false, nova_once:per_interval(Key, Interval)),
        ?assertEqual(false, nova_once:per_interval(Key, Interval)),
        ?assertEqual({true, 0}, nova_once:per_interval(Other, Interval)),
        ok = interval_passes(Key, Interval),
        ?assertEqual({true, 2}, nova_once:per_interval(Key, Interval)),
        ?assertEqual(false, nova_once:per_interval(Key, Interval))
    after
        nova_once:forget(Key),
        nova_once:forget(Other)
    end.

%% Puts the last true for Key an interval back, which is what waiting for
%% one does, without a sleep that a busy machine makes longer than it says.
interval_passes(Key, Interval) ->
    atomics:sub(persistent_term:get({nova_once, interval, Key}), 1, Interval).

%% A key is free to be asked about both ways.
apart_test() ->
    Key = {?MODULE, make_ref()},
    try
        ?assert(nova_once:first(Key)),
        ?assertEqual({true, 0}, nova_once:per_interval(Key, 60000)),
        ?assertNot(nova_once:first(Key)),
        ?assertEqual(false, nova_once:per_interval(Key, 60000))
    after
        nova_once:forget(Key)
    end.

forget_test() ->
    Key = {?MODULE, make_ref()},
    ?assert(nova_once:first(Key)),
    ?assertEqual({true, 0}, nova_once:per_interval(Key, 60000)),
    ok = nova_once:forget(Key),
    ?assert(nova_once:first(Key)),
    ?assertEqual({true, 0}, nova_once:per_interval(Key, 60000)),
    ok = nova_once:forget(Key),
    ok = nova_once:forget(Key).
