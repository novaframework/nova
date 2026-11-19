-module(nova_once).
-moduledoc false.

-export([first/1, per_interval/2, forget/1]).

-doc """
Whether this is the first call with `Key` since the node started. A caller
logs when it is, so that what every request can cause is logged once. Two
processes that ask at the same moment may both be told `true`.
""".
-spec first(Key :: term()) -> boolean().
first(Key) ->
    case persistent_term:get({?MODULE, Key}, false) of
        true ->
            false;
        false ->
            persistent_term:put({?MODULE, Key}, true),
            true
    end.

-doc """
Whether a caller may log about `Key` now: `{true, Skipped}` for the first
call and then once every `Interval` milliseconds on the node, `false` in
between. `Skipped` is the number of calls that were told `false` since the
last `true`. For what a request can cause over and over, where the first
occurrence is not the only one worth a line.
""".
-spec per_interval(Key :: term(), Interval :: pos_integer()) -> {true, Skipped :: non_neg_integer()} | false.
per_interval(Key, Interval) ->
    Now = erlang:monotonic_time(millisecond),
    Cell = cell(Key, Now - Interval),
    Last = atomics:get(Cell, 1),
    case Now - Last >= Interval andalso atomics:compare_exchange(Cell, 1, Last, Now) =:= ok of
        true ->
            {true, atomics:exchange(Cell, 2, 0)};
        false ->
            atomics:add(Cell, 2, 1),
            false
    end.

-doc """
Forgets every call with `Key`, so that the next one is the first again. For
the tests of what is logged once.
""".
-spec forget(Key :: term()) -> ok.
forget(Key) ->
    _ = persistent_term:erase({?MODULE, Key}),
    _ = persistent_term:erase({?MODULE, interval, Key}),
    ok.

-spec cell(term(), integer()) -> atomics:atomics_ref().
cell(Key, Last) ->
    cell(Key, Last, persistent_term:get({?MODULE, interval, Key}, undefined)).

-spec cell(term(), integer(), dynamic()) -> atomics:atomics_ref().
cell(Key, Last, undefined) ->
    Cell = atomics:new(2, [{signed, true}]),
    atomics:put(Cell, 1, Last),
    persistent_term:put({?MODULE, interval, Key}, Cell),
    Cell;
cell(_Key, _Last, Cell) ->
    Cell.
