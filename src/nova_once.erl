-module(nova_once).
-moduledoc false.

-export([first/1, per_interval/2]).

-doc """
Whether this is the first call with `Key` since the node started. A caller
logs when it is, so that what every request can cause is logged once. Two
processes that ask at the same moment may both be told `true`.
""".
-spec first(Key :: term()) -> boolean().
first(Key) ->
    case persistent_term:get(Key, false) of
        true ->
            false;
        false ->
            persistent_term:put(Key, true),
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

%% The time of the last line and the calls skipped since. Two processes that
%% come first at the same moment each make a cell, and one of them stays.
cell(Key, Last) ->
    case persistent_term:get(Key, undefined) of
        Cell when is_reference(Cell) ->
            Cell;
        _ ->
            Cell = atomics:new(2, [{signed, true}]),
            atomics:put(Cell, 1, Last),
            persistent_term:put(Key, Cell),
            Cell
    end.
