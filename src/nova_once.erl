-module(nova_once).
-moduledoc false.

-export([first/1]).

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
