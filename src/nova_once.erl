-module(nova_once).
-moduledoc false.

-export([first/1, forget/1]).

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
Forgets every call with `Key`, so that the next one is the first again. For
the tests of what is logged once.
""".
-spec forget(Key :: term()) -> ok.
forget(Key) ->
    _ = persistent_term:erase({?MODULE, Key}),
    ok.
