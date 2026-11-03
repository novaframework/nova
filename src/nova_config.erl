-module(nova_config).
-moduledoc false.

-export([get_env/2]).

-doc """
Reads a setting that may live in the bootstrap application or in `nova`.
The bootstrap application's environment wins, then `nova`'s, then `Default`.
""".
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case bootstrap_env(Key) of
        {ok, Value} -> Value;
        undefined -> application:get_env(nova, Key, Default)
    end.

bootstrap_env(Key) ->
    case application:get_env(nova, bootstrap_application) of
        {ok, App} when is_atom(App) -> application:get_env(App, Key);
        _ -> undefined
    end.
