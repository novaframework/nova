%%% Changes a setting of the nova environment the way a boot would have
%%% found it: Nova reads those it checks when it starts, and not again.
-module(nova_test_settings).

-export([set/2, unset/1]).

-spec set(nova_settings:key(), term()) -> ok.
set(Key, Value) ->
    application:set_env(nova, Key, Value),
    nova_settings:validate().

-spec unset(nova_settings:key()) -> ok.
unset(Key) ->
    application:unset_env(nova, Key),
    nova_settings:validate().
