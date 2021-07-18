-module(nova_erlydtl_inventory).
-behaviour(erlydtl_library).
-include_lib("nova/include/nova.hrl").

-export([
         version/0,
         inventory/1,
         url/2
        ]).

version() -> 1.

inventory(filters) -> [];
inventory(tags) -> [url].

url([Url|Variables], _Options) ->
    App = proplists:get_value(application, Variables),
    NovaEnv = nova:get_env(apps, []),
    case proplists:get_value(binary_to_atom(App, utf8), NovaEnv) of
        undefined ->
            ?WARNING("Could not find application: ~p. Called from erlydtl-tag 'url'.", [App]),
            <<"#">>;
        #{prefix := Prefix} ->
            PrefixBin = list_to_binary(Prefix),
            << PrefixBin/binary, Url/binary >>
    end.
