-module(nova_erlydtl_inventory).
-behaviour(erlydtl_library).

-include_lib("kernel/include/logger.hrl").

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
    Apps = nova:get_apps(),
    case proplists:get_value(binary_to_atom(App, utf8), Apps) of
        undefined ->
            ?LOG_WARNING(#{msg => <<"Template could not find application">>, application => App}),
            <<"#">>;
        Prefix ->
            PrefixBin = list_to_binary(Prefix),
            maybe_remove_starting_slash(<< PrefixBin/binary, Url/binary >>)
    end.

maybe_remove_starting_slash(<<"/", "/", Rest/binary>>) ->
    <<"/", Rest/binary>>;
maybe_remove_starting_slash(Url) ->
    Url.
