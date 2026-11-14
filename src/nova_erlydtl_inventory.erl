-module(nova_erlydtl_inventory).
-moduledoc false.
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
    case [Prefix || {Name, Prefix} <- nova:get_apps(), atom_to_binary(Name) =:= App] of
        [] ->
            ?LOG_WARNING(#{msg => <<"Template could not find application">>, application => App}),
            <<"#">>;
        [Prefix | _] ->
            PrefixBin = list_to_binary(Prefix),
            maybe_remove_starting_slash(<< PrefixBin/binary, Url/binary >>)
    end.

maybe_remove_starting_slash(<<"/", "/", Rest/binary>>) ->
    <<"/", Rest/binary>>;
maybe_remove_starting_slash(Url) ->
    Url.
