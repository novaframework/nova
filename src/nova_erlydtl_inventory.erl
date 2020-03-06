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

url(Variables, _Options) ->
    Url = hd(Variables),
    App = proplists:get_value(application, Variables),
    case nova_router:get_app(binary_to_atom(App, utf8)) of
        {error, _} ->
            ?WARNING("Could not find application: ~p. Called from erlydtl-tag 'url'.", [App]),
            <<"#">>;
        {ok, #{prefix := Prefix}} ->
            PrefixBin = list_to_binary(Prefix),
            << PrefixBin/binary, Url/binary >>
    end.
