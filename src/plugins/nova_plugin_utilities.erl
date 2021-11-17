-module(nova_plugin_utilities).
-export([parse_options/1]).

parse_options(Options) ->
    ListOptions = maps:to_list(Options),
    parse_options(ListOptions, []).

parse_options([], Parsed) -> Parsed;
parse_options([{K, V}|T], Parsed) when V == true ->
    parse_options(T, [K|Parsed]);
parse_options([{K, L}|T], Parsed) when is_list(L) ->
    parse_options(T, [{K, L}|Parsed]).
