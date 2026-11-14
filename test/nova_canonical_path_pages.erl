%%% The controllers of nova_canonical_path_SUITE.
-module(nova_canonical_path_pages).

-export([page/1, canonical/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

canonical(#{seen_by_plugin := Path}) ->
    {status, 200, #{}, Path}.
