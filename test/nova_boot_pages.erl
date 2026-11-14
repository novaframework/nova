%%% The controllers of nova_boot_SUITE.
-module(nova_boot_pages).

-export([page/1, allow/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

allow(_Req) ->
    true.
