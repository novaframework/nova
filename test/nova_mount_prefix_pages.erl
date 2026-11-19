%%% The controllers of nova_mount_prefix_SUITE.
-module(nova_mount_prefix_pages).

-export([page/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.
