%%% The controllers of nova_route_methods_SUITE.
-module(nova_route_methods_pages).

-export([page/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.
