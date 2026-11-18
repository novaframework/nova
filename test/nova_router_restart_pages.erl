%%% The controllers of nova_router_restart_SUITE.
-module(nova_router_restart_pages).

-export([require_token/1, page/1]).

require_token(Req) ->
    nova_req:header(~"authorization", Req) =:= ~"let-me-in".

page(_Req) ->
    {status, 200, #{}, ~"page"}.
