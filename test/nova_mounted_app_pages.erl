%%% The controllers of nova_mounted_app_SUITE.
-module(nova_mounted_app_pages).

-export([page/1, require_token/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

require_token(Req) ->
    nova_req:header(~"authorization", Req) =:= ~"let-me-in".
