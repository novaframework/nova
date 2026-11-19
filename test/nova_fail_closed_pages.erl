%%% The controllers of nova_fail_closed_SUITE.
-module(nova_fail_closed_pages).

-export([unexpected/1, page/1, crash/1, deny/1]).

unexpected(_Req) ->
    {ok, allowed}.

page(_Req) ->
    {status, 200, #{}, ~"page"}.

crash(_Req) ->
    error(security_callback_crashed).

deny(_Req) ->
    false.
