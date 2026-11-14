%%% The controllers of nova_mounted_precedence_SUITE.
-module(nova_mounted_precedence_pages).

-export([mounted_page/1, host_not_found/1, mounted_not_found/1, host_page/1, deny/1]).

mounted_page(_Req) ->
    {status, 200, #{}, ~"mounted"}.

host_not_found(_Req) ->
    {status, 404, #{}, ~"host 404"}.

mounted_not_found(_Req) ->
    {status, 404, #{}, ~"mounted 404"}.

host_page(_Req) ->
    {status, 200, #{}, ~"host"}.

deny(_Req) ->
    false.
