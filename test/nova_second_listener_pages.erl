%%% The controllers of nova_second_listener_SUITE.
-module(nova_second_listener_pages).

-export([key/1, crash/1, first_error/1, second_not_found/1, missing/1, second_error/1]).

key(Req) ->
    {status, 200, #{}, io_lib:format("~0p", [nova_router:dispatch_key(Req)])}.

crash(_Req) ->
    error(controller_crashed).

first_error(_Req) ->
    {status, 500, #{}, ~"first 500"}.

second_not_found(_Req) ->
    {status, 404, #{}, ~"second 404"}.

missing(_Req) ->
    throw({404, missing}).

second_error(_Req) ->
    {status, 500, #{}, ~"second 500"}.
