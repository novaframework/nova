%%% The controllers of nova_listeners_SUITE.
-module(nova_listeners_pages).

-export([where/1]).

where(Req) ->
    {status, 200, #{}, iolist_to_binary(io_lib:format("~p ~p", [nova_req:listener(Req), nova_req:http_version(Req)]))}.
