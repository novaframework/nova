-module(nova_handler_test_fallback).

-export([resolve/2]).

resolve(_Req, not_a_tuple) ->
    {status, 409}.
