-module(nova_handler_test_controller).
-fallback_controller(nova_handler_test_fallback).

-export([
         atom_return/1,
         crash/1,
         terminate/3
        ]).

atom_return(_Req) ->
    not_a_tuple.

crash(_Req) ->
    error(boom).

terminate(Reason, _Req, State) ->
    self() ! {terminated, Reason, State},
    ok.
