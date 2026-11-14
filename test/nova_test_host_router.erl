-module(nova_test_host_router).
-behaviour(nova_router).

-export([routes/1, set_routes/1]).

routes(_Environment) ->
    persistent_term:get(?MODULE, []).

set_routes(Routes) ->
    persistent_term:put(?MODULE, Routes).
