%%% The router of nova_test_deep_app, an application that only exists as
%%% an entry in the nova_apps of nova_test_nested_app.
-module(nova_test_deep_router).
-behaviour(nova_router).

-export([routes/1, set_routes/1]).

routes(_Environment) ->
    persistent_term:get(?MODULE, []).

set_routes(Routes) ->
    persistent_term:put(?MODULE, Routes).
