%%%-------------------------------------------------------------------
%% @doc
%% Nova application behaviour callback (Not used)
%% @end
%%%-------------------------------------------------------------------

-module(nova_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    nova_sup:start_link().

prep_stop(State) ->
    nova:graceful_shutdown(),
    State.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
