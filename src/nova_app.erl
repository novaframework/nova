%%%-------------------------------------------------------------------
-module(nova_app).
-moduledoc false.

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    note_unset_environment(),
    %% The plugins the router lists are those of the routes compiled from
    %% here on. The list is stored outside the supervision tree and would
    %% otherwise still hold those of an earlier run in this VM.
    nova_router:forget_plugins(),
    nova_sup:start_link().

%% The application of the server package stops before this one and takes
%% its listeners down. Those that are still there, with that application
%% left running, are taken down here.
prep_stop(State) ->
    ok = nova_sup:shutdown_listeners(),
    State.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% An unset environment is prod. A developer who left the key out expects
%% dev, and finds the routes of routes(dev) missing.
note_unset_environment() ->
    case application:get_env(nova, environment) of
        undefined ->
            ?LOG_NOTICE(#{msg => ~"The environment key of the nova application is not set and is read as prod: routers are asked for routes(prod), and nothing meant for development is on. Set {nova, [{environment, dev}]} for development",
                          key => environment,
                          read_as => nova:get_environment()});
        {ok, _Environment} ->
            ok
    end.
