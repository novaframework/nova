%%%-------------------------------------------------------------------
-module(nova_app).
-moduledoc "Nova application behaviour callback".

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    warn_about_unset_environment(),
    %% The plugins the router lists are those of the routes compiled from
    %% here on. The list is stored outside the supervision tree and would
    %% otherwise still hold those of an earlier run in this VM.
    nova_router:forget_plugins(),
    nova_sup:start_link().

prep_stop(State) ->
    graceful_shutdown(),
    State.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% An unset environment is dev to nova:get_environment/0, so a release that
%% left the key out serves what its routers return for dev.
warn_about_unset_environment() ->
    case application:get_env(nova, environment) of
        undefined ->
            ?LOG_WARNING(#{msg => ~"The nova environment is not set and is read as dev: every router is asked for routes(dev), so routes meant for development are served, and watcher output is printed. Set {nova, [{environment, prod}]} in a release",
                           environment => undefined,
                           read_as => dev});
        {ok, _Environment} ->
            ok
    end.

graceful_shutdown() ->
    Delay = application:get_env(nova, shutdown_delay, 0),
    case Delay > 0 of
        true ->
            ?LOG_NOTICE(#{msg => <<"Graceful shutdown started">>, delay_ms => Delay}),
            timer:sleep(Delay);
        false ->
            ok
    end,
    Listeners = nova_sup:listeners(),
    ?LOG_NOTICE(#{msg => <<"Suspending listeners">>, listeners => Listeners}),
    [suspend(Listener) || Listener <- Listeners],
    DrainTimeout = application:get_env(nova, shutdown_drain_timeout, 15000),
    ?LOG_NOTICE(#{msg => <<"Draining connections">>, timeout_ms => DrainTimeout}),
    drain_connections(Listeners, DrainTimeout),
    ?LOG_NOTICE(#{msg => <<"Stopping listeners">>}),
    [cowboy:stop_listener(Listener) || Listener <- Listeners],
    ok.

%% A listener can already be gone if the application was removed at runtime,
%% so neither suspending nor inspecting it may bring the shutdown down.
suspend(Listener) ->
    try ranch:suspend_listener(Listener) of
        _Result -> ok
    catch
        _Class:_Reason -> ok
    end.

active_connections(Listener) ->
    try ranch:info(Listener) of
        #{active_connections := N} -> N
    catch
        _Class:_Reason -> 0
    end.

drain_connections(Listeners, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    drain_loop(Listeners, Deadline).

drain_loop(Listeners, Deadline) ->
    case lists:sum([active_connections(Listener) || Listener <- Listeners]) of
        0 ->
            ok;
        N ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true ->
                    ?LOG_WARNING(#{msg => <<"Drain timeout reached">>,
                                   remaining_connections => N}),
                    ok;
                false ->
                    timer:sleep(500),
                    drain_loop(Listeners, Deadline)
            end
    end.
