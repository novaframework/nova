%%%-------------------------------------------------------------------
%% @doc
%% Nova application behaviour callback (Not used)
%% @end
%%%-------------------------------------------------------------------

-module(nova_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    nova_sup:start_link().

prep_stop(State) ->
    graceful_shutdown(),
    State.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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
