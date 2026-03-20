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
    ?LOG_NOTICE(#{msg => <<"Suspending listener">>}),
    ranch:suspend_listener(nova_listener),
    DrainTimeout = application:get_env(nova, shutdown_drain_timeout, 15000),
    ?LOG_NOTICE(#{msg => <<"Draining connections">>, timeout_ms => DrainTimeout}),
    drain_connections(DrainTimeout),
    ?LOG_NOTICE(#{msg => <<"Stopping listener">>}),
    cowboy:stop_listener(nova_listener),
    ok.

drain_connections(Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    drain_loop(Deadline).

drain_loop(Deadline) ->
    case ranch:info(nova_listener) of
        #{active_connections := 0} ->
            ok;
        #{active_connections := N} ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true ->
                    ?LOG_WARNING(#{msg => <<"Drain timeout reached">>,
                                   remaining_connections => N}),
                    ok;
                false ->
                    timer:sleep(500),
                    drain_loop(Deadline)
            end
    end.
