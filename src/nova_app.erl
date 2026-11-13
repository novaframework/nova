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
    ByServer = nova_sup:listeners_by_server(),
    DrainTimeout = application:get_env(nova, shutdown_drain_timeout, 15000),
    ?LOG_NOTICE(#{msg => <<"Draining listeners">>, listeners => nova_sup:listeners(),
                  timeout_ms => DrainTimeout}),
    maps:foreach(fun(Server, Refs) -> ok = Server:drain_listeners(Refs, DrainTimeout) end, ByServer),
    ?LOG_NOTICE(#{msg => <<"Stopping listeners">>}),
    maps:foreach(fun(Server, Refs) -> [Server:stop_listener(Ref) || Ref <- Refs] end, ByServer),
    ok.
