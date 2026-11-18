%%%-------------------------------------------------------------------
-module(nova_app).
-moduledoc false.

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).
-export([note_no_listener/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    note_unset_environment(),
    note_missing_server(),
    %% The plugins the router lists are those of the routes compiled from
    %% here on. The list is stored outside the supervision tree and would
    %% otherwise still hold those of an earlier run in this VM.
    nova_router:forget_plugins(),
    case nova_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid, no_listener_timer(nova_settings:value(server_grace))};
        NotStarted ->
            NotStarted
    end.

%% The application of the server package stops before this one and takes
%% its listeners down. Those that are still there, with that application
%% left running, are taken down here.
prep_stop(State) ->
    ok = nova_sup:shutdown_listeners(),
    State.

stop(undefined) ->
    ok;
stop(Timer) ->
    _ = timer:cancel(Timer),
    ok.

%% A node that serves nothing on purpose sets server_grace to infinity.
no_listener_timer(infinity) ->
    undefined;
no_listener_timer(Grace) ->
    {ok, Timer} = timer:apply_after(Grace, ?MODULE, note_no_listener, []),
    Timer.

-doc false.
%% A server package that is a dependency, and in the applications of
%% nothing, is on the code path and is never started. Nova cannot tell as
%% it starts, since the package starts after it, so it looks once more.
-spec note_no_listener() -> ok.
note_no_listener() ->
    case {whereis(nova_sup), nova_sup:listeners()} of
        {Sup, []} when is_pid(Sup) ->
            ?LOG_WARNING(#{msg => ~"Nova is started and no server package has started a listener, so nothing is served. Add the server package to the applications of your application, after nova. A node that starts slowly, or serves nothing on purpose, sets server_grace",
                           server => nova_server:module(), setting => server_grace});
        _StoppedOrListening ->
            ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Nova serves through a server package. Before 1.0 Cowboy came with Nova,
%% so an application that moves up without adding one finds nothing listening.
note_missing_server() ->
    Server = nova_server:module(),
    case code:ensure_loaded(Server) of
        {module, Server} ->
            ok;
        {error, _} ->
            ?LOG_WARNING(#{msg => ~"No server package is in the release, so Nova serves nothing. Add nova_cowboy or nova_roadrunner to the dependencies and to the applications of your application",
                           server => Server})
    end.

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
