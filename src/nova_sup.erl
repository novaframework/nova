%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Nova supervisor
%%% @end

-module(nova_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("nova/include/nova.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% This is a bit ugly, but we need to do this anyhow(?)
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Configuration = application:get_env(nova, cowboy_configuration, #{}),

    setup_cowboy(Configuration),

    SessionManager = application:get_env(nova, session_manager, nova_session_ets),

    BootstrapApp = application:get_env(nova, bootstrap_application, undefined),

    Children = [
                child(nova_router, nova_router, [BootstrapApp]),
                child(nova_handlers, nova_handlers),
                child(nova_plugin, nova_plugin),
                child(SessionManager, SessionManager)
               ],

    case application:get_env(nova, dev_mode, false) of
        false ->
            ?INFO("Starting nova in production mode...");
        true ->
            ?INFO("Starting nova in developer mode...")
    end,
    {ok, {SupFlags, Children}}.




%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Id, Mod, Args) ->
    #{id => Id,
      start => {Mod, start_link, Args},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Mod]}.

child(Id, Mod) ->
    child(Id, Mod, []).

setup_cowboy(Configuration) ->
    case start_cowboy(Configuration) of
        {ok, _} ->
            ok;
        {error, Error} ->
            ?WARNING("Cowboy could not start reason: ~p", [Error])
    end.

start_cowboy(Configuration) ->
    ?INFO("Nova is starting cowboy..."),
    case maps:get(use_ssl, Configuration, false) of
        false ->
            cowboy:start_clear(
              nova_listener,
              [{port, maps:get(port, Configuration, 8080)}],
              #{middlewares => [cowboy_router, nova_security_handler, cowboy_handler],
                stream_handlers => [nova_stream_h, cowboy_compress_h, cowboy_stream_h],
                compress => true});
        _ ->
            CACert = maps:get(ca_cert, Configuration),
            Cert = maps:get(cert, Configuration),
            Port = maps:get(ssl_port, Configuration, 8443),
            ?INFO("Nova is starting SSL on port ~p", [Port]),
            cowboy:start_tls(
              nova_listener, [
                              {port, Port},
                              {certfile, Cert},
                              {cacertfile, CACert}
                             ],
              #{middlewares => [cowboy_router, nova_security_handler, cowboy_handler]})
    end.
