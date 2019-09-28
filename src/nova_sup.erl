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
    application:ensure_all_started(ranch),
    case application:get_env(use_ssl) of
        {ok, true} ->
            {ok, Cert} = application:get_env(ssl_certfile),
            {ok, CACert} = application:get_env(ssl_cacertfile),
            ?INFO("Starting Nova with SSL support. Cert: ~s, CACert: ~s", [Cert, CACert]),
            start_cowboy_secure(CACert, Cert);
        _ ->
            start_cowboy()
    end,

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Children = [
                child(nova_router, nova_router),
                child(nova_session, nova_session)
               ],

    Children2 = case application:get_env(rest_only) of
                    {ok, true} ->
                        ?INFO("Starting Nova with rest_only option. Views will not be compiled"),
                        Children;
                    _ ->
                        ?INFO("Starting Nova as normal..."),
                        [child(nova_compiler, nova_compiler)|Children]
                end,
    case application:get_env(dev_mode) of
        {ok, true} ->
            ?INFO("Starting nova in developer mode..."),
            application:ensure_all_started(sync);
        _ ->
            ok
    end,

    {ok, {SupFlags, Children2}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Id, Mod) ->
    #{id => Id,
      start => {Mod, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Mod]}.


start_cowboy() ->
    Port =
        case application:get_env(web_port) of
            undefined ->
                8080;
            {ok, WebPort} ->
                WebPort
        end,
    ?INFO("Nova is running on port ~p", [Port]),
    {ok, _} = cowboy:start_clear(
                nova_listener,
                [{port, Port}],
                #{middlewares => [cowboy_router, nova_security_handler, nova_http_controller]}).

start_cowboy_secure(CACert, Cert) ->
    Port = case application:get_env(ssl_port) of
               undefined -> 8443;
               {ok, SSLPort} -> SSLPort
           end,
    ?INFO("Nova is running SSL on port ~p", [Port]),
    {ok, _} = cowboy:start_tls(nova_listener, [
                                               {port, Port},
                                               {certfile, Cert},
                                               {cacertfile, CACert}
                                              ],
                               #{middlewares => [cowboy_router, nova_security_handler, nova_http_controller]}).
