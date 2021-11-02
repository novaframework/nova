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

-include("include/nova.hrl").
-include("nova.hrl").

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

    case application:get_env(nova, dev_mode, false) of
        false ->
            ?NOTICE("Starting nova in production mode...");
        true ->
            ?NOTICE("Starting nova in developer mode...")
    end,

    Configuration = application:get_env(nova, cowboy_configuration, #{}),

    SessionManager = application:get_env(nova, session_manager, nova_session_ets),

    Children = [
                child(nova_handlers, nova_handlers),
                child(SessionManager, SessionManager),
                child(nova_cache_sup, supervisor, nova_cache_sup),
                child(nova_watcher, nova_watcher)
               ],

    setup_cowboy(Configuration),


    {ok, {SupFlags, Children}}.




%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Id, Type, Mod, Args) ->
    #{id => Id,
      start => {Mod, start_link, Args},
      restart => permanent,
      shutdown => 5000,
      type => Type,
      modules => [Mod]}.

child(Id, Type, Mod) ->
    child(Id, Type, Mod, []).

child(Id, Mod) ->
    child(Id, worker, Mod).

setup_cowboy(Configuration) ->
    case start_cowboy(Configuration) of
        {ok, App, Host, Port} ->
            Host0 = inet:ntoa(Host),
            CowboyVersion = get_version(cowboy),
            NovaVersion = get_version(nova),
            ?NOTICE("Running ~s with nova ~s and cowboy ~s at http://~s:~B",
                    [App, NovaVersion, CowboyVersion, Host0, Port]);
        {error, Error} ->
            ?ERROR("Cowboy could not start reason: ~p", [Error])
    end.

start_cowboy(Configuration) ->
    Middlewares = [
                   nova_router, %% Lookup routes
                   nova_plugin_handler, %% Handle pre-request plugins
                   nova_security_handler, %% Handle security
                   nova_handler, %% Controller
                   nova_plugin_handler %% Handle post-request plugins
                  ],
    StreamHandlers = maps:get(stream_handlers, Configuration, [nova_stream_h, cowboy_compress_h, cowboy_stream_h]),
    Options = maps:get(options, Configuration, #{compress => true}),

    %% Build the options map
    CowboyOptions1 = Options#{middlewares => Middlewares,
                              stream_handlers => StreamHandlers},

    BootstrapApp = application:get_env(nova, bootstrap_application, undefined),
    %% Compile the routes
    Dispatch =
        case BootstrapApp of
            undefined ->
                ?ERROR("You do not have a main nova application defined.
                        Add the following in your sys.config-file:~n{nova,
                        [~n  {bootstrap_application, your_application}~n..."),
                throw({error, no_nova_app_defined});
            App ->
                ExtraApps = application:get_env(App, nova_apps, []),
                nova_router:compile([nova|[App|ExtraApps]])
        end,

    CowboyOptions2 = CowboyOptions1#{env => #{dispatch => Dispatch}},

    Host = maps:get(ip, Configuration, { 0, 0, 0, 0}),

    case maps:get(use_ssl, Configuration, false) of
        false ->
            Port = maps:get(port, Configuration, ?NOVA_STD_PORT),
            case cowboy:start_clear(
                   ?NOVA_LISTENER,
                   [{port, Port},
                    {ip, Host}],
                   CowboyOptions2) of
                {ok, _Pid} ->
                    {ok, BootstrapApp, Host, Port};
                Error ->
                    Error
            end;
        _ ->
            CACert = maps:get(ca_cert, Configuration),
            Cert = maps:get(cert, Configuration),
            Port = maps:get(ssl_port, Configuration, ?NOVA_STD_SSL_PORT),
            ?NOTICE("Nova is starting SSL on port ~p", [Port]),
            case cowboy:start_tls(
              ?NOVA_LISTENER, [
                               {port, Port},
                               {ip, Host},
                               {certfile, Cert},
                               {cacertfile, CACert}
                              ],
              CowboyOptions2) of
                {ok, _Pid} ->
                    {ok, BootstrapApp, Host, Port};
                Error ->
                    Error
            end
    end.


get_version(Application) ->
    case lists:keyfind(Application, 1, application:loaded_applications()) of
        {_, _, Version} ->
            Version;
        false ->
            not_found
    end.
