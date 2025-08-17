%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Nova supervisor
%%% @end

-module(nova_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         add_application/2,
         remove_application/1,
         get_started_applications/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova.hrl").

-define(SERVER, ?MODULE).

-define(NOVA_LISTENER, fun(LApp, LPort) -> list_to_atom(atom_to_list(LApp) ++ integer_to_list(LPort)) end).
-define(NOVA_STD_PORT, 8080).
-define(NOVA_STD_SSL_PORT, 8443).
-define(NOVA_SUP_TABLE, nova_sup_table).
-define(COWBOY_LISTENERS, cowboy_listeners).


-record(nova_server, {
                      app :: atom(),
                      host :: inet:ip_address(),
                      port :: number(),
                      listener :: ranch:ref()
                     }).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Add a Nova application. This can either be on the same cowboy server that
%% a previous application was started with, or a new one if the configuration
%% ie port is different.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_application(App :: atom(), Configuration :: map()) -> {ok, App :: atom(),
                                                                 Host :: inet:ip_address(), Port :: number()}
              | {error, Reason :: any()}.
add_application(App, Configuration) ->
    setup_cowboy(App, Configuration).

%%--------------------------------------------------------------------
%% @doc
%% Get all started Nova applications. This will return a list of
%% #nova_server{} records that contains the application name, host, port
%% and listener reference.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_started_applications() -> [#{app => atom(), host => inet:ip_address(), port => number()}].
get_started_applications() ->
    %% Fetch all started applications from the ETS table
    Apps = ets:tab2list(?NOVA_SUP_TABLE),
    [ #{app => App, host => Host, port => Port} ||
        #nova_server{app = App, host = Host, port = Port} <- Apps ].

%%--------------------------------------------------------------------
%% @doc
%% Remove a Nova application. This will stop the cowboy listener so request
%% to that application will not be handled anymore.
%%
%% @end
%%--------------------------------------------------------------------
remove_application(App) ->
    case ets:lookup(?NOVA_SUP_TABLE, App) of
        [] ->
            ?LOG_ERROR(#{msg => <<"Application not found">>, app => App}),
            {error, not_found};
        [#nova_server{listener = Listener}] ->
            ?LOG_NOTICE(#{msg => <<"Stopping cowboy listener">>, app => App, listener => Listener}),
            cowboy:stop_listener(Listener),
            ets:delete(?NOVA_SUP_TABLE, App),
            ok
    end.

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
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% Initialize the ETS table for application state
    ets:new(?NOVA_SUP_TABLE, [named_table, protected, set]),

    %% This is a bit ugly, but we need to do this anyhow(?)
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    %% Bootstrap the environment
    Environment = nova:get_environment(),
    nova_pubsub:start(),

    ?LOG_NOTICE(#{msg => <<"Starting nova">>, environment => Environment}),

    SessionManager = application:get_env(nova, session_manager, nova_session_ets),

    Children = [
                child(nova_handlers, nova_handlers),
                child(SessionManager, SessionManager),
                child(nova_watcher, nova_watcher)
               ],

    setup_cowboy(),


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


%%%-------------------------------------------------------------------
%%% Nova Cowboy setup
%%%-------------------------------------------------------------------
setup_cowboy() ->
    CowboyConfiguration = application:get_env(nova, cowboy_configuration, #{}),
    BootstrapApp = application:get_env(nova, bootstrap_application, undefined),
    setup_cowboy(BootstrapApp, CowboyConfiguration).

setup_cowboy(BootstrapApp, Configuration) ->
    case start_cowboy(BootstrapApp, Configuration) of
        {ok, App, Host, Port} ->
            Host0 = inet:ntoa(Host),
            CowboyVersion = get_version(cowboy),
            NovaVersion = get_version(nova),
            UseStacktrace = application:get_env(nova, use_stacktrace, false),
            persistent_term:put(nova_use_stacktrace, UseStacktrace),
            ?LOG_NOTICE(#{msg => <<"Nova is running">>,
                          url => unicode:characters_to_binary(io_lib:format("http://~s:~B", [Host0, Port])),
                          cowboy_version => CowboyVersion, nova_version => NovaVersion, app => App});
        {error, Error} ->
            ?LOG_ERROR(#{msg => <<"Cowboy could not start">>, reason => Error})
    end.


-spec start_cowboy(BootstrapApp :: atom(), Configuration :: map()) ->
          {ok, BootstrapApp :: atom(), Host :: string() | {integer(), integer(), integer(), integer()},
           Port :: integer()} | {error, Reason :: any()}.
start_cowboy(BootstrapApp, Configuration) ->
    %% Determine if we have an already started cowboy on the host/port configuration
    Host = maps:get(ip, Configuration, { 0, 0, 0, 0}),
    Port = maps:get(port, Configuration, ?NOVA_STD_PORT),

    Listeners = nova:get_env(?COWBOY_LISTENERS, []),
    AlreadyStarted = lists:any(fun({X, Y}) -> X == Host andalso Y == Port end, Listeners),

    %% If yes we only need to add things to the dispatch
    case AlreadyStarted of
        true ->
            %% A cowboy listener is already running on this host/port configuration - just add to the
            %% dispatch.
            logger:info(#{msg => <<"There's already a Cowboy listener running with the host/port config. Adding routes to dispatch.">>, host => Host, port => Port}),
            ok;
        _ ->
            %% Cowboy configuration
            Middlewares = [
                           nova_router, %% Lookup routes
                           nova_plugin_handler, %% Handle pre-request plugins
                           nova_security_handler, %% Handle security
                           nova_handler, %% Controller
                           nova_plugin_handler %% Handle post-request plugins
                          ],
            StreamH = [
                       nova_stream_h,
                       cowboy_compress_h,
                       cowboy_stream_h
                      ],

            %% Good debug message in case someone wants to double check which config they are running with
            logger:debug(#{msg => <<"Configure cowboy">>, stream_handlers => StreamH, middlewares => Middlewares}),

            StreamHandlers = maps:get(stream_handlers, Configuration, StreamH),
            MiddlewareHandlers = maps:get(middleware_handlers, Configuration, Middlewares),
            Options = maps:get(options, Configuration, #{compress => true}),

            %% Build the options map
            CowboyOptions1 = Options#{middlewares => MiddlewareHandlers,
                                      stream_handlers => StreamHandlers},

            %% Compile the routes
            Dispatch =
                case BootstrapApp of
                    undefined ->
                        ?LOG_ERROR(#{msg => <<"You need to define bootstrap_application option in configuration">>}),
                        throw({error, no_nova_app_defined});
                    App ->
                        ExtraApps = application:get_env(App, nova_apps, []),
                        nova_router:compile([nova|[App|ExtraApps]])
                end,

            CowboyOptions2 =
                case application:get_env(nova, use_persistent_term, true) of
                    true ->
                        CowboyOptions1;
                    _ ->
                        CowboyOptions1#{env => #{dispatch => Dispatch}}
                end,

            case maps:get(use_ssl, Configuration, false) of
                false ->
                    case cowboy:start_clear(
                           ?NOVA_LISTENER(BootstrapApp, Port),
                           [{port, Port},
                            {ip, Host}],
                           CowboyOptions2) of
                        {ok, _Pid} ->
                            nova:set_env(?COWBOY_LISTENERS, [{Host, Port}|Listeners]),
                            ets:insert(?NOVA_SUP_TABLE, #nova_server{
                                                           app = BootstrapApp,
                                                           host = Host,
                                                           port = Port,
                                                           listener = ?NOVA_LISTENER(BootstrapApp, Port)
                                                          }),
                            {ok, BootstrapApp, Host, Port};
                        Error ->
                            Error
                    end;
                _ ->
                    SSLPort = maps:get(ssl_port, Configuration, ?NOVA_STD_SSL_PORT),
                    SSLOptions = maps:get(ssl_options, Configuration, #{}),
                    TransportOpts = maps:put(port, SSLPort, SSLOptions),
                    TransportOpts1 = maps:put(ip, Host, TransportOpts),

                    case cowboy:start_tls(
                           ?NOVA_LISTENER(BootstrapApp, SSLPort),
                           maps:to_list(TransportOpts1), CowboyOptions2) of
                        {ok, _Pid} ->
                            ?LOG_NOTICE(#{msg => <<"Nova starting SSL">>, port => SSLPort}),
                            ets:insert(?NOVA_SUP_TABLE, #nova_server{
                                                           app = BootstrapApp,
                                                           host = Host,
                                                           port = SSLPort,
                                                           listener = ?NOVA_LISTENER(BootstrapApp, SSLPort)
                                                          }),
                            nova:set_env(?COWBOY_LISTENERS, [{Host, SSLPort}|Listeners]),
                            {ok, BootstrapApp, Host, SSLPort};
                        Error ->
                            ?LOG_ERROR(#{msg => <<"Could not start cowboy with SSL">>, reason => Error}),
                            Error
                    end
            end
    end.



get_version(Application) ->
    case lists:keyfind(Application, 1, application:loaded_applications()) of
        {_, _, Version} ->
            erlang:list_to_binary(Version);
        false ->
            not_found
    end.
