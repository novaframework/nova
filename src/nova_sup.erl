%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Nova supervisor.
%%%
%%% Owns the Cowboy listeners. The bootstrap application gets one at startup
%%% from the `cowboy_configuration' key, and further applications can be
%%% started and stopped at runtime with {@link add_application/2} and
%%% {@link remove_application/1}.
%%%
%%% Each listener owns its own routing table, so an application started on a
%%% second port serves only its own routes. Applications added to a listener
%%% that is already bound to the same host and port share that listener's
%%% table instead.
%%% @end

-module(nova_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         add_application/2,
         remove_application/1,
         get_started_applications/0,
         listeners/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova.hrl").

-define(SERVER, ?MODULE).
-define(NOVA_LISTENER, nova_listener).
-define(NOVA_STD_PORT, 8080).
-define(NOVA_STD_SSL_PORT, 8443).
-define(NOVA_LISTENERS_TABLE, nova_listeners).

-type nova_app() :: atom() | {atom(), map()}.

-record(nova_listener, {
                        ref :: ranch:ref(),
                        apps = [] :: [atom()],
                        host :: inet:ip_address() | string(),
                        port :: inet:port_number(),
                        dispatch_key :: nova_router:dispatch_key(),
                        tls = false :: boolean()
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
%% Start a Nova application at runtime.
%%
%% `Configuration' takes the same shape as the `cowboy_configuration'
%% environment key. If a listener is already bound to the requested host and
%% port, the application's routes are added to that listener's routing table.
%% Otherwise a new listener is started with a routing table of its own.
%% @end
%%--------------------------------------------------------------------
-spec add_application(App :: atom(), Configuration :: map()) ->
          {ok, App :: atom(), Host :: inet:ip_address() | string(), Port :: inet:port_number()} |
          {error, Reason :: any()}.
add_application(App, Configuration) ->
    Host = maps:get(ip, Configuration, {0, 0, 0, 0}),
    Port = effective_port(Configuration),
    case find_listener(Host, Port) of
        {ok, Listener} ->
            attach_application(App, Listener);
        error ->
            start_listener(App, Host, Port, Configuration)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stop a Nova application. Its routes are removed from the listener serving
%% it, and the listener itself is stopped once no applications are left on it.
%% @end
%%--------------------------------------------------------------------
-spec remove_application(App :: atom()) -> ok | {error, not_found}.
remove_application(App) ->
    case [L || L = #nova_listener{apps = Apps} <- all_listeners(), lists:member(App, Apps)] of
        [] ->
            ?LOG_ERROR(#{msg => <<"Application not found">>, app => App}),
            {error, not_found};
        Listeners ->
            [detach_application(App, Listener) || Listener <- Listeners],
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Every started Nova application, with the listener serving it.
%% @end
%%--------------------------------------------------------------------
-spec get_started_applications() -> [#{app := atom(),
                                       host := inet:ip_address() | string(),
                                       port := inet:port_number(),
                                       listener := ranch:ref()}].
get_started_applications() ->
    [#{app => App, host => Host, port => Port, listener => Ref}
     || #nova_listener{ref = Ref, apps = Apps, host = Host, port = Port} <- all_listeners(),
        App <- Apps].

%%--------------------------------------------------------------------
%% @doc
%% Every Cowboy listener Nova has started. Used by the graceful shutdown in
%% nova_app, which has to drain all of them and not just the first.
%% @end
%%--------------------------------------------------------------------
-spec listeners() -> [ranch:ref()].
listeners() ->
    [Ref || #nova_listener{ref = Ref} <- all_listeners()].

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
    %% This is a bit ugly, but we need to do this anyhow(?)
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Environment = nova:get_environment(),

    nova_pubsub:start(),

    ?LOG_NOTICE(#{msg => <<"Starting nova">>, environment => Environment}),

    ensure_listener_table(),

    Configuration = application:get_env(nova, cowboy_configuration, #{}),

    SessionManager = application:get_env(nova, session_manager, nova_session_ets),

    Children0 = [
                 child(nova_handlers, nova_handlers),
                 child(nova_plugin_manager, nova_plugin_manager),
                 child(nova_watcher, nova_watcher)
                ],

    %% try to ensure callback module is loaded first
    ExportedFuns = SessionManager:module_info(exports),

    Children =
        case proplists:get_value(start_link, ExportedFuns) of
            0 -> [child(SessionManager, SessionManager) | Children0];
            _ -> Children0
        end,

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

%% The table survives a nova_sup restart, so creating it has to be idempotent.
%% It is public because add_application/2 and remove_application/1 are called
%% by whoever wants the application started, not by the supervisor process.
ensure_listener_table() ->
    case ets:whereis(?NOVA_LISTENERS_TABLE) of
        undefined ->
            ets:new(?NOVA_LISTENERS_TABLE,
                    [named_table, public, set, {keypos, #nova_listener.ref}]);
        _Tid ->
            ?NOVA_LISTENERS_TABLE
    end.

all_listeners() ->
    case ets:whereis(?NOVA_LISTENERS_TABLE) of
        undefined -> [];
        _Tid      -> ets:tab2list(?NOVA_LISTENERS_TABLE)
    end.

find_listener(Host, Port) ->
    case [L || L = #nova_listener{host = H, port = P} <- all_listeners(), H =:= Host, P =:= Port] of
        [Listener | _] -> {ok, Listener};
        []             -> error
    end.

setup_cowboy(Configuration) ->
    case start_cowboy(Configuration) of
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

-spec start_cowboy(Configuration :: map()) ->
          {ok, BootstrapApp :: atom(), Host :: inet:ip_address() | string(),
           Port :: inet:port_number()} | {error, Reason :: any()}.
start_cowboy(Configuration) ->
    BootstrapApp = application:get_env(nova, bootstrap_application, undefined),

    Dispatch =
        case BootstrapApp of
            undefined ->
                ?LOG_ERROR(#{msg => <<"You need to define bootstrap_application option in configuration">>}),
                throw({error, no_nova_app_defined});
            App ->
                ExtraApps = application:get_env(App, nova_apps, []),
                %% nova is compiled last so that its own 404/500 routes act as
                %% defaults. Routes are first-wins, so compiling nova first
                %% made an application's own status-code routes unreachable.
                nova_router:compile(resolve_nova_apps([App | ExtraApps] ++ [nova]))
        end,

    Host = maps:get(ip, Configuration, {0, 0, 0, 0}),
    Port = effective_port(Configuration),

    CowboyOptions = cowboy_options(Configuration, nova_dispatch, Dispatch),

    case bind(?NOVA_LISTENER, Host, Port, Configuration, CowboyOptions) of
        {ok, Tls} ->
            register_listener(#nova_listener{ref = ?NOVA_LISTENER,
                                             apps = [BootstrapApp],
                                             host = Host,
                                             port = Port,
                                             dispatch_key = nova_dispatch,
                                             tls = Tls}),
            {ok, BootstrapApp, Host, Port};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Runtime application lifecycle
%%--------------------------------------------------------------------

start_listener(App, Host, Port, Configuration) ->
    Ref = {?NOVA_LISTENER, App, Port},
    DispatchKey = {nova_dispatch, App, Port},
    ExtraApps = application:get_env(App, nova_apps, []),
    Dispatch = nova_router:compile(resolve_nova_apps([App | ExtraApps] ++ [nova]), DispatchKey),
    CowboyOptions = cowboy_options(Configuration, DispatchKey, Dispatch),
    case bind(Ref, Host, Port, Configuration, CowboyOptions) of
        {ok, Tls} ->
            register_listener(#nova_listener{ref = Ref,
                                             apps = [App],
                                             host = Host,
                                             port = Port,
                                             dispatch_key = DispatchKey,
                                             tls = Tls}),
            ?LOG_NOTICE(#{msg => <<"Started Nova application on a new listener">>,
                          app => App, port => Port, listener => Ref}),
            {ok, App, Host, Port};
        {error, Reason} ->
            ?LOG_ERROR(#{msg => <<"Could not start listener for application">>,
                         app => App, port => Port, reason => Reason}),
            {error, Reason}
    end.

attach_application(App, Listener = #nova_listener{ref = Ref, apps = Apps, host = Host,
                                                  port = Port, dispatch_key = DispatchKey}) ->
    case lists:member(App, Apps) of
        true ->
            {error, {already_started, App}};
        false ->
            ExtraApps = application:get_env(App, nova_apps, []),
            nova_router:compile(resolve_nova_apps([App | ExtraApps]), DispatchKey),
            register_listener(Listener#nova_listener{apps = Apps ++ [App]}),
            ?LOG_NOTICE(#{msg => <<"Added Nova application to an existing listener">>,
                          app => App, port => Port, listener => Ref}),
            {ok, App, Host, Port}
    end.

detach_application(App, #nova_listener{ref = Ref, apps = Apps, dispatch_key = DispatchKey} = Listener) ->
    ok = nova_router:remove_application(App, DispatchKey),
    case lists:delete(App, Apps) of
        [] ->
            ?LOG_NOTICE(#{msg => <<"Stopping cowboy listener">>, app => App, listener => Ref}),
            case cowboy:stop_listener(Ref) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR(#{msg => <<"Could not stop cowboy listener">>,
                                 listener => Ref, reason => Reason})
            end,
            ets:delete(?NOVA_LISTENERS_TABLE, Ref),
            ok = nova_router:delete_dispatch(DispatchKey),
            ok;
        Remaining ->
            register_listener(Listener#nova_listener{apps = Remaining}),
            ok
    end.

register_listener(Listener) ->
    ensure_listener_table(),
    true = ets:insert(?NOVA_LISTENERS_TABLE, Listener),
    ok.

%%--------------------------------------------------------------------
%% Cowboy plumbing
%%--------------------------------------------------------------------

%% The port a configuration actually binds. Only reading the `port' key made
%% a TLS listener on ssl_port invisible to the already-bound check.
effective_port(Configuration) ->
    case maps:get(use_ssl, Configuration, false) of
        false -> maps:get(port, Configuration, ?NOVA_STD_PORT);
        _     -> maps:get(ssl_port, Configuration, ?NOVA_STD_SSL_PORT)
    end.

cowboy_options(Configuration, DispatchKey, Dispatch) ->
    Middlewares = [
                   nova_router, %% Lookup routes
                   nova_plugin_handler, %% Handle pre-request plugins
                   nova_security_handler, %% Handle security
                   nova_handler, %% Controller
                   nova_plugin_handler %% Handle post-request plugins
                  ],
    StreamH = [nova_stream_h,
               cowboy_compress_h,
               cowboy_stream_h],
    StreamHandlers = maps:get(stream_handlers, Configuration, StreamH),
    MiddlewareHandlers = maps:get(middleware_handlers, Configuration, Middlewares),
    Options = maps:get(options, Configuration, #{compress => true}),

    %% nova_router reads its routing table out of the Env, so a listener always
    %% carries the key of the table it serves.
    Env0 = maps:get(env, Options, #{}),
    Env = Env0#{nova_dispatch_key => DispatchKey},

    CowboyOptions = Options#{middlewares => MiddlewareHandlers,
                             stream_handlers => StreamHandlers,
                             env => Env},

    case application:get_env(nova, use_persistent_term, true) of
        true -> CowboyOptions;
        _    -> CowboyOptions#{env => Env#{dispatch => Dispatch}}
    end.

%% Returns {ok, IsTls} so the caller can record how the listener was bound.
bind(Ref, Host, Port, Configuration, CowboyOptions) ->
    case maps:get(use_ssl, Configuration, false) of
        false ->
            case cowboy:start_clear(Ref, [{port, Port}, {ip, Host}], CowboyOptions) of
                {ok, _Pid} -> {ok, false};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            bind_tls(Ref, Host, Port, Configuration, CowboyOptions)
    end.

bind_tls(Ref, Host, Port, Configuration, CowboyOptions) ->
    TransportOpts =
        case maps:get(ca_cert, Configuration, undefined) of
            undefined ->
                SSLOptions = maps:get(ssl_options, Configuration, #{}),
                maps:to_list(SSLOptions#{port => Port, ip => Host});
            CACert ->
                Cert = maps:get(cert, Configuration),
                ?LOG_DEPRECATED(<<"0.10.3">>, <<"Use of ca_cert/cert is deprecated, use ssl_options instead">>),
                [{port, Port}, {ip, Host}, {certfile, Cert}, {cacertfile, CACert}]
        end,
    case cowboy:start_tls(Ref, TransportOpts, CowboyOptions) of
        {ok, _Pid} ->
            ?LOG_NOTICE(#{msg => <<"Nova starting SSL">>, port => Port}),
            {ok, true};
        {error, Reason} ->
            ?LOG_ERROR(#{msg => <<"Could not start cowboy with SSL">>, reason => Reason}),
            {error, Reason}
    end.

get_version(Application) ->
    case lists:keyfind(Application, 1, application:loaded_applications()) of
        {_, _, Version} ->
            erlang:list_to_binary(Version);
        false ->
            not_found
    end.

%% @doc Recursively resolve nested nova_apps.
%% Each nova_app can declare its own nova_apps dependencies.
%% Dependencies are resolved depth-first so child app routes
%% are registered before the parent. An application already resolved is
%% skipped, so a cycle terminates.
-spec resolve_nova_apps([nova_app()]) -> [nova_app()].
resolve_nova_apps(Apps) ->
    {Resolved, _Seen} = resolve_nova_apps(Apps, [], []),
    Resolved.

-spec resolve_nova_apps([nova_app()], [nova_app()], [atom()]) -> {[nova_app()], [atom()]}.
resolve_nova_apps([], Acc, Seen) ->
    {lists:reverse(Acc), Seen};
resolve_nova_apps([App | Rest], Acc, Seen) ->
    Name = nova_app_name(App),
    case lists:member(Name, Seen) of
        true ->
            resolve_nova_apps(Rest, Acc, Seen);
        false ->
            Nested = application:get_env(Name, nova_apps, []),
            {NestedApps, Seen0} = resolve_nova_apps(Nested, [], [Name | Seen]),
            resolve_nova_apps(Rest, [App | lists:reverse(NestedApps)] ++ Acc, Seen0)
    end.

%% A nova_app is either the application name or {Name, Options}.
-spec nova_app_name(nova_app()) -> atom().
nova_app_name({App, _Options}) -> App;
nova_app_name(App)             -> App.
