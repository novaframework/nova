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

-ifdef(TEST).
-export([listener_child_spec/1, clear_child_spec/3, tls_child_spec/3]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include("../include/nova.hrl").

-define(SERVER, ?MODULE).
-define(NOVA_LISTENER, nova_listener).
-define(NOVA_STD_PORT, 8080).
-define(NOVA_STD_SSL_PORT, 8443).


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

    UseStacktrace = application:get_env(nova, use_stacktrace, false),
    persistent_term:put(nova_use_stacktrace, UseStacktrace),

    %% The cowboy/ranch listener is a supervised child rather than a side
    %% effect, so a failed bind (e.g. eaddrinuse) fails init/1 and surfaces
    %% through application:start/1 instead of being logged and ignored.
    {ok, {SupFlags, Children ++ cowboy_childspecs(Configuration)}}.

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

cowboy_childspecs(Configuration) ->
    {ChildSpec, App, Host, Port} = listener_child_spec(Configuration),
    Host0 = inet:ntoa(Host),
    ?LOG_NOTICE(#{msg => <<"Nova is running">>,
                  url => unicode:characters_to_binary(io_lib:format("http://~s:~B", [Host0, Port])),
                  cowboy_version => get_version(cowboy), nova_version => get_version(nova), app => App}),
    [ChildSpec].

-spec listener_child_spec(Configuration :: map()) ->
          {supervisor:child_spec(), BootstrapApp :: atom(),
           Host :: string() | {integer(), integer(), integer(), integer()}, Port :: integer()}.
listener_child_spec(Configuration) ->
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

    %% Build the options map
    CowboyOptions1 = Options#{middlewares => MiddlewareHandlers,
                              stream_handlers => StreamHandlers},

    BootstrapApp = application:get_env(nova, bootstrap_application, undefined),

    %% Compile the routes
    Dispatch =
        case BootstrapApp of
            undefined ->
                ?LOG_ERROR(#{msg => <<"You need to define bootstrap_application option in configuration">>}),
                throw({error, no_nova_app_defined});
            App ->
                ExtraApps = application:get_env(App, nova_apps, []),
                nova_router:compile(resolve_nova_apps([nova, App | ExtraApps], []))
        end,

    CowboyOptions2 =
        case application:get_env(nova, use_persistent_term, true) of
            true ->
                CowboyOptions1;
            _ ->
                CowboyOptions1#{env => #{dispatch => Dispatch}}
        end,

    Host = maps:get(ip, Configuration, { 0, 0, 0, 0}),

    case maps:get(use_ssl, Configuration, false) of
        false ->
            Port = maps:get(port, Configuration, ?NOVA_STD_PORT),
            ChildSpec = clear_child_spec(?NOVA_LISTENER, [{port, Port}, {ip, Host}], CowboyOptions2),
            {ChildSpec, BootstrapApp, Host, Port};
        _ ->
            case maps:get(ca_cert, Configuration, undefined) of
                undefined ->
                    Port = maps:get(ssl_port, Configuration, ?NOVA_STD_SSL_PORT),
                    SSLOptions = maps:get(ssl_options, Configuration, #{}),
                    TransportOpts = maps:put(port, Port, SSLOptions),
                    TransportOpts1 = maps:put(ip, Host, TransportOpts),
                    ?LOG_NOTICE(#{msg => <<"Nova starting SSL">>, port => Port}),
                    ChildSpec = tls_child_spec(?NOVA_LISTENER, maps:to_list(TransportOpts1), CowboyOptions2),
                    {ChildSpec, BootstrapApp, Host, Port};
                CACert ->
                    Cert = maps:get(cert, Configuration),
                    Port = maps:get(ssl_port, Configuration, ?NOVA_STD_SSL_PORT),
                    ?LOG_DEPRECATED(<<"0.10.3">>, <<"Use of use_ssl is deprecated, use ssl instead">>),
                    ?LOG_NOTICE(#{msg => <<"Nova starting SSL">>, port => Port}),
                    ChildSpec = tls_child_spec(?NOVA_LISTENER,
                                               [{port, Port}, {ip, Host},
                                                {certfile, Cert}, {cacertfile, CACert}],
                                               CowboyOptions2),
                    {ChildSpec, BootstrapApp, Host, Port}
            end
    end.

%% These mirror cowboy:start_clear/3 and cowboy:start_tls/3 (cowboy 2.15) but
%% yield a supervisor:child_spec/0 via ranch:child_spec/5 so the listener lives
%% in nova's supervision tree instead of being started as a side effect. Keep
%% the option transforms in listener_opts/2 in sync if the cowboy pin changes.
clear_child_spec(Ref, TransOpts0, ProtoOpts0) ->
    {TransOpts, ProtoOpts} = listener_opts(TransOpts0, ProtoOpts0),
    ranch:child_spec(Ref, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts).

tls_child_spec(Ref, TransOpts0, ProtoOpts0) ->
    {TransOpts, ProtoOpts} = listener_opts(TransOpts0, ProtoOpts0),
    ranch:child_spec(Ref, ranch_ssl, TransOpts, cowboy_tls, ProtoOpts).

listener_opts(TransOpts0, ProtoOpts0) ->
    TransOpts1 = ranch:normalize_opts(TransOpts0),
    {TransOpts2, DynamicBuffer} = ensure_dynamic_buffer(TransOpts1, ProtoOpts0),
    {TransOpts, ConnectionType} = ensure_connection_type(TransOpts2),
    {TransOpts, ProtoOpts0#{connection_type => ConnectionType, dynamic_buffer => DynamicBuffer}}.

ensure_connection_type(TransOpts = #{connection_type := ConnectionType}) ->
    {TransOpts, ConnectionType};
ensure_connection_type(TransOpts) ->
    {TransOpts#{connection_type => supervisor}, supervisor}.

ensure_dynamic_buffer(TransOpts, #{dynamic_buffer := DynamicBuffer}) ->
    {TransOpts, DynamicBuffer};
ensure_dynamic_buffer(TransOpts = #{socket_opts := SocketOpts}, _) ->
    case proplists:get_value(buffer, SocketOpts, undefined) of
        undefined ->
            {TransOpts#{socket_opts => [{buffer, 512} | SocketOpts]}, {512, 131072}};
        _ ->
            {TransOpts, false}
    end;
ensure_dynamic_buffer(TransOpts, _) ->
    {TransOpts, false}.



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
%% are registered before the parent.
-spec resolve_nova_apps([atom()], [atom()]) -> [atom()].
resolve_nova_apps([], Acc) ->
    lists:reverse(Acc);
resolve_nova_apps([App | Rest], Acc) ->
    case lists:member(App, Acc) of
        true ->
            %% Already resolved — skip to prevent cycles
            resolve_nova_apps(Rest, Acc);
        false ->
            Nested = application:get_env(App, nova_apps, []),
            Acc1 = resolve_nova_apps(Nested, [App | Acc]),
            resolve_nova_apps(Rest, Acc1)
    end.
