%%% Author: Niclas Axelsson <niclas@burbas.se>

-module(nova_sup).
-moduledoc """
Nova supervisor.

Keeps track of the listeners. Those of the `listeners` configuration are started by the server package as its application starts, and further applications can be started and stopped at runtime with `add_application/2` and `remove_application/1`.

Each listener owns its own routing table, so an application started on a second port serves only its own routes. Applications added to a listener that is already bound to the same host and port share that listener's table instead.
""".

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         add_application/2,
         remove_application/1,
         get_started_applications/0,
         listeners/0,
         dispatch_keys/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-ifdef(TEST).
-export([warn_without_session_exists/2]).
-endif.

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(NOVA_LISTENERS_TABLE, nova_listeners).
-define(ATTACH_TRIES, 3).

-type nova_app() :: atom() | {atom(), map()}.

%% Called by nova_listener_owner, in the application of the server package
-export([start_listeners/1, shutdown_listeners/0, shutdown_listeners/1, stop_listeners/1]).

%% Called by nova_applications
-export([running/0, added/0, drop_unbound/0, listeners_owner/0, attach_to_listener/2, start_listener_for/3, detach_application/1,
         unserved_groups/0, check_route_groups/1]).

%% Started by the supervisor as its first child
-export([start_pubsub_scope/0]).

-record(nova_listener, {
                        name :: nova_listener:name(),
                        bind :: {inet:ip_address() | string(), inet:port_number()},
                        server :: module(),
                        apps = [] :: [atom()],
                        dispatch_key :: nova_router:dispatch_key(),
                        %% Those of `apps` that were added at runtime, and
                        %% whether the listener was started for one of them.
                        added = [] :: [atom()],
                        for_application = false :: boolean(),
                        %% A listener is listed before the server binds
                        %% it, and bound once the server said it is.
                        bound = false :: boolean()
                       }).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
-doc """
Starts the supervisor of the `nova` application. The listeners are started
by the application of the server package, which starts after this one.
""".
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
-doc """
Start a Nova application at runtime.

`Settings` are those of a listener, as `nova_listener` describes them, or
those of `cowboy_configuration` as Nova took them before 1.0. With
`#{listener => Name}` the routes of the application are added to the
listener of that name. Without it they are added to the listener already
bound to the `ip` and `port` of the settings, and when there is none a
listener named after the application is started with them, unless a
listener has that name already: `{error, {listener_exists, App}}`.

A listener that is there serves as it was started, so an application is
not added to it with settings that ask it for something else, `tls` or
`steps` say: `{error, {listener_settings_ignored, Name, Keys}}`. The
settings a listener was started with for an application are those it has.
`port => 0` is refused, since the port that would be is not known when the
application is added: `{error, {invalid_listener_setting, App, port, 0}}`.

An application that starts before the server package has, as a library that
calls this from its `start/2` may, is added once the listeners of the
configuration are started. The answer is where it will be served, and the
node does not start when it cannot be: the server package stops with
`{application_not_added, App, Reason}`. A route group of its that names a
listener which does not serve it is such a reason.

Nova remembers the applications that were added until `remove_application/1`
takes them out, and adds them again when the server package is started
again. A route group of one that was added to running listeners, and that
names a listener which does not serve it, is logged as a warning, then and
when it is added again. A listener that was started for an application
keeps its name and its settings for as long as an application is served on
it, also when the one it was started for is removed.

An application is added again to the listener it was served on. When it
was added by `ip` and `port` to a listener that was there already, and that
listener is gone or moved, none is started for it and the server package
stops with `{application_not_added, App, {no_such_listener, Name}}`. When
the address of a listener that was started for an application is that of
another by then, it stops with `{address_in_use, Other}` as the reason.

An application the configuration lists on the listener it is added to is
served there already: the answer says where, and it is not remembered as
added. `{error, {already_started, App}}` is the answer to adding an
application to a listener a second time.

Applications are added and removed one at a time. The routes of an added
application are compiled in a process of Nova's that does not outlive the
call, and a router that calls this from there gets
`{error, adding_an_application}`.

When the server package begins to stop, the application that is being
added is waited for: `shutdown_drain_timeout`, before the connections of
the listeners are. One that is still being added by then is not added, and
the call exits with `killed`. An application that is added from then on,
or that waited behind that one, is kept for the next start of the server
package, as one that is added before the first.

`{error, {no_such_listener, Name}}` when no listener has the name, and when
the listener was stopped or started again while the router of the
application ran. Its routes are then not served.

`{error, {routing_table_changed, Name}}` when routes were added to the
listener with `nova_router`, or an application was removed from it there,
each time the router of the application ran. The application is compiled
again when that happens, three times at most.

`{error, server_not_started}` when the `nova` application is not running.
""".
-spec add_application(App :: atom(), Settings :: map()) ->
          {ok, App :: atom(), Host :: inet:ip_address() | string(), Port :: inet:port_number()} |
          {error, Reason :: any()}.
add_application(App, Settings) ->
    nova_applications:add(App, Settings).

-doc false.
%% For nova_applications: the listeners that are up, each with where it is
%% bound and the applications on it.
-spec running() -> [{nova_listener:name(), {inet:ip_address() | string(), inet:port_number()}, [atom()]}].
running() ->
    [{Name, Bind, Apps} || #nova_listener{name = Name, bind = Bind, apps = Apps} <- all_listeners()].

-doc false.
%% For nova_applications when it is started again. A listener that was
%% being started for an application then was started by a process that
%% went with the one before. Whether the server bound it is not known, and
%% no call that added the application was answered: it is stopped.
-spec drop_unbound() -> ok.
drop_unbound() ->
    _ = [begin
             ?LOG_WARNING(#{msg => ~"A listener was being started for an application as the process that keeps the added applications stopped. It is stopped, and the application is not added",
                            listener => Name}),
             stop_listener(Name)
         end || #nova_listener{name = Name, for_application = true, bound = false} <- all_listeners()],
    ok.

-doc false.
%% For nova_applications when it is started again: what was added to the
%% listeners that are up, and whether a listener was started for it.
-spec added() -> [{nova_listener:name(), {inet:ip_address() | string(), inet:port_number()}, [atom()], boolean()}].
added() ->
    [{Name, Bind, Added, ForApplication}
     || #nova_listener{name = Name, bind = Bind, added = Added, for_application = ForApplication}
            <- all_listeners()].

-doc false.
%% For nova_applications: the process that has the listeners, when there
%% are any.
-spec listeners_owner() -> pid() | undefined.
listeners_owner() ->
    case ets:info(?NOVA_LISTENERS_TABLE, owner) of
        Owner when is_pid(Owner) -> Owner;
        _NoTable -> undefined
    end.

-doc false.
%% For nova_applications, which adds one application at a time. An
%% application the configuration lists on the listener is served there
%% already, which is what was asked for.
-spec attach_to_listener(App :: atom(), Name) ->
          {ok, Host :: inet:ip_address() | string(), Port :: inet:port_number()} |
          {error, {no_such_listener | routing_table_changed, Name}}
              when Name :: nova_listener:name().
attach_to_listener(App, Name) ->
    case find_listener(Name) of
        {ok, Listener = #nova_listener{bind = {Host, Port}}} ->
            case attach_application(App, Listener) of
                {error, {no_such_listener, Name}} -> {error, {no_such_listener, Name}};
                {error, {routing_table_changed, Name}} -> {error, {routing_table_changed, Name}};
                _AddedOrListed -> {ok, Host, Port}
            end;
        error ->
            {error, {no_such_listener, Name}}
    end.

-doc false.
%% For nova_applications. A listener that is started for an application
%% has the name of the one it was first started for, which a listener of
%% the configuration may have by now, and its own address, which one may be
%% bound to.
-spec start_listener_for(Name :: nova_listener:name(), App :: atom(), Settings :: map()) ->
          {ok, Host :: inet:ip_address() | string(), Port :: inet:port_number()} |
          {error, Reason :: any()}.
start_listener_for(Name, App, Settings) ->
    case nova_listener:normalize(Name, nova_listener:runtime_settings(Settings), #{applications => [App]}) of
        {ok, #{ip := Host, port := Port} = Listener} ->
            case {find_listener(Name), find_listener(Host, Port)} of
                {{ok, _Named}, _Bound} ->
                    {error, {listener_exists, Name}};
                {error, {ok, #nova_listener{name = By}}} ->
                    {error, {address_in_use, By}};
                {error, error} ->
                    case start_checked(nova_server:module(), Listener#{applications := [App]}, [App]) of
                        {ok, App, Host, Port} -> {ok, Host, Port};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
-doc """
Stop a Nova application. Its routes are removed from the listener serving
it, and the listener itself is stopped once no applications are left on it.

`{error, not_found}` when it is not served and was not added, and
`{error, adding_an_application}` to a router that calls this while an
application is added and its routes are compiled.
""".
-spec remove_application(App :: atom()) -> ok | {error, not_found | adding_an_application}.
remove_application(App) ->
    nova_applications:remove(App).

-doc false.
%% For nova_applications.
-spec detach_application(App :: atom()) -> ok | {error, not_found}.
detach_application(App) ->
    case [L || L = #nova_listener{apps = Apps} <- all_listeners(), lists:member(App, Apps)] of
        [] ->
            {error, not_found};
        Listeners ->
            _ = [detach_application(App, Listener) || Listener <- Listeners],
            ok
    end.

%%--------------------------------------------------------------------
-doc "Every started Nova application, with the listener serving it.".
-spec get_started_applications() -> [#{app := atom(),
                                       host := inet:ip_address() | string(),
                                       port := inet:port_number(),
                                       listener := nova_listener:name()}].
get_started_applications() ->
    [#{app => App, host => Host, port => Port, listener => Name}
     || #nova_listener{name = Name, apps = Apps, bind = {Host, Port}} <- all_listeners(),
        App <- Apps].

%%--------------------------------------------------------------------
-doc "The names of the listeners that are started.".
-spec listeners() -> [nova_listener:name()].
listeners() ->
    [Name || #nova_listener{name = Name} <- all_listeners()].

%%--------------------------------------------------------------------
-doc false.
%% Called by nova_listener_owner as it starts, in the application of a
%% server package, after the `nova` application and the server itself have
%% started. The caller owns the table of listeners from here on, so there
%% are listeners for as long as it lives. A package that is started on a
%% node whose server is another starts none.
-spec start_listeners(Server :: module()) -> ok | {error, Reason :: any()}.
start_listeners(Server) ->
    case nova_server:module() of
        Server ->
            ?NOVA_LISTENERS_TABLE = ets:new(?NOVA_LISTENERS_TABLE,
                                            [named_table, public, set, {keypos, #nova_listener.bind}]),
            %% Whatever stops the boot, a route that does not compile
            %% included, the listeners started before it are stopped: the
            %% owner that would stop them never comes to be.
            try start_then_add(Server) of
                ok ->
                    ok;
                {error, Reason} ->
                    ok = stop_listeners(Server),
                    {error, nova_redact:tls_options(Reason)}
            catch
                throw:{error, Reason} ->
                    ok = stop_listeners(Server),
                    {error, nova_redact:tls_options(Reason)};
                Class:Reason:Stacktrace ->
                    ok = stop_listeners(Server),
                    erlang:raise(Class, nova_redact:tls_options(Reason), nova_redact:stacktrace(Stacktrace))
            end;
        Other ->
            ?LOG_NOTICE(#{msg => ~"Server package is started but not used: the server key of the nova application names another",
                          started => Server, server => Other}),
            ok
    end.

%% The applications that were added are added again, and from here on one
%% is added to a listener as it is asked for.
start_then_add(Server) ->
    case start_configured(Server) of
        ok -> nova_applications:listening(self());
        {error, Reason} -> {error, Reason}
    end.

-doc false.
%% Stops taking connections, waits for the open ones and stops the listeners:
%% those of one server when its application stops, and whatever is left when
%% `nova` does.
-spec shutdown_listeners() -> ok.
shutdown_listeners() ->
    ok = nova_applications:draining(),
    graceful_shutdown(all_listeners()).

-spec shutdown_listeners(Server :: module()) -> ok.
shutdown_listeners(Server) ->
    case nova_server:module() of
        Server ->
            ok = nova_applications:draining(),
            graceful_shutdown(listeners_of(Server));
        _Other ->
            ok
    end.

-doc false.
%% Stops the listeners of a server as they are, without waiting for their
%% connections.
-spec stop_listeners(Server :: module()) -> ok.
stop_listeners(Server) ->
    _ = [stop_listener(Name) || #nova_listener{name = Name} <- listeners_of(Server)],
    ok.

listeners_of(Server) ->
    [Listener || Listener = #nova_listener{server = S} <- all_listeners(), S =:= Server].

%%--------------------------------------------------------------------
-doc false.
-spec dispatch_keys() -> [nova_router:dispatch_key()].
dispatch_keys() ->
    [Key || #nova_listener{dispatch_key = Key} <- all_listeners()].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
-doc false.
init([]) ->
    %% This is a bit ugly, but we need to do this anyhow(?)
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Environment = nova:get_environment(),

    ?LOG_NOTICE(#{msg => ~"Starting nova", environment => Environment}),

    ok = nova_settings:validate(),

    persistent_term:put(nova_use_stacktrace, application:get_env(nova, use_stacktrace, false)),

    SessionManager = application:get_env(nova, session_manager, nova_session_ets),

    Children0 = [
                 child(nova_applications, nova_applications),
                 child(nova_handlers, nova_handlers),
                 child(nova_plugin_manager, nova_plugin_manager),
                 child(nova_multipart_spool, nova_multipart_spool),
                 child(nova_watcher, nova_watcher)
                ],

    %% try to ensure callback module is loaded first
    ExportedFuns = SessionManager:module_info(exports),
    warn_without_session_exists(SessionManager, ExportedFuns),
    case nova_session:enabled() of
        true -> nova_session:validate_cookie_config();
        false -> ok
    end,

    Children =
        case proplists:get_value(start_link, ExportedFuns) of
            0 -> [child(SessionManager, SessionManager) | Children0];
            _ -> Children0
        end,

    {ok, {SupFlags, [pubsub_scope_child() | Children]}}.

-doc false.
-spec start_pubsub_scope() -> {ok, pid()} | ignore.
start_pubsub_scope() ->
    case pg:start_link(nova_scope) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, _Pid}} -> ignore
    end.

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

%% The scope nova_pubsub and the ETS session backend use. As a child it is
%% restarted when it dies. A scope that something else started before Nova
%% is left with its owner.
pubsub_scope_child() ->
    #{id => nova_pubsub_scope,
      start => {?MODULE, start_pubsub_scope, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [pg]}.

all_listeners() ->
    case ets:whereis(?NOVA_LISTENERS_TABLE) of
        undefined -> [];
        _Tid      -> ets:tab2list(?NOVA_LISTENERS_TABLE)
    end.

find_listener(Host, Port) ->
    case ets:whereis(?NOVA_LISTENERS_TABLE) of
        undefined ->
            error;
        Tid ->
            case ets:lookup(Tid, {Host, Port}) of
                [Listener] -> {ok, Listener};
                []         -> error
            end
    end.

find_listener(Name) ->
    case [Listener || Listener = #nova_listener{name = N} <- all_listeners(), N =:= Name] of
        [Listener] -> {ok, Listener};
        [] -> error
    end.

%%--------------------------------------------------------------------
%% The listeners of the configuration
%%--------------------------------------------------------------------

start_configured(Server) ->
    case application:get_env(nova, bootstrap_application, undefined) of
        undefined ->
            ?LOG_ERROR(#{msg => ~"You need to define bootstrap_application option in configuration"}),
            {error, no_nova_app_defined};
        _Bootstrap ->
            case nova_listener:configured() of
                {ok, Listeners} ->
                    note_legacy_configuration(),
                    start_all(Server, Listeners);
                {error, Reason} ->
                    ?LOG_ERROR(#{msg => ~"The listeners of the nova application are not configured as nova_listener describes",
                                 reason => nova_redact:tls_options(Reason)}),
                    {error, Reason}
            end
    end.

note_legacy_configuration() ->
    case application:get_env(nova, listeners) =:= undefined
        andalso application:get_env(nova, cowboy_configuration) =/= undefined of
        true ->
            ?LOG_NOTICE(#{msg => ~"cowboy_configuration is read as the settings of one listener named default. Write it as {listeners, #{default => #{...}}}: see nova_listener",
                          key => cowboy_configuration});
        false ->
            ok
    end.

%% The applications that were added come after the listeners of the
%% configuration, and the check of its route groups after those: one of
%% them may be what serves a group.
start_all(_Server, []) ->
    ok;
start_all(Server, [Listener | Rest]) ->
    case start_checked(Server, Listener, []) of
        {ok, _App, _Host, _Port} -> start_all(Server, Rest);
        {error, Reason} -> {error, Reason}
    end.

-doc false.
%% For nova_applications. `Configured` are the groups no listener served
%% before the applications that were added: those are the groups of the
%% configuration, and nova_applications sees to the ones they brought.
-spec check_route_groups([Group]) -> ok | {error, {route_group_not_served, atom(), string(), nova_listener:name()}}
              when Group :: {App :: atom(), Prefix :: string(), Listener :: nova_listener:name()}.
check_route_groups(Configured) ->
    case [Group || Group <- unserved_groups(), lists:member(Group, Configured)] of
        [] ->
            ok;
        [{App, Prefix, Listener} | _] ->
            ?LOG_ERROR(#{msg => ~"A route group names a listener that is not configured or does not serve its application",
                         app => App, prefix => Prefix, listener => Listener, listeners => listeners()}),
            {error, {route_group_not_served, App, Prefix, Listener}}
    end.

-doc false.
%% The route groups that name a listener whose routing table they are not
%% in: one that is not there, or one the application is not served on.
-spec unserved_groups() -> [{App :: atom(), Prefix :: string(), Listener :: nova_listener:name()}].
unserved_groups() ->
    Bound = [{Name, Group} || #nova_listener{name = Name, dispatch_key = DispatchKey} <- all_listeners(),
                              Group <- nova_router:bound_groups(DispatchKey)],
    lists:usort([{App, Prefix, Listener}
                 || {_On, {_Status, App, Prefix, Names}} <- Bound,
                    Listener <- Names,
                    not lists:member({Listener, {served, App, Prefix, Names}}, Bound)]).

%% `For` is the application a listener is started for, when it is.
start_checked(Server, Listener = #{name := Name}, For) ->
    case checked(Server, Listener) of
        ok ->
            start_listener(Server, Listener, For);
        {error, Reason} ->
            ?LOG_ERROR(#{msg => ~"Listener asks for what the server cannot do", listener => Name,
                         server => Server, reason => nova_redact:tls_options(Reason)}),
            {error, Reason}
    end.

checked(Server, Listener) ->
    case nova_listener:check(Listener, Server, nova_server:capabilities(Server)) of
        ok ->
            case nova_server:supports(Server, {check_listener, 1}) of
                true -> Server:check_listener(Listener);
                false -> ok
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% The table without a key is the one of the listener that serves the
%% bootstrap application: every keyless nova_router caller addresses it.
dispatch_key(#{name := Name, applications := Apps}) ->
    Bootstrap = application:get_env(nova, bootstrap_application, undefined),
    case lists:member(Bootstrap, Apps) andalso not lists:member(nova_dispatch, dispatch_keys()) of
        true -> nova_dispatch;
        false -> {nova_dispatch, Name}
    end.

start_listener(Server, Listener = #{name := Name, ip := Host, port := Port, applications := Apps,
                                    protocols := Protocols}, For) ->
    DispatchKey = dispatch_key(Listener),
    %% The applications a listener serves say in their environment which
    %% others they mount, so it has to be loaded by now. It is in a release.
    _ = [application:load(Each) || Each <- Apps],
    %% Routes are first-wins. An application comes before those it mounts so
    %% nothing it mounts can take over one of its routes, and nova last so its
    %% 404/500 routes are only defaults.
    NovaApps = lists:append([with_nova_apps(Each) || Each <- Apps]) ++ [nova],
    Names = nova_listener:protocol_names(Protocols),
    Serving = #{name => Name,
                protocols => Names,
                websocket => [Version || Version <- websocket_protocols(Server, Listener),
                                         lists:member(Version, Names)]},
    Dispatch = try
                   nova_router:compile_fresh(resolve_nova_apps(NovaApps), DispatchKey, Serving)
               catch
                   Class:NotCompiled:Stacktrace ->
                       ok = nova_router:delete_dispatch(DispatchKey),
                       erlang:raise(Class, NotCompiled, Stacktrace)
               end,
    %% The listener is listed before the server binds it. What stops the
    %% listeners stops the ones that are listed, and the process that
    %% starts this one can be stopped as the server binds: it is then not
    %% left serving with nothing that knows of it. It says from then on
    %% that it is started for an application, which is how the process
    %% that keeps the added applications knows of it when that is started
    %% again.
    case reserve_listener(#nova_listener{name = Name, bind = {Host, Port}, server = Server, apps = Apps,
                                         dispatch_key = DispatchKey, added = For,
                                         for_application = For =/= []}) of
        ok ->
            try Server:start_listener(Listener, env(Server, Listener, DispatchKey, Dispatch)) of
                {ok, _Pid} ->
                    started(Server, Listener, DispatchKey);
                {error, NotStarted} ->
                    ok = forget_listener({Host, Port}),
                    not_started(Server, Listener, DispatchKey, nova_redact:tls_options(NotStarted))
            catch
                Kind:Raised:Where ->
                    ok = forget_listener({Host, Port}),
                    ok = nova_router:delete_dispatch(DispatchKey),
                    erlang:raise(Kind, Raised, Where)
            end;
        {error, NotListed} ->
            not_started(Server, Listener, DispatchKey, NotListed)
    end.

not_started(Server, #{name := Name, port := Port}, DispatchKey, Reason) ->
    ok = nova_router:delete_dispatch(DispatchKey),
    ?LOG_ERROR(#{msg => ~"Could not start listener",
                 listener => Name, port => Port, server => Server, reason => Reason}),
    {error, {listener_not_started, Reason}}.

%% The listeners go with their owner, and that can be as one is started for
%% an application. It is then not left serving with nothing that stops it.
started(Server, #{name := Name, ip := Host, port := Port, tls := Tls, applications := [App | _] = Apps,
                  protocols := Protocols},
        DispatchKey) ->
    Names = nova_listener:protocol_names(Protocols),
    case bound({Host, Port}, Name) of
        true ->
            say_what_the_session_cookie_needs(Tls =/= false),
            note_http3_only(Name, Names),
            ?LOG_NOTICE(#{msg => ~"Nova is running",
                          url => unicode:characters_to_binary(
                                   io_lib:format("~s://~s:~B", [scheme(Tls), format_host(Host), Port])),
                          listener => Name,
                          protocols => Names,
                          server => Server,
                          nova_version => get_version(nova),
                          apps => Apps}),
            {ok, App, Host, Port};
        false ->
            _ = Server:stop_listener(Name),
            ok = nova_router:delete_dispatch(DispatchKey),
            ?LOG_ERROR(#{msg => ~"The listeners were stopped as one was started, which is stopped with them",
                         listener => Name, port => Port, server => Server}),
            {error, {listener_not_started, listeners_stopped}}
    end.

%% Whether the listener is still listed under its name, and bound from
%% here on. One that was stopped meanwhile is not listed again by this.
bound({Host, Port}, Name) ->
    case find_listener(Host, Port) of
        {ok, #nova_listener{name = Name}} ->
            try
                ets:update_element(?NOVA_LISTENERS_TABLE, {Host, Port}, {#nova_listener.bound, true})
            catch
                error:badarg -> false
            end;
        _GoneOrAnother ->
            false
    end.

%% The versions a server takes WebSockets over can depend on how the
%% listener is set up, which the server then says itself.
websocket_protocols(Server, Listener) ->
    case nova_server:supports(Server, {websocket_protocols, 1}) of
        true -> Server:websocket_protocols(Listener);
        false -> maps:get(websocket, nova_server:capabilities(Server))
    end.

%% A client tries HTTP/3 once it is told of it: by the alt-svc header of an
%% answer over another version, or by an HTTPS record in DNS.
note_http3_only(Name, [http3]) ->
    ?LOG_NOTICE(#{msg => ~"Listener serves HTTP/3 alone. Browsers only come to it when an HTTPS record in DNS says it is there",
                  listener => Name});
note_http3_only(_Name, _Protocols) ->
    ok.

scheme(false) -> "http";
scheme(_Tls) -> "https".

%% What every request of a listener runs with. nova_router reads its routing
%% table out of it, so a listener always carries the key of the table it
%% serves.
env(Server, #{name := Name, steps := Steps}, DispatchKey, Dispatch) ->
    Env = #{nova_dispatch_key => DispatchKey, nova_steps => Steps, listener => Name, nova_server => Server},
    case application:get_env(nova, use_persistent_term, true) of
        true -> Env;
        _    -> Env#{dispatch => Dispatch}
    end.

%% Nova cannot see whether a proxy in front of a listener without TLS has it,
%% and a browser that reached the site over plain http drops a Secure cookie
%% without a word. So it is said once, when the node starts.
say_what_the_session_cookie_needs(true) ->
    ok;
say_what_the_session_cookie_needs(false) ->
    case nova_session:needs_https() of
        true ->
            ?LOG_NOTICE(#{msg => ~"The session cookie is Secure, so a browser keeps it only where it reached the site over https. This listener has no TLS, which is right behind a proxy that terminates TLS. Where browsers reach it over plain http no session is kept: set environment to dev on a development machine, or session_cookie_opts to #{secure => false} together with a session_cookie_name without the __Host- prefix",
                          notice => session_cookie_needs_https,
                          cookie => nova_session:cookie_name(),
                          settings => [session_cookie_name, session_cookie_opts, environment]});
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% Runtime application lifecycle
%%--------------------------------------------------------------------

attach_application(App, Listener) ->
    attach_application(App, Listener, ?ATTACH_TRIES).

attach_application(App, Listener = #nova_listener{name = Name, apps = Apps, bind = {Host, Port},
                                                  dispatch_key = DispatchKey}, Tries) ->
    case lists:member(App, Apps) of
        true ->
            {error, {already_started, App}};
        false ->
            try nova_router:compile(resolve_nova_apps(with_nova_apps(App)), DispatchKey) of
                _Dispatch -> listed(App, Name, {Host, Port}, DispatchKey)
            catch
                %% The listener was started again as the router ran.
                error:{routing_table_replaced, DispatchKey} ->
                    {error, {no_such_listener, Name}};
                %% Routes were stored into the table as the router ran,
                %% which is no reason not to add the application to it.
                error:{routing_table_changed, DispatchKey} when Tries > 1 ->
                    attach_application(App, Listener, Tries - 1);
                error:{routing_table_changed, DispatchKey} ->
                    {error, {routing_table_changed, Name}}
            end
    end.

%% The router of the application ran meanwhile, so the listener is what it
%% is now and not what it was before.
listed(App, Name, {Host, Port}, DispatchKey) ->
    case find_listener(Name) of
        {ok, Listener = #nova_listener{apps = Now, added = Added, bind = {Host, Port},
                                       dispatch_key = DispatchKey}} ->
            case register_listener(Listener#nova_listener{apps = Now ++ [App], added = Added ++ [App]}) of
                ok ->
                    ?LOG_NOTICE(#{msg => ~"Added Nova application to an existing listener",
                                  app => App, port => Port, listener => Name}),
                    {ok, App, Host, Port};
                {error, listeners_stopped} ->
                    {error, {no_such_listener, Name}}
            end;
        _GoneOrAnother ->
            {error, {no_such_listener, Name}}
    end.

detach_application(App, #nova_listener{name = Name, apps = Apps, added = Added,
                                       dispatch_key = DispatchKey} = Listener) ->
    ok = try
             nova_router:remove_application(App, DispatchKey)
         catch
             %% The table is built again, which only code outside Nova
             %% does to that of a listener that is up. The application is
             %% not among those of the listener from here on.
             error:{routing_table_replaced, DispatchKey} -> ok
         end,
    case lists:delete(App, Apps) of
        [] ->
            ?LOG_NOTICE(#{msg => ~"Stopping listener", app => App, listener => Name}),
            stop_listener(Name);
        Remaining ->
            _ = register_listener(Listener#nova_listener{apps = Remaining, added = lists:delete(App, Added)}),
            ok
    end.

stop_listener(Name) ->
    case find_listener(Name) of
        {ok, #nova_listener{server = Server, bind = Bind, dispatch_key = DispatchKey}} ->
            case Server:stop_listener(Name) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR(#{msg => ~"Could not stop listener", listener => Name, reason => Reason})
            end,
            ok = forget_listener(Bind),
            ok = nova_router:delete_dispatch(DispatchKey);
        error ->
            ok
    end.

register_listener(Listener) ->
    try ets:insert(?NOVA_LISTENERS_TABLE, Listener) of
        true -> ok
    catch
        error:badarg -> {error, listeners_stopped}
    end.

%% Under an address no other listener of Nova's has.
reserve_listener(Listener) ->
    try ets:insert_new(?NOVA_LISTENERS_TABLE, Listener) of
        true -> ok;
        false -> {error, eaddrinuse}
    catch
        error:badarg -> {error, listeners_stopped}
    end.

forget_listener(Bind) ->
    try ets:delete(?NOVA_LISTENERS_TABLE, Bind) of
        true -> ok
    catch
        error:badarg -> ok
    end.

%%--------------------------------------------------------------------
%% Shutdown
%%--------------------------------------------------------------------

graceful_shutdown([]) ->
    ok;
graceful_shutdown(Listeners) ->
    case nova_settings:current(shutdown_delay) of
        Delay when is_integer(Delay), Delay > 0 ->
            ?LOG_NOTICE(#{msg => ~"Graceful shutdown started", delay_ms => Delay}),
            timer:sleep(Delay);
        _None ->
            ok
    end,
    DrainTimeout = nova_settings:current(shutdown_drain_timeout),
    Names = [Name || #nova_listener{name = Name} <- Listeners],
    ?LOG_NOTICE(#{msg => ~"Draining connections", listeners => Names, timeout_ms => DrainTimeout}),
    _ = [drain(Server, [Name || #nova_listener{name = Name, server = S} <- Listeners, S =:= Server], DrainTimeout)
         || Server <- lists:usort([Server || #nova_listener{server = Server} <- Listeners])],
    ?LOG_NOTICE(#{msg => ~"Stopping listeners"}),
    _ = [stop_listener(Name) || Name <- Names],
    ok.

%% A drain that fails must not keep the node from stopping.
drain(Server, Names, Timeout) ->
    try
        Server:drain_listeners(Names, Timeout)
    catch
        Class:Reason ->
            ?LOG_WARNING(#{msg => ~"Draining the listeners failed, so they are stopped as they are",
                           server => Server, class => Class, reason => Reason}),
            ok
    end.

%% The ip configuration key is an address tuple in practice, but ranch also
%% accepts a hostname, which inet:ntoa/1 would crash on.
format_host(Host) when is_tuple(Host) -> inet:ntoa(Host);
format_host(Host)                     -> Host.

get_version(Application) ->
    case lists:keyfind(Application, 1, application:loaded_applications()) of
        {_, _, Version} ->
            erlang:list_to_binary(Version);
        false ->
            not_found
    end.

%% Recursively resolve nested nova_apps. Each nova_app can declare its own
%% nova_apps dependencies. An application comes before the applications it
%% mounts, and routes are first-wins, so where both declare the same route or
%% status page the mounting application's is kept. An application is mounted
%% once, by the first entry that reaches it. An entry that reaches it again is
%% skipped when it asks for the same security callbacks, and stops the boot
%% when it asks for others, since one of the two would be dropped without a
%% word. An application that mounts itself, directly or through another, is
%% skipped, so a cycle terminates.
-spec resolve_nova_apps([nova_app()]) -> [nova_app()].
resolve_nova_apps(Apps) ->
    {Resolved, _Seen} = resolve_nova_apps(Apps, [], [], []),
    lists:reverse(Resolved).

-spec resolve_nova_apps([nova_app()], [nova_app()], [{atom(), term()}], [atom()]) ->
          {[nova_app()], [{atom(), term()}]}.
resolve_nova_apps([], Acc, Seen, _Around) ->
    {Acc, Seen};
resolve_nova_apps([App | Rest], Acc, Seen, Around) ->
    Name = nova_app_name(App),
    Security = security_in_force(App),
    case lists:keyfind(Name, 1, Seen) of
        false ->
            {Acc0, Seen0} = resolve_nova_apps(nested_nova_apps(App), [App | Acc], [{Name, Security} | Seen],
                                              [Name | Around]),
            resolve_nova_apps(Rest, Acc0, Seen0, Around);
        {Name, Mounted} when Mounted =:= Security ->
            resolve_nova_apps(Rest, Acc, Seen, Around);
        {Name, Mounted} ->
            case lists:member(Name, Around) of
                true ->
                    resolve_nova_apps(Rest, Acc, Seen, Around);
                false ->
                    ?LOG_ERROR(#{msg => ~"Application is mounted by two nova_apps entries that ask for different security callbacks. Mount it once, or give both entries the same secure and override_secure",
                                 app => Name, mounted_with => in_words(Mounted), also_asked_for => in_words(Security)}),
                    throw({error, {nova_app_mounted_twice, Name}})
            end
    end.

%% The callbacks an entry puts in front of the routes of its application:
%% those it inherits from the applications it is mounted through and its
%% own, which are the same wish wherever they are written.
-spec security_in_force(nova_app()) -> {[term()], [term()]}.
security_in_force(App) ->
    Options = nova_app_options(App),
    {[callback(Secure) || Secure <- nova_router:secures_in_force(Options)],
     [callback(Override) || Override <- nova_router:overrides_in_force(Options)]}.

%% {Mod, Fun} and fun Mod:Fun/1 are one callback.
callback({Mod, Fun}) when is_atom(Mod), is_atom(Fun) -> fun Mod:Fun/1;
callback(Other) -> Other.

in_words({Secures, Overrides}) ->
    #{secure => Secures, override_secure => Overrides}.

%% An application that starts on a listener. No entry mounts it, so the
%% options it is compiled with are those of the entry it has written for
%% itself in its own nova_apps, if any. The applications it mounts follow
%% as its nested ones.
-spec with_nova_apps(atom()) -> [nova_app()].
with_nova_apps(App) ->
    case [Entry || Entry <- configured_nova_apps(App), nova_app_name(Entry) =:= App] of
        [Own | _] ->
            notice_own_entry(App, nova_app_options(Own)),
            [Own];
        [] ->
            [App]
    end.

%% For years such an entry was ignored. One with a prefix now moves every
%% route of the application, so it is said when the routes are compiled.
notice_own_entry(App, Options) when map_size(Options) > 0 ->
    ?LOG_NOTICE(#{msg => ~"Application has an entry for itself in its nova_apps, and is compiled with its options",
                  app => App, options => lists:sort(maps:keys(Options))});
notice_own_entry(_App, _Options) ->
    ok.

%% The nova_apps of a mounted application. A secure and an override_secure
%% cover every route the application brings in, so those in force for it
%% are handed on to the applications it mounts. Nothing else is: they are
%% mounted with the options of their own entry.
-spec nested_nova_apps(nova_app()) -> [nova_app()].
nested_nova_apps(App) ->
    Nested = configured_nova_apps(nova_app_name(App)),
    case security_in_force(App) of
        {[], []} ->
            Nested;
        {Secures, Overrides} ->
            [{nova_app_name(NestedApp), (nova_app_options(NestedApp))#{inherited_secure => Secures,
                                                                        inherited_override_secure => Overrides}}
             || NestedApp <- Nested]
    end.

%% The nova_apps of an application as they are configured, which nothing
%% has looked at before this.
-spec configured_nova_apps(atom()) -> [nova_app()].
configured_nova_apps(Name) ->
    case application:get_env(Name, nova_apps, []) of
        Entries when is_list(Entries) -> [nova_app(Entry) || Entry <- Entries]
    end.

-spec nova_app(term()) -> nova_app().
nova_app({Name, Options}) when is_atom(Name), is_map(Options) -> {Name, Options};
nova_app(Name) when is_atom(Name)                             -> Name.

-spec nova_app_options(nova_app()) -> map().
nova_app_options({_App, Options}) -> Options;
nova_app_options(_App)            -> #{}.

%% A nova_app is either the application name or {Name, Options}.
-spec nova_app_name(nova_app()) -> atom().
nova_app_name({App, _Options}) -> App;
nova_app_name(App)             -> App.

%% Without session_exists/1 a presented session id is used as is, so a client
%% can pick its own id.
warn_without_session_exists(SessionManager, ExportedFuns) ->
    case nova_session:enabled() andalso not lists:member({session_exists, 1}, ExportedFuns) of
        true ->
            ?LOG_WARNING(#{msg => ~"Session manager does not implement session_exists/1, so presented session ids are accepted unchecked",
                           session_manager => SessionManager});
        false ->
            ok
    end.
