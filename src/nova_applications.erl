%%% The applications that were added with nova_sup:add_application/2.
%%%
%%% They are kept here, with nova_sup, which outlives the server package:
%%% its listeners come and go with it, and what was added to them is added
%%% again when they are back. One process adds and removes them, one at a
%%% time, so what is kept is what was done.
%%%
%%% Each application is kept with the name of the listener it is on, and
%%% each listener that was started for one with how it was started. Where
%%% an application goes is decided the same way before the listeners are up
%%% and after, and when they are back it goes where it was.
-module(nova_applications).
-moduledoc false.

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, add/2, remove/1, listening/1, draining/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, format_status/1]).
-export([work/4]).

-define(ADDING, nova_adding_application).

-type bind() :: {inet:ip_address() | string(), inet:port_number()}.

%% `served` is whether the application was on its listener yet. One that
%% was added while no listeners were up is not, until they are.
-record(added, {
                app :: atom(),
                settings :: map(),
                listener :: nova_listener:name(),
                bind :: bind(),
                served = false :: boolean()
               }).

%% A listener that is started for an application, under its name and with
%% its settings. It is there for as long as an application is on it, also
%% when the one it was started for is removed.
-record(started, {
                  name :: nova_listener:name(),
                  settings :: map(),
                  bind :: bind()
                 }).

-record(state, {
                rows = [] :: [#added{}],
                started = [] :: [#started{}],
                owner = undefined :: undefined | {pid(), reference()}
               }).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(App :: atom(), Settings :: map()) ->
          {ok, App :: atom(), Host :: inet:ip_address() | string(), Port :: inet:port_number()} |
          {error, Reason :: any()}.
add(App, Settings) ->
    case call({add, App, Settings}, infinity) of
        {ok, App, Host, Port} -> {ok, App, Host, Port};
        {error, Reason} -> {error, Reason};
        not_started -> {error, server_not_started}
    end.

-spec remove(App :: atom()) -> ok | {error, not_found | adding_an_application}.
remove(App) ->
    case call({remove, App}, infinity) of
        ok -> ok;
        {error, not_found} -> {error, not_found};
        {error, adding_an_application} -> {error, adding_an_application};
        not_started -> {error, not_found}
    end.

%% The listeners of the configuration are started, and `Owner` has them.
-spec listening(Owner :: pid()) -> ok | {error, Reason :: any()}.
listening(Owner) ->
    case call({listening, Owner}, infinity) of
        ok -> ok;
        {error, Reason} -> {error, Reason};
        not_started -> {error, server_not_started}
    end.

%% The listeners take no more connections and are about to stop. An
%% application that is still being added does not keep them from it.
-spec draining() -> ok.
draining() ->
    Timeout = application:get_env(nova, shutdown_drain_timeout, 15000),
    case call(draining, Timeout) of
        timeout ->
            ?LOG_WARNING(#{msg => ~"An application is still being added, so the listeners are stopped without waiting for it",
                           timeout_ms => Timeout}),
            ok;
        _Told ->
            ok
    end.

%% A router that adds an application as its own routes are compiled asks
%% the process that is waiting for those.
-spec call(term(), timeout()) -> dynamic().
call(Request, Timeout) ->
    case get(?ADDING) of
        true ->
            {error, adding_an_application};
        _Not ->
            try gen_server:call(?MODULE, Request, Timeout) of
                {raised, Class, Reason, Stacktrace} -> erlang:raise(Class, Reason, Stacktrace);
                Reply -> Reply
            catch
                exit:{noproc, _Call} -> not_started;
                exit:{timeout, _Call} -> timeout;
                %% The call has the settings of the request in it.
                exit:{Reason, _Call} -> exit({Reason, ?MODULE})
            end
    end.

%% The listeners outlive this process, and when it is started again they
%% are still there, with the applications that were added to them.
-spec init([]) -> {ok, #state{}}.
init([]) ->
    case nova_sup:listeners_owner() of
        undefined -> {ok, #state{}};
        Owner -> {ok, recovered(Owner)}
    end.

%% How a listener was started for an application went with the process,
%% so one is not started for it again: a row without it is refused when the
%% listeners are back, and the server package does not start.
recovered(Owner) ->
    Listed = listed(),
    Running = nova_sup:running(),
    Rows = [#added{app = App, settings = #{ip => Host, port => Port}, listener = Name, bind = {Host, Port},
                   served = true}
            || {Name, {Host, Port}, Apps} <- Running,
               App <- Apps,
               not lists:member(App, proplists:get_value(Name, Listed, []))],
    ?LOG_ERROR(#{msg => ~"The process that keeps the applications added with nova_sup:add_application/2 was started again. They are served. A listener that was started for one cannot be started again as it was, so the server package will not start again before the applications on it are removed",
                 applications => [App || #added{app = App} <- Rows],
                 listeners => [Name || {Name, _Bind, _Apps} <- Running, not lists:keymember(Name, 1, Listed)]}),
    #state{rows = Rows, owner = {Owner, monitor(process, Owner)}}.

listed() ->
    case nova_listener:configured() of
        {ok, Listeners} -> [{Name, Apps} || #{name := Name, applications := Apps} <- Listeners];
        {error, _Reason} -> []
    end.

%% What a router raises as its application is added is raised in the
%% caller, as it was when the caller added it itself.
-spec handle_call(term(), gen_server:from(), #state{}) -> {reply, term(), #state{}}.
handle_call(Request, _From, State) ->
    case answer(Request, State) of
        {raised, Class, Reason, Stacktrace} -> {reply, {raised, Class, Reason, Stacktrace}, State};
        {Reply, NewState} -> {reply, Reply, watching(Request, Reply, NewState)}
    end.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', Monitor, process, Owner, _Reason}, State = #state{owner = {Owner, Monitor}}) ->
    {noreply, State#state{owner = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% The settings of a listener have its TLS options in them, and so have
%% the state and the request a crash is reported with. The events of a
%% debugged process have every request in them, and are left out.
-spec format_status(gen_server:format_status()) -> gen_server:format_status().
format_status(Status) ->
    Safe = #{Key => nova_redact:tls_options(Value) || Key := Value <- maps:without([log], Status)},
    case Status of
        #{log := _Events} -> Safe#{log => []};
        #{} -> Safe
    end.

watching({listening, Owner}, ok, State) ->
    Unwatched = unwatched(State),
    Unwatched#state{owner = {Owner, monitor(process, Owner)}};
watching(draining, ok, State) ->
    unwatched(State);
watching(_Request, _Reply, State) ->
    State.

unwatched(State = #state{owner = {_Owner, Monitor}}) ->
    true = demonitor(Monitor, [flush]),
    State#state{owner = undefined};
unwatched(State) ->
    State.

%%--------------------------------------------------------------------
%% The code of an application
%%--------------------------------------------------------------------

%% The routes of an application are compiled by its own router. That runs
%% in a process of its own, so what it links to, receives or leaves behind
%% is not with the one that keeps the applications.
answer(Request, State) ->
    case compiles_routes(Request, State) of
        true -> in_worker(Request, State);
        false -> handled(Request, State)
    end.

compiles_routes({add, _App, _Settings}, #state{owner = Owner}) -> Owner =/= undefined;
compiles_routes({listening, _Owner}, _State) -> true;
compiles_routes(_Request, _State) -> false.

in_worker(Request, State) ->
    Tag = make_ref(),
    {Worker, Monitor} = spawn_monitor(?MODULE, work, [self(), Tag, Request, State]),
    receive
        {Tag, Answer} ->
            true = demonitor(Monitor, [flush]),
            Answer;
        {'DOWN', Monitor, process, Worker, Reason} ->
            {raised, exit, Reason, []}
    end.

-spec work(pid(), reference(), term(), #state{}) -> ok.
work(Registry, Tag, Request, State) ->
    put(?ADDING, true),
    Registry ! {Tag, handled(Request, State)},
    ok.

handled(Request, State) ->
    try
        handle(Request, State)
    catch
        Class:Reason:Stacktrace ->
            {raised, Class, nova_redact:tls_options(Reason), nova_redact:stacktrace(Stacktrace)}
    end.

handle({add, App, Settings}, State = #state{owner = undefined}) ->
    case expected(State) of
        {ok, Listeners} ->
            case place(App, Settings, Listeners) of
                {on, Name, Bind} -> with_row(App, Settings, Name, Bind, false, [], State);
                {start, Bind} -> with_row(App, Settings, App, Bind, false, [started(App, Settings, Bind)], State);
                {error, Reason} -> {{error, Reason}, State}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle({add, App, Settings}, State) ->
    Unserved = nova_sup:unserved_groups(),
    case put_on(App, Settings, place(App, Settings, nova_sup:running())) of
        {ok, Name, Bind, Started} ->
            ok = warn_unserved(App, nova_sup:unserved_groups() -- Unserved),
            with_row(App, Settings, Name, Bind, true, Started, State);
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle({remove, App}, State = #state{rows = Rows, started = Started}) ->
    Left = [Row || Row = #added{app = Added} <- Rows, Added =/= App],
    case {nova_sup:detach_application(App), Left} of
        {{error, not_found}, Rows} ->
            ?LOG_ERROR(#{msg => ~"Application not found", app => App}),
            {{error, not_found}, State};
        {_Detached, Left} ->
            InUse = [Listener || Listener = #started{name = Name} <- Started,
                                 lists:keymember(Name, #added.listener, Left)],
            {ok, State#state{rows = Left, started = InUse}}
    end;
handle({listening, _Owner}, State = #state{rows = Rows, started = Started}) ->
    Configured = nova_sup:unserved_groups(),
    case replay(Rows, Started, [], [], [], Configured) of
        {ok, Replayed} ->
            case nova_sup:check_route_groups(Configured) of
                ok -> {ok, State#state{rows = Replayed}};
                {error, Reason} -> {{error, Reason}, State}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle(draining, State) ->
    {ok, State};
handle(_Unknown, State) ->
    {{error, unknown_request}, State}.

with_row(App, Settings, Name, {Host, Port} = Bind, Served, Started, State = #state{rows = Rows, started = Before}) ->
    Row = #added{app = App, settings = Settings, listener = Name, bind = Bind, served = Served},
    {{ok, App, Host, Port}, State#state{rows = Rows ++ [Row], started = Before ++ Started}}.

started(Name, Settings, Bind) ->
    #started{name = Name, settings = Settings, bind = Bind}.

%%--------------------------------------------------------------------
%% Where an application goes
%%--------------------------------------------------------------------

%% On the listener it names, or on the one that is bound to the address of
%% its settings. When none is, one is started for it under its own name,
%% which no other listener may have. `Listeners` are those that are up, or
%% those that are going to be.
place(App, #{listener := Name}, Listeners) ->
    case lists:keyfind(Name, 1, Listeners) of
        {Name, Bind, Apps} -> on_listener(App, Name, Bind, Apps);
        false -> {error, {no_such_listener, Name}}
    end;
place(App, Settings, Listeners) ->
    case nova_listener:normalize(App, nova_listener:runtime_settings(Settings), #{applications => [App]}) of
        {ok, #{ip := Host, port := Port}} ->
            case {lists:keyfind({Host, Port}, 2, Listeners), lists:keymember(App, 1, Listeners)} of
                {{Name, Bind, Apps}, _Named} -> on_listener(App, Name, Bind, Apps);
                {false, true} -> {error, {listener_exists, App}};
                {false, false} -> {start, {Host, Port}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

on_listener(App, Name, Bind, Apps) ->
    case lists:member(App, Apps) of
        true -> {error, {already_started, App}};
        false -> {on, Name, Bind}
    end.

%% The listeners there are going to be: those of the configuration, and
%% those that are started for an application. A library that adds itself
%% as it starts may be listed by the configuration as well, which is not
%% adding it twice.
expected(#state{rows = Rows, started = Started}) ->
    case nova_listener:configured() of
        {ok, Configured} ->
            {ok, [{Name, {Host, Port}, apps_on(Name, Rows)}
                  || #{name := Name, ip := Host, port := Port} <- Configured]
                 ++ [{Name, Bind, apps_on(Name, Rows)} || #started{name = Name, bind = Bind} <- Started]};
        {error, Reason} ->
            {error, Reason}
    end.

apps_on(Name, Rows) ->
    [App || #added{app = App, listener = On} <- Rows, On =:= Name].

put_on(App, _Settings, {on, Name, _Bind}) ->
    case nova_sup:attach_to_listener(App, Name) of
        {ok, Host, Port} -> {ok, Name, {Host, Port}, []};
        {error, Reason} -> {error, Reason}
    end;
put_on(App, Settings, {start, _Bind}) ->
    case nova_sup:start_listener_for(App, App, Settings) of
        {ok, Host, Port} -> {ok, App, {Host, Port}, [started(App, Settings, {Host, Port})]};
        {error, Reason} -> {error, Reason}
    end;
put_on(_App, _Settings, {error, Reason}) ->
    {error, Reason}.

%%--------------------------------------------------------------------
%% As the listeners come up
%%--------------------------------------------------------------------

%% An application that was served before is added as it was the first
%% time: a route group of its that no listener serves is a warning. One
%% that was not served yet is checked as the configuration is, once all are
%% added, since the listener a group names may be one a later application
%% starts. `Up` are the listeners that are started so far, and `Before`
%% the groups no listener served before the application at hand.
replay([], _Started, _Up, Replayed, Brought, Unserved) ->
    case check_brought(lists:reverse(Brought), Unserved) of
        ok -> {ok, lists:reverse(Replayed)};
        {error, Reason} -> {error, Reason}
    end;
replay([#added{app = App, listener = Name, served = Served} = Row | Rows], Started, Up, Replayed, Brought, Before) ->
    try back_on(Row, Started, Up) of
        {ok, Host, Port} ->
            Unserved = nova_sup:unserved_groups(),
            New = [{App, Served, Group} || Group <- Unserved -- Before],
            Back = Row#added{bind = {Host, Port}, served = true},
            replay(Rows, Started, [Name | Up], [Back | Replayed], lists:reverse(New, Brought), Unserved);
        {error, Reason} ->
            not_added(App, Reason)
    catch
        throw:{error, Reason} ->
            not_added(App, Reason)
    end.

%% An application goes back on the listener it was on. One that was started
%% for an application is started as it was, by the first that is on it. A
%% listener of the configuration is where an application that named it
%% goes, wherever it is bound now. One that was added to it by its address
%% asked for that address, and is not served elsewhere when the listener
%% is gone from it.
back_on(#added{app = App, listener = Name, settings = Settings, bind = Bind}, Started, Up) ->
    case lists:keyfind(Name, #started.name, Started) of
        #started{settings = With} ->
            case lists:member(Name, Up) of
                true -> nova_sup:attach_to_listener(App, Name);
                false -> nova_sup:start_listener_for(Name, App, With)
            end;
        false ->
            case lists:keyfind(Name, 1, nova_sup:running()) of
                {Name, On, _Apps} when On =:= Bind; is_map_key(listener, Settings) ->
                    nova_sup:attach_to_listener(App, Name);
                _Elsewhere ->
                    {error, {no_such_listener, Name}}
            end
    end.

check_brought([], _Unserved) ->
    ok;
check_brought([{App, Served, {GroupApp, Prefix, Listener} = Group} | Rest], Unserved) ->
    case lists:member(Group, Unserved) of
        false ->
            check_brought(Rest, Unserved);
        true when Served ->
            ok = warn_unserved(App, [Group]),
            check_brought(Rest, Unserved);
        true ->
            not_added(App, {route_group_not_served, GroupApp, Prefix, Listener})
    end.

not_added(App, NotAdded) ->
    Reason = nova_redact:tls_options(NotAdded),
    ?LOG_ERROR(#{msg => ~"An application that was added with nova_sup:add_application/2 cannot be served",
                 app => App, reason => Reason}),
    {error, {application_not_added, App, Reason}}.

%% At boot a route group that no listener it names serves stops the node.
%% At runtime the listener it names may still be to come, so the groups
%% the application brought, and no listener they name serves, are said.
warn_unserved(App, Groups) ->
    _ = [?LOG_WARNING(#{msg => ~"A route group names a listener that is not started or does not serve its application, so the group is not served there",
                        app => GroupApp, prefix => Prefix, listener => Listener, added => App})
         || {GroupApp, Prefix, Listener} <- Groups],
    ok.
