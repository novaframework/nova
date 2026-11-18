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
-define(STARTED, {?MODULE, started}).
-define(DRAINING_SLACK, 5000).
-define(MAX_TIMER, 16#FFFFFFFF).

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
%% when the one it was started for is removed. The settings of one that
%% was found running by this process as it was started again are lost.
-record(started, {
                  name :: nova_listener:name(),
                  settings :: map() | lost,
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

%% The listeners take no more connections and are about to stop. The
%% application that is being added by then is waited for as long as the
%% connections of the listeners are, and is not added after that: what its
%% router waits for is not waited for. One that is added from here on is
%% kept for the next start, also when it asked before this did and waited
%% behind the one that was being added.
%%
%% It is not a call, which the process would come to after the
%% applications that wait: it has to know while it waits for a router.
-spec draining() -> ok.
draining() ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Registry when is_pid(Registry) ->
            draining(Registry, nova_settings:current(shutdown_drain_timeout))
    end.

draining(Registry, Timeout) when is_integer(Timeout) ->
    Alias = monitor(process, Registry, [{alias, reply_demonitor}]),
    Registry ! {draining, Alias, Timeout},
    receive
        {Alias, drained} ->
            ok;
        {'DOWN', Alias, process, Registry, _Reason} ->
            ok
    after min(Timeout + ?DRAINING_SLACK, ?MAX_TIMER) ->
            true = demonitor(Alias, [flush]),
            ?LOG_WARNING(#{msg => ~"The listeners are stopped without the process that keeps the added applications knowing of it",
                           timeout_ms => Timeout}),
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
                %% The call has the settings of the request in it.
                exit:{Reason, _Call} -> exit({Reason, ?MODULE})
            end
    end.

%% The listeners outlive this process, and when it is started again they
%% are still there, with the applications that were added to them.
-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    %% Started again is under the supervisor it was under before. A start
    %% of `nova` that failed had another.
    Supervisor = whereis(nova_sup),
    Again = is_pid(Supervisor) andalso persistent_term:get(?STARTED, undefined) =:= Supervisor,
    ok = persistent_term:put(?STARTED, Supervisor),
    case nova_sup:listeners_owner() of
        undefined when Again ->
            %% Nothing says what was added while no listeners are up.
            ?LOG_ERROR(#{msg => ~"The process that keeps the applications added with nova_sup:add_application/2 was started again while no listeners were up. The applications that were added are not kept, and are not served when the server package starts: add them again"}),
            {ok, #state{}};
        undefined ->
            {ok, #state{}};
        Owner ->
            {ok, recovered(Owner)}
    end.

%% The listeners say which applications were added to them. How one was
%% started for an application went with the process, so it is not started
%% again, nor is another of its name taken for it: the applications on it
%% are refused when the listeners are back, and the server package does
%% not start.
recovered(Owner) ->
    ok = nova_sup:drop_unbound(),
    Added = nova_sup:added(),
    Rows = [#added{app = App, settings = #{ip => Host, port => Port}, listener = Name, bind = {Host, Port},
                   served = true}
            || {Name, {Host, Port}, Apps, _ForApplication} <- Added,
               App <- Apps],
    Lost = [#started{name = Name, settings = lost, bind = Bind} || {Name, Bind, _Apps, true} <- Added],
    ?LOG_ERROR(#{msg => ~"The process that keeps the applications added with nova_sup:add_application/2 was started again. They are served. A listener that was started for one cannot be started again as it was, so the server package will not start again before the applications on it are removed",
                 applications => [App || #added{app = App} <- Rows],
                 listeners => [Name || #started{name = Name} <- Lost]}),
    #state{rows = Rows, started = Lost, owner = {Owner, monitor(process, Owner)}}.

%% What a router raises as its application is added is raised in the
%% caller, as it was when the caller added it itself.
-spec handle_call(term(), gen_server:from(), #state{}) -> {reply, term(), #state{}}.
handle_call(Request, _From, State) ->
    {Answer, Drainers} = answer(Request, State),
    {Reply, Kept} = case Answer of
                        {raised, Class, Reason, Stacktrace} -> {{raised, Class, Reason, Stacktrace}, State};
                        {Answered, NewState} -> {Answered, watching(Request, Answered, NewState)}
                    end,
    {reply, Reply, drained(Drainers, Kept)}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', Monitor, process, Owner, _Reason}, State = #state{owner = {Owner, Monitor}}) ->
    {noreply, State#state{owner = undefined}};
handle_info({draining, Drainer, _Timeout}, State) ->
    {noreply, drained([Drainer], State)};
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
watching(_Request, _Reply, State) ->
    State.

%% Those that stop the listeners are told once no application is being
%% added to them, and none is from here on.
drained([], State) ->
    State;
drained(Drainers, State) ->
    _ = [Drainer ! {Drainer, drained} || Drainer <- Drainers],
    unwatched(State).

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
%% is not with the one that keeps the applications. It goes with this one,
%% so it adds nothing that is not kept.
answer(Request, Kept) ->
    State = current(Kept),
    case compiles_routes(Request, State) of
        true -> in_worker(Request, State);
        false -> {handled(Request, State), []}
    end.

%% The owner of the listeners can be gone before its 'DOWN' is here, and
%% the listeners with it.
current(State = #state{owner = {Owner, _Monitor}}) ->
    case nova_sup:listeners_owner() of
        Owner -> State;
        _Gone -> unwatched(State)
    end;
current(State) ->
    State.

compiles_routes({add, _App, _Settings}, #state{owner = Owner}) -> Owner =/= undefined;
compiles_routes({listening, _Owner}, _State) -> true;
compiles_routes(_Request, _State) -> false.

in_worker(Request, State) ->
    Tag = make_ref(),
    {Worker, Monitor} = spawn_opt(?MODULE, work, [self(), Tag, Request, State], [link, monitor]),
    worked({Worker, Monitor, Tag}, []).

%% The listeners can begin to stop as the router runs. It is given the
%% time their connections are given, once, however many ask meanwhile.
worked({Worker, Monitor, Tag} = Work, Drainers) ->
    receive
        {Tag, Answer} ->
            true = demonitor(Monitor, [flush]),
            {Answer, Drainers};
        {'DOWN', Monitor, process, Worker, Reason} ->
            %% What a router links to can go down with the settings of a
            %% listener as its reason.
            {{raised, exit, nova_redact:tls_options(Reason), []}, Drainers};
        {draining, Drainer, Timeout} ->
            ok = overdue_after(Timeout, Tag, Drainers),
            worked(Work, [Drainer | Drainers]);
        {Tag, overdue, Timeout} ->
            ?LOG_WARNING(#{msg => ~"An application was still being added as the listeners stop, and is not added",
                           app => adding_app(Worker), timeout_ms => Timeout}),
            exit(Worker, kill),
            worked(Work, Drainers)
    end.

overdue_after(Timeout, Tag, []) when is_integer(Timeout), Timeout >= 0 ->
    _ = erlang:send_after(Timeout, self(), {Tag, overdue, Timeout}),
    ok;
overdue_after(_Timeout, _Tag, _Drainers) ->
    ok.

-spec work(pid(), reference(), term(), #state{}) -> ok.
work(Registry, Tag, Request, State) ->
    put(?ADDING, true),
    Registry ! {Tag, handled(Request, State)},
    ok.

adding(App) ->
    proc_lib:set_label({?MODULE, App}).

adding_app(Worker) ->
    case proc_lib:get_label(Worker) of
        {?MODULE, App} -> App;
        _None -> undefined
    end.

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
            case place(App, Settings, Listeners, State) of
                {on, Name, Bind} -> with_row(App, Settings, Name, Bind, false, [], State);
                {start, Bind} -> with_row(App, Settings, App, Bind, false, [started(App, Settings, Bind)], State);
                {served, {Host, Port}} -> {{ok, App, Host, Port}, State};
                {error, Reason} -> {{error, Reason}, State}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle({add, App, Settings}, State) ->
    ok = adding(App),
    Unserved = nova_sup:unserved_groups(),
    case put_on(App, Settings, place(App, Settings, nova_sup:running(), State)) of
        {ok, Name, Bind, Started} ->
            ok = warn_unserved(App, nova_sup:unserved_groups() -- Unserved),
            with_row(App, Settings, Name, Bind, true, Started, State);
        {served, {Host, Port}} ->
            {{ok, App, Host, Port}, State};
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
place(App, #{listener := Name} = Settings, Listeners, State) ->
    case lists:keyfind(Name, 1, Listeners) of
        {Name, Bind, Apps} -> on_listener(App, Name, Bind, Apps, maps:without([listener], Settings), State);
        false -> {error, {no_such_listener, Name}}
    end;
place(App, Settings, Listeners, State) ->
    case nova_listener:normalize(App, nova_listener:runtime_settings(Settings), #{applications => [App]}) of
        {ok, #{port := 0}} ->
            %% Which port that would be is not known here, and the next to
            %% ask for one would be put on the same listener.
            {error, {invalid_listener_setting, App, port, 0}};
        {ok, #{ip := Host, port := Port}} ->
            case {lists:keyfind({Host, Port}, 2, Listeners), lists:keymember(App, 1, Listeners)} of
                {{Name, Bind, Apps}, _Named} ->
                    on_listener(App, Name, Bind, Apps, maps:without([ip, port], Settings), State);
                {false, true} ->
                    {error, {listener_exists, App}};
                {false, false} ->
                    {start, {Host, Port}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% A listener that is there serves as it was started. Settings that ask it
%% for something, TLS or steps of its own say, would not be applied, so
%% an application that comes with them is not put on it. The settings a
%% listener was started with for an application are those it has.
%%
%% An application that is on the listener and was not added to it is one
%% the configuration lists there. It is served where it asks to be, as a
%% library that adds itself as it starts may find, and is not kept as
%% added. Adding one a second time is what is refused.
on_listener(App, Name, Bind, Apps, Asked, #state{rows = Rows, started = Started}) ->
    case {applied(Name, Asked, Started), lists:member(App, apps_on(Name, Rows)), lists:member(App, Apps)} of
        {false, _Added, _On} -> {error, {listener_settings_ignored, Name, lists:sort(maps:keys(Asked))}};
        {true, true, _On} -> {error, {already_started, App}};
        {true, false, true} -> {served, Bind};
        {true, false, false} -> {on, Name, Bind}
    end.

applied(_Name, Asked, _Started) when map_size(Asked) =:= 0 ->
    true;
applied(Name, Asked, Started) ->
    case lists:keyfind(Name, #started.name, Started) of
        #started{settings = With} when is_map(With) -> maps:without([ip, port], With) =:= Asked;
        _NotStartedWithAny -> false
    end.

%% The listeners there are going to be: those of the configuration, and
%% those that are started for an application.
expected(#state{rows = Rows, started = Started}) ->
    case nova_listener:configured() of
        {ok, Configured} ->
            {ok, [{Name, {Host, Port}, Apps ++ apps_on(Name, Rows)}
                  || #{name := Name, ip := Host, port := Port, applications := Apps} <- Configured]
                 ++ [{Name, Bind, apps_on(Name, Rows)} || #started{name = Name, bind = Bind} <- Started]};
        {error, Reason} ->
            %% What is not valid can be the TLS options of a listener.
            {error, nova_redact:tls_options(Reason)}
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
put_on(_App, _Settings, {served, Bind}) ->
    {served, Bind};
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
    ok = adding(App),
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
        #started{settings = lost} ->
            {error, {no_such_listener, Name}};
        #started{settings = With} ->
            case lists:member(Name, Up) of
                true -> nova_sup:attach_to_listener(App, Name);
                false -> nova_sup:start_listener_for(Name, App, With)
            end;
        false ->
            case lists:keyfind(Name, 1, nova_sup:running()) of
                {Name, On, _Apps} when On =:= Bind; is_map_key(listener, Settings) ->
                    nova_sup:attach_to_listener(App, Name);
                {Name, On, _Apps} ->
                    ?LOG_NOTICE(#{msg => ~"A listener is no longer at the address an application was added to it by",
                                  app => App, listener => Name, added_at => Bind, listener_at => On}),
                    {error, {no_such_listener, Name}};
                false ->
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
