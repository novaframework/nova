%%% The applications that were added with nova_sup:add_application/2.
%%%
%%% They are kept here, with nova_sup, which outlives the server package:
%%% its listeners come and go with it, and what was added to them is added
%%% again when they are back. One process adds and removes them, one at a
%%% time, so what is kept is what was done.
-module(nova_applications).
-moduledoc false.

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, add/2, remove/1, listening/1, draining/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, format_status/1]).

-type bind() :: {inet:ip_address() | string(), inet:port_number()}.

%% `listener` is the one the application is served on, and `started`
%% whether that was started for it. An application that was added while no
%% listeners were up, and is not served yet, has none.
-record(added, {
                app :: atom(),
                settings :: map(),
                bind :: bind(),
                listener = undefined :: undefined | nova_listener:name(),
                started = false :: boolean()
               }).

%% A listener that was started for an application that is removed since,
%% with others still on it.
-record(kept, {
               name :: nova_listener:name(),
               settings :: map(),
               bind :: bind()
              }).

-record(state, {
                rows = [] :: [#added{} | #kept{}],
                owner = undefined :: undefined | {pid(), reference()}
               }).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(App :: atom(), Settings :: map()) ->
          {ok, App :: atom(), Host :: inet:ip_address() | string(), Port :: inet:port_number()} |
          {error, Reason :: any()}.
add(App, Settings) ->
    call({add, App, Settings}, {error, server_not_started}).

-spec remove(App :: atom()) -> ok | {error, not_found}.
remove(App) ->
    call({remove, App}, {error, not_found}).

%% The listeners of the configuration are started, and `Owner` has them.
-spec listening(Owner :: pid()) -> ok | {error, Reason :: any()}.
listening(Owner) ->
    call({listening, Owner}, {error, server_not_started}).

%% The listeners take no more connections and are about to stop.
-spec draining() -> ok.
draining() ->
    call(draining, ok).

%% A router that adds an application as its own routes are compiled asks
%% the process that is waiting for those.
-spec call(term(), NotStarted) -> dynamic() | NotStarted.
call(Request, NotStarted) ->
    case whereis(?MODULE) =:= self() of
        true -> {error, adding_an_application};
        false -> ask(Request, NotStarted)
    end.

ask(Request, NotStarted) ->
    try gen_server:call(?MODULE, Request, infinity) of
        {raised, Class, Reason, Stacktrace} -> erlang:raise(Class, Reason, Stacktrace);
        Reply -> Reply
    catch
        exit:{noproc, _Call} -> NotStarted;
        %% The call has the settings of the request in it.
        exit:{Reason, _Call} -> exit({Reason, ?MODULE})
    end.

-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

%% What a router raises as its application is added is raised in the
%% caller, as it was when the caller added it itself.
-spec handle_call(term(), gen_server:from(), #state{}) -> {reply, term(), #state{}}.
handle_call(Request, _From, State) ->
    try handle(Request, State) of
        {Reply, NewState} -> {reply, Reply, NewState}
    catch
        Class:Reason:Stacktrace -> {reply, {raised, Class, Reason, Stacktrace}, State}
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

handle({add, App, Settings}, State = #state{owner = undefined, rows = Rows}) ->
    case bind_of(App, Settings, Rows) of
        {ok, {Host, Port} = Bind} ->
            case [Row || Row = #added{app = Added, bind = On} <- Rows, Added =:= App, On =:= Bind] of
                [] ->
                    Row = #added{app = App, settings = Settings, bind = Bind},
                    {{ok, App, Host, Port}, State#state{rows = Rows ++ [Row]}};
                [_Row | _] ->
                    {{error, {already_started, App}}, State}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle({add, App, Settings}, State = #state{rows = Rows}) ->
    Unserved = nova_sup:unserved_groups(),
    case nova_sup:add_to_listener(App, Settings, new) of
        {ok, already, _Name, _Host, _Port} ->
            {{error, {already_started, App}}, State};
        {ok, How, Name, Host, Port} ->
            ok = warn_unserved(App, nova_sup:unserved_groups() -- Unserved),
            Row = #added{app = App, settings = Settings, bind = {Host, Port}, listener = Name,
                         started = How =:= started},
            {{ok, App, Host, Port}, State#state{rows = Rows ++ [Row]}};
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle({remove, App}, State = #state{rows = Rows}) ->
    case {nova_sup:detach_application(App), forget(App, Rows)} of
        {{error, not_found}, Rows} ->
            ?LOG_ERROR(#{msg => ~"Application not found", app => App}),
            {{error, not_found}, State};
        {_Detached, Left} ->
            {ok, State#state{rows = Left}}
    end;
handle({listening, Owner}, State = #state{rows = Rows}) ->
    Configured = nova_sup:unserved_groups(),
    case replay(Rows, [Row || Row = #kept{} <- Rows], [], []) of
        {ok, Replayed} ->
            case nova_sup:check_route_groups(Configured) of
                ok -> {ok, State#state{rows = Replayed, owner = {Owner, monitor(process, Owner)}}};
                {error, Reason} -> {{error, Reason}, State}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end;
handle(draining, State = #state{owner = {_Owner, Monitor}}) ->
    true = demonitor(Monitor, [flush]),
    {ok, State#state{owner = undefined}};
handle(draining, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Before the listeners are up
%%--------------------------------------------------------------------

%% Where the application is going to be served: on a listener of the
%% configuration, or on one that is started as those before it are added.
bind_of(App, Settings, Rows) ->
    case nova_listener:configured() of
        {ok, Configured} ->
            Listeners = [{Name, {Host, Port}} || #{name := Name, ip := Host, port := Port} <- Configured],
            expected_bind(App, Settings, Listeners ++ to_start(Rows, [Bind || {_Name, Bind} <- Listeners]));
        {error, Reason} ->
            {error, Reason}
    end.

expected_bind(_App, #{listener := Name}, Listeners) ->
    case lists:keyfind(Name, 1, Listeners) of
        {Name, Bind} -> {ok, Bind};
        false -> {error, {no_such_listener, Name}}
    end;
expected_bind(App, Settings, Listeners) ->
    case nova_listener:normalize(App, nova_listener:runtime_settings(Settings), #{applications => [App]}) of
        {ok, #{ip := Host, port := Port}} ->
            case lists:keymember({Host, Port}, 2, Listeners) orelse not lists:keymember(App, 1, Listeners) of
                true -> {ok, {Host, Port}};
                false -> {error, {listener_exists, App}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% The listeners that are started as the rows are added, oldest first: one
%% for each address and port that nothing before it is bound to.
to_start([], _Bound) ->
    [];
to_start([#kept{name = Name, bind = Bind} | Rows], Bound) ->
    [{Name, Bind} | to_start(Rows, [Bind | Bound])];
to_start([#added{app = App, settings = Settings, bind = Bind} | Rows], Bound) ->
    case is_map_key(listener, Settings) orelse lists:member(Bind, Bound) of
        true -> to_start(Rows, Bound);
        false -> [{App, Bind} | to_start(Rows, [Bind | Bound])]
    end.

%%--------------------------------------------------------------------
%% As the listeners come up
%%--------------------------------------------------------------------

%% An application that was served before is added to the listener it was
%% on, and as it was the first time: a route group of its that no listener
%% serves is a warning. One that was not served yet is checked as the
%% configuration is, once all are added, since the listener a group names
%% may be one a later application starts.
replay([], _Kept, Replayed, Unserved) ->
    case settle(lists:reverse(Unserved), nova_sup:unserved_groups()) of
        ok -> {ok, lists:reverse(Replayed)};
        {error, Reason} -> {error, Reason}
    end;
replay([#kept{} = Row | Rows], Kept, Replayed, Unserved) ->
    replay(Rows, Kept, [Row | Replayed], Unserved);
replay([#added{app = App, settings = Settings, listener = Listener} = Row | Rows], Kept, Replayed, Unserved) ->
    Before = nova_sup:unserved_groups(),
    try nova_sup:add_to_listener(App, Settings, was(Row, Kept)) of
        {ok, How, Name, Host, Port} ->
            Brought = [{App, Listener =:= undefined, Group} || Group <- nova_sup:unserved_groups() -- Before],
            Served = Row#added{bind = {Host, Port}, listener = Name, started = How =:= started},
            replay(Rows, Kept, [Served | Replayed], lists:reverse(Brought, Unserved));
        {error, Reason} ->
            not_added(App, Reason)
    catch
        throw:{error, Reason} ->
            not_added(App, Reason)
    end.

%% A listener is started for an application that was not served yet,
%% and for one that had its own. One that was on the listener of another
%% is not given one of its own when that listener is gone: its settings
%% are an address, and not how it is to be served there.
was(#added{listener = undefined}, _Kept) ->
    new;
was(#added{started = true}, _Kept) ->
    new;
was(#added{settings = #{listener := Name}}, Kept) ->
    case [Row || Row = #kept{name = Named} <- Kept, Named =:= Name] of
        [#kept{settings = Settings, bind = Bind} | _] -> {kept, Name, Settings, Bind};
        [] -> {on, Name}
    end;
was(#added{bind = Bind, listener = Name}, Kept) ->
    case [Row || Row = #kept{bind = On} <- Kept, On =:= Bind] of
        [#kept{name = Named, settings = Settings} | _] -> {kept, Named, Settings, Bind};
        [] -> {on, Name}
    end.

settle([], _Unserved) ->
    ok;
settle([{App, Early, {GroupApp, Prefix, Listener} = Group} | Rest], Unserved) ->
    case lists:member(Group, Unserved) of
        false ->
            settle(Rest, Unserved);
        true when Early ->
            not_added(App, {route_group_not_served, GroupApp, Prefix, Listener});
        true ->
            ok = warn_unserved(App, [Group]),
            settle(Rest, Unserved)
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

%%--------------------------------------------------------------------
%% As an application is removed
%%--------------------------------------------------------------------

%% A listener that was started for the application is still that of the
%% others on it. How it was started is kept under its name for as long as
%% one of them is, so that it is the same listener when it is started again.
forget(App, Rows) ->
    Left = lists:append([without(App, Row) || Row <- Rows]),
    [Row || Row <- Left, in_use(Row, Left)].

without(App, #added{app = App, started = true, settings = Settings, bind = Bind}) ->
    [#kept{name = App, settings = Settings, bind = Bind}];
without(App, #added{app = App}) ->
    [];
without(_App, Row) ->
    [Row].

in_use(#kept{bind = Bind}, Rows) ->
    [] =/= [App || #added{app = App, bind = On} <- Rows, On =:= Bind];
in_use(#added{}, _Rows) ->
    true.
