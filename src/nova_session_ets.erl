%%%-------------------------------------------------------------------
%%% Copyright (C) 2019, Niclas Axelsson
%%% Created :  8 Dec 2019 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_session_ets).
-moduledoc """
The default session backend. Sessions live in an ETS table on every node and
are replicated through `nova_pubsub`.

A session is dropped `session_max_age` seconds (14 days by default) after it
was created, whatever happens to it in between. With
`{nova, [{session_max_age, infinity}]}` sessions live until they are deleted
or the node restarts. Expired sessions are swept once a minute; a session
created while the setting was `infinity` never expires.

The table holds at most `max_sessions` sessions, 1 000 000 by default. Once
it is full, a write that starts a new session drops another one to make room:
an expired session first, then a provisional one, then the session nearest its
expiry. Sessions that exist are still written. Set
`{nova, [{max_sessions, infinity}]}` to turn the limit off. Nova does not
start with a value that is neither `infinity` nor an integer above 0. The
setting is read from the `nova` environment when Nova starts.

A provisional session is one that only holds what was written with
`nova_session:set_provisional/3`, which is how `nova_csrf_plugin` stores the
token of a visitor. It is dropped after a day, or after `session_max_age`
when that is shorter, and it is the first to go when the table is full, so
requests without a cookie cannot fill the table and keep clients from signing
in or sign anybody out. The first `nova_session:set/3` makes it an ordinary
session. A provisional write that would start a session when every session in
a full table is an ordinary one returns `{error, session_store_full}`.

A session that was deleted, which is what signing out and
`nova_session:renew/1` do, stays deleted: the id is remembered for five
minutes, on every node, and a write to it in that time stores nothing and
returns `{error, session_ended}`. A request that was on its way when its
client signed out so cannot bring the session back under an id that somebody
else may hold. `nova_session` stores what such a request writes in a session
of its own. At most as many ended ids are remembered as the table may hold
sessions (1 000 000 when `max_sessions` is `infinity`), and the one that
ended first is forgotten first. A request that takes longer than five
minutes is not covered.

Sessions do not survive a restart, and a node that starts or joins later only
has the sessions written after it joined. During a rolling deploy or behind a
load balancer without sticky sessions, a client whose id the serving node does
not have gets a fresh session (Nova replaces ids the backend does not know),
so it is signed out. Use a shared backend, such as a database, when sessions
have to outlive a node.

Rows are `{SessionId, Data, ExpiresAt, Class}`. Older releases stored
`{SessionId, Data}` or `{SessionId, Data, ExpiresAt}`; `code_change/3`
converts both on a hot upgrade, to ordinary sessions.
""".

-behaviour(gen_server).
-behaviour(nova_session).

%% API
-export([
         start_link/0,
         get_value/2,
         set_value/3,
         set_provisional_value/3,
         delete_value/1,
         delete_value/2,
         session_exists/1,
         session_ended/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-define(SERVER, ?MODULE).
-define(TABLE, nova_session_ets_entries).
%% {Rank, ExpiresAt, SessionId} for every row, so the session to drop from a
%% full table and the expired ones are found without walking the table.
-define(EXPIRY, nova_session_ets_expiry).
%% {SessionId, Until, Order} for every id that was deleted a while ago, and
%% {Order, SessionId, Until} to find the one that was deleted first. Until
%% is in monotonic time, which the node that owns the tables does not see
%% jump, and Order a number that grows with every id that ends.
-define(ENDED, nova_session_ets_ended).
-define(ENDED_ORDER, nova_session_ets_ended_order).
-define(ENDED_FOR, 300000).
-define(MAX_ENDED, 1000000).
-define(CHANNEL, '__sessions').
-define(SWEEP_INTERVAL, 60000).
-define(PROVISIONAL_MAX_AGE, 86400).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_pubsub.hrl").

-record(state, {warned_at :: integer() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
-doc "Starts the process that owns the session table. `nova_sup` calls it.".
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Read straight from the table, so reads do not queue behind the writes.
-doc """
Returns `{ok, Value}` for `Key` in the session, or `{error, not_found}` when
the session or the key does not exist or the session has expired.
""".
-spec get_value(SessionId :: binary(), Key :: binary()) -> {ok, Value :: term()} | {error, not_found}.
get_value(SessionId, Key) ->
    try live_session(SessionId) of
        {ok, #{Key := Value}, _ExpiresAt, _Class} -> {ok, Value};
        _ -> {error, not_found}
    catch
        error:badarg -> {error, not_found}
    end.

%% Every write waits for the local copy, so the next request on this node,
%% which may arrive as soon as the response is out, already sees it.
-doc """
Stores `Value` under `Key` in the session, on every node, and creates the
session when it does not exist. A provisional session becomes an ordinary
one. In a full table a new session takes the place of another one, as the
module documentation describes. Returns `{error, session_store_full}` when
the session is not in the table of this node after the write.
""".
-spec set_value(SessionId :: binary(), Key :: binary(), Value :: term()) ->
          ok | {error, session_store_full | session_ended}.
set_value(SessionId, Key, Value) ->
    write(kept, "set_value", SessionId, Key, Value).

-doc """
Stores `Value` under `Key` like `set_value/3`, and creates a provisional
session when the session does not exist. A session that exists keeps its
kind. In a full table a new provisional session takes the place of an
expired or another provisional one. Returns `{error, session_store_full}`
when there is none of those to drop.
""".
-spec set_provisional_value(SessionId :: binary(), Key :: binary(), Value :: term()) ->
          ok | {error, session_store_full | session_ended}.
set_provisional_value(SessionId, Key, Value) ->
    write(provisional, "set_provisional_value", SessionId, Key, Value).

%% The session is looked up after the write: another node's session may have
%% taken the room that was made for this one, and the session may have been
%% deleted while the write was on its way to the server, which then stored
%% nothing. An id that has ended is not asked room for, nor sent on.
write(Class, Topic, SessionId, Key, Value) ->
    case not session_ended(SessionId) andalso has_room(SessionId, Class) of
        true ->
            nova_pubsub:broadcast(?CHANNEL, Topic, {SessionId, Key, Value}),
            sync(),
            written(SessionId);
        false ->
            written(SessionId)
    end.

-spec written(SessionId :: binary()) -> ok | {error, session_store_full | session_ended}.
written(SessionId) ->
    case session_exists(SessionId) of
        true ->
            ok;
        false ->
            case session_ended(SessionId) of
                true -> {error, session_ended};
                false -> {error, session_store_full}
            end
    end.

-doc "Deletes the session on every node.".
-spec delete_value(SessionId :: binary()) -> ok.
delete_value(SessionId) ->
    nova_pubsub:broadcast(?CHANNEL, "delete_value", SessionId),
    sync().

-doc "Deletes `Key` from the session on every node.".
-spec delete_value(SessionId :: binary(), Key :: binary()) -> ok.
delete_value(SessionId, Key) ->
    nova_pubsub:broadcast(?CHANNEL, "delete_value", {SessionId, Key}),
    sync().

%% Read straight from the table: nova_session calls this on every access by a
%% request that carries a session cookie.
-doc "Tells whether this node holds a session with this id that has not expired.".
-spec session_exists(SessionId :: binary()) -> boolean().
session_exists(SessionId) ->
    try ets:lookup(?TABLE, SessionId) of
        [{SessionId, _Session, ExpiresAt, _Class}] -> not expired(ExpiresAt);
        _ -> false
    catch
        error:badarg -> false
    end.

%% Read straight from the table, like session_exists/1: nova_session asks
%% before it starts a session under an id that no session is stored under.
-spec session_ended(SessionId :: binary()) -> boolean().
session_ended(SessionId) ->
    try ets:lookup(?ENDED, SessionId) of
        [{SessionId, Until, _Order}] -> Until > erlang:monotonic_time(millisecond);
        [] -> false
    catch
        error:badarg -> false
    end.

sync() ->
    ok = gen_server:call(?SERVER, sync).

%% A write that is refused here is not sent to the other nodes. Each node
%% keeps to the limit for what the others send it as well, in store/5.
has_room(SessionId, Class) ->
    case max_sessions() of
        infinity ->
            true;
        Max ->
            session_exists(SessionId) orelse ets:info(?TABLE, size) < Max
                orelse gen_server:call(?SERVER, {make_room, Class, Max})
    end.

-spec max_sessions() -> pos_integer() | infinity.
max_sessions() ->
    case nova_settings:value(max_sessions) of
        Max when is_integer(Max), Max > 0 -> Max;
        _Infinity -> infinity
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Initializes the server
-doc false.
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE, [set, named_table, {read_concurrency, true}]),
    ets:new(?EXPIRY, [ordered_set, named_table]),
    ets:new(?ENDED, [set, named_table, {read_concurrency, true}]),
    ets:new(?ENDED_ORDER, [ordered_set, named_table]),
    nova_pubsub:join(?CHANNEL),
    schedule_sweep(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Handling call messages
-doc false.
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({make_room, Class, Max}, _From, State) ->
    {Room, State1} = make_room(Class, Max, State),
    {reply, Room, State1};
handle_call(sync, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Handling cast messages
-doc false.
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Handling all non call/cast messages
-doc false.
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(#nova_pubsub{topic = "set_value", payload = {SessionId, Key, Value}}, State) ->
    {noreply, store(kept, SessionId, Key, Value, State)};
handle_info(#nova_pubsub{topic = "set_provisional_value", payload = {SessionId, Key, Value}}, State) ->
    {noreply, store(provisional, SessionId, Key, Value, State)};
handle_info(#nova_pubsub{topic = "delete_value", payload = {SessionId, Key}}, State) ->
    case live_session(SessionId) of
        {ok, Session, ExpiresAt, Class} ->
            ets:insert(?TABLE, {SessionId, maps:remove(Key, Session), ExpiresAt, Class});
        error ->
            ok
    end,
    {noreply, State};
handle_info(#nova_pubsub{topic = "delete_value", payload = SessionId}, State) ->
    drop(SessionId),
    ended(SessionId),
    {noreply, State};
handle_info(sweep, State) ->
    sweep(),
    sweep_ended(erlang:monotonic_time(millisecond)),
    schedule_sweep(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-doc false.
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Convert process state when code is changed
-doc false.
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
%% An older release had one kind of session, and nothing in a row tells one
%% that signs somebody in from one that only holds a CSRF token, under a key
%% the application chooses. Every row is converted to a kept session: taken
%% for a provisional one, the session of somebody signed in could be dropped
%% for a request without a session, which is what the classes are there to
%% prevent. The price is that the token sessions of the old release are not
%% the first to go until they expire.
code_change(_OldVsn, _State, _Extra) ->
    _ = ets:whereis(?EXPIRY) =:= undefined andalso ets:new(?EXPIRY, [ordered_set, named_table]),
    _ = ets:whereis(?ENDED) =:= undefined andalso ets:new(?ENDED, [set, named_table, {read_concurrency, true}]),
    _ = ets:whereis(?ENDED_ORDER) =:= undefined andalso ets:new(?ENDED_ORDER, [ordered_set, named_table]),
    Old = ets:select(?TABLE, [{{'$1', '$2'}, [], [{{'$1', '$2', infinity}}]},
                              {{'$1', '$2', '$3'}, [], ['$_']}]),
    _ = [insert(SessionId, Session, ExpiresAt, kept) || {SessionId, Session, ExpiresAt} <- Old],
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
-doc false.
-spec format_status(Status) -> NewStatus when Status :: #{'log'=>[any()], 'message'=>_, 'reason'=>_, 'state'=>_},
                                              NewStatus :: #{'log'=>[any()], 'message'=>_, 'reason'=>_, 'state'=>_}.
format_status(Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
live_session(SessionId) ->
    case ets:lookup(?TABLE, SessionId) of
        [{SessionId, Session, ExpiresAt, Class}] ->
            case expired(ExpiresAt) of
                false -> {ok, Session, ExpiresAt, Class};
                true -> error
            end;
        [] ->
            error
    end.

%% A session keeps the expiry it was created with. A provisional one that is
%% written with set_value/3 becomes one to keep, with the expiry of a
%% session created at that point. Nothing is stored under an id that has
%% ended: the write is one that was on its way when the session was deleted,
%% here or on another node.
store(Class, SessionId, Key, Value, State) ->
    case session_ended(SessionId) of
        true -> State;
        false -> store_live(Class, SessionId, Key, Value, State)
    end.

store_live(Class, SessionId, Key, Value, State) ->
    case live_session(SessionId) of
        {ok, Session, _ExpiresAt, provisional} when Class =:= kept ->
            drop(SessionId),
            insert(SessionId, Session#{Key => Value}, expires_at(kept), kept),
            State;
        {ok, Session, ExpiresAt, Stored} ->
            ets:insert(?TABLE, {SessionId, Session#{Key => Value}, ExpiresAt, Stored}),
            State;
        error ->
            drop(SessionId),
            {Room, State1} = case max_sessions() of
                                 infinity -> {true, State};
                                 Max -> make_room(Class, Max, State)
                             end,
            _ = Room andalso insert(SessionId, #{Key => Value}, expires_at(Class), Class),
            State1
    end.

insert(SessionId, Session, ExpiresAt, Class) ->
    ets:insert(?EXPIRY, {{rank(Class), ExpiresAt, SessionId}}),
    ets:insert(?TABLE, {SessionId, Session, ExpiresAt, Class}).

drop(SessionId) ->
    case ets:lookup(?TABLE, SessionId) of
        [{SessionId, _Session, ExpiresAt, Class}] ->
            ets:delete(?EXPIRY, {rank(Class), ExpiresAt, SessionId}),
            ets:delete(?TABLE, SessionId);
        [] ->
            true
    end.

rank(provisional) -> 0;
rank(kept)        -> 1.

%% The id of a session that was deleted is remembered for longer than a
%% request is on its way, so one that began before cannot write the session
%% back. Every sign-out adds one, so they are counted like the sessions,
%% and the one that ended first makes room.
ended(SessionId) ->
    forget_ended(SessionId),
    Until = erlang:monotonic_time(millisecond) + ?ENDED_FOR,
    Order = erlang:unique_integer([monotonic]),
    ets:insert(?ENDED_ORDER, {Order, SessionId, Until}),
    ets:insert(?ENDED, {SessionId, Until, Order}),
    forget_first_ended(max_ended()).

forget_ended(SessionId) ->
    case ets:lookup(?ENDED, SessionId) of
        [{SessionId, _Until, Order}] ->
            ets:delete(?ENDED_ORDER, Order),
            ets:delete(?ENDED, SessionId);
        [] ->
            true
    end.

forget_first_ended(Max) ->
    case ets:info(?ENDED, size) > Max andalso ets:first_lookup(?ENDED_ORDER) of
        {_Order, [{_, SessionId, _Until}]} ->
            forget_ended(SessionId),
            forget_first_ended(Max);
        _Room ->
            ok
    end.

sweep_ended(Now) ->
    case ets:first_lookup(?ENDED_ORDER) of
        {_Order, [{_, SessionId, Until}]} when Until =< Now ->
            forget_ended(SessionId),
            sweep_ended(Now);
        _NoneDue ->
            ok
    end.

-spec max_ended() -> pos_integer().
max_ended() ->
    case max_sessions() of
        infinity -> ?MAX_ENDED;
        Max -> Max
    end.

%% The session of a class that is nearest its expiry. No session expires
%% at 0 and no session id is a number, so the key asked for comes before
%% every session of the class and after every session of the class in front.
nearest_expiry(Class) ->
    Rank = rank(Class),
    case ets:next(?EXPIRY, {Rank, 0, 0}) of
        {Rank, ExpiresAt, SessionId} -> {ExpiresAt, SessionId};
        _ -> none
    end.

%% Whether there is room for one more session of Class, after dropping what
%% it takes to get under Max. Each session dropped is found in the index, so
%% the work is the same however many sessions the table holds.
make_room(Class, Max, State) ->
    Size = ets:info(?TABLE, size),
    case Size < Max orelse session_to_drop(Class) of
        true ->
            {true, State};
        none ->
            {false, warn_when_full(Size, Max, State)};
        {Dropped, SessionId} ->
            drop(SessionId),
            make_room(Class, Max, warn_when_full(Dropped, Size, Max, State))
    end.

%% An expired session first. Then a provisional one, which is all a
%% provisional session may take the place of, so that requests without a
%% cookie cannot push out a session that is in use. A session to keep takes
%% the place of the one nearest its expiry when there is nothing else.
session_to_drop(Class) ->
    Nearest = [{Stored, nearest_expiry(Stored)} || Stored <- [provisional, kept]],
    case [SessionId || {_Stored, {ExpiresAt, SessionId}} <- Nearest, expired(ExpiresAt)] of
        [Expired | _] ->
            {expired, Expired};
        [] ->
            case Nearest of
                [{provisional, {_ExpiresAt, SessionId}}, _Kept] -> {provisional, SessionId};
                [_Provisional, {kept, {_ExpiresAt, SessionId}}] when Class =:= kept -> {kept, SessionId};
                _ -> none
            end
    end.

%% Expired sessions are at the front of each class in the index.
sweep() ->
    sweep(provisional),
    sweep(kept).

sweep(Class) ->
    case nearest_expiry(Class) of
        {ExpiresAt, SessionId} when is_integer(ExpiresAt) ->
            case expired(ExpiresAt) of
                true ->
                    drop(SessionId),
                    sweep(Class);
                false ->
                    ok
            end;
        _ ->
            ok
    end.

%% Dropping an expired session is what the sweep does anyway. Anything else
%% means the table is too small for the sessions in use, or is being filled.
warn_when_full(expired, _Size, _Max, State) ->
    State;
warn_when_full(_Dropped, Size, Max, State) ->
    warn_when_full(Size, Max, State).

warn_when_full(Size, Max, State = #state{warned_at = WarnedAt}) ->
    Now = erlang:monotonic_time(millisecond),
    case WarnedAt =:= undefined orelse Now - WarnedAt >= ?SWEEP_INTERVAL of
        true ->
            ?LOG_WARNING(#{msg => ~"Session store is full. Sessions are dropped to make room for new ones, provisional ones first, and a provisional one is refused when there is none to drop",
                           sessions => Size, max_sessions => Max}),
            State#state{warned_at = Now};
        false ->
            State
    end.

expires_at(kept) ->
    case max_age() of
        undefined -> infinity;
        MaxAge -> erlang:system_time(millisecond) + MaxAge * 1000
    end;
expires_at(provisional) ->
    MaxAge = case max_age() of
                 undefined -> ?PROVISIONAL_MAX_AGE;
                 Configured -> min(Configured, ?PROVISIONAL_MAX_AGE)
             end,
    erlang:system_time(millisecond) + MaxAge * 1000.

expired(infinity) -> false;
expired(ExpiresAt) -> ExpiresAt =< erlang:system_time(millisecond).

max_age() ->
    case nova_session:max_age() of
        infinity -> undefined;
        MaxAge -> MaxAge
    end.

schedule_sweep() ->
    erlang:send_after(?SWEEP_INTERVAL, self(), sweep).
