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
it is full, expired sessions are dropped, and when that leaves no room a write
that would start a new session returns `{error, session_store_full}`. Sessions
that exist are still written. Set `{nova, [{max_sessions, infinity}]}` to turn
the limit off.

Sessions do not survive a restart, and a node that starts or joins later only
has the sessions written after it joined. During a rolling deploy or behind a
load balancer without sticky sessions, a client whose id the serving node does
not have gets a fresh session (Nova replaces ids the backend does not know),
so it is signed out. Use a shared backend, such as a database, when sessions
have to outlive a node.

Rows are `{SessionId, Data, ExpiresAt}`. Older releases stored
`{SessionId, Data}`; `code_change/3` converts them on a hot upgrade.
""".

-behaviour(gen_server).
-behaviour(nova_session).

%% API
-export([
         start_link/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2,
         session_exists/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-define(SERVER, ?MODULE).
-define(TABLE, nova_session_ets_entries).
-define(CHANNEL, '__sessions').
-define(SWEEP_INTERVAL, 60000).
-define(RECLAIM_INTERVAL, 1000).
-define(DEFAULT_MAX_SESSIONS, 1000000).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova_pubsub.hrl").

-record(state, {reclaimed_at :: integer() | undefined,
                warned_at :: integer() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
-doc "Starts the server".
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Read straight from the table, so reads do not queue behind the writes.
-spec get_value(SessionId :: binary(), Key :: binary()) -> {ok, Value :: binary()} | {error, not_found}.
get_value(SessionId, Key) ->
    try live_session(SessionId) of
        {ok, #{Key := Value}, _ExpiresAt} -> {ok, Value};
        _ -> {error, not_found}
    catch
        error:badarg -> {error, not_found}
    end.

%% Every write waits for the local copy, so the next request on this node,
%% which may arrive as soon as the response is out, already sees it.
-spec set_value(SessionId :: binary(), Key :: binary(), Value :: binary()) -> ok | {error, session_store_full}.
set_value(SessionId, Key, Value) ->
    case has_room(SessionId) of
        true ->
            nova_pubsub:broadcast(?CHANNEL, "set_value", {SessionId, Key, Value}),
            sync();
        false ->
            {error, session_store_full}
    end.

-spec delete_value(SessionId :: binary()) -> ok.
delete_value(SessionId) ->
    nova_pubsub:broadcast(?CHANNEL, "delete_value", SessionId),
    sync().

-spec delete_value(SessionId :: binary(), Key :: binary()) -> ok.
delete_value(SessionId, Key) ->
    nova_pubsub:broadcast(?CHANNEL, "delete_value", {SessionId, Key}),
    sync().

%% Read straight from the table: nova_session calls this on every access by a
%% request that carries a session cookie.
-spec session_exists(SessionId :: binary()) -> boolean().
session_exists(SessionId) ->
    try ets:lookup(?TABLE, SessionId) of
        [{SessionId, _Session, ExpiresAt}] -> not expired(ExpiresAt);
        _ -> false
    catch
        error:badarg -> false
    end.

sync() ->
    ok = gen_server:call(?SERVER, sync).

%% Only the node taking the write counts: what other nodes replicate is
%% stored, or the nodes would end up with different sessions.
has_room(SessionId) ->
    case max_sessions() of
        infinity ->
            true;
        Max ->
            session_exists(SessionId) orelse ets:info(?TABLE, size) < Max
                orelse gen_server:call(?SERVER, {reclaim, Max}) < Max
    end.

max_sessions() ->
    case nova_config:get_env(max_sessions, ?DEFAULT_MAX_SESSIONS) of
        infinity -> infinity;
        Max when is_integer(Max), Max >= 0 -> Max;
        _ -> ?DEFAULT_MAX_SESSIONS
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
handle_call({reclaim, Max}, _From, State) ->
    State1 = reclaim(State),
    Size = ets:info(?TABLE, size),
    {reply, Size, warn_when_full(Size, Max, State1)};
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
    case live_session(SessionId) of
        {ok, Session, ExpiresAt} ->
            ets:insert(?TABLE, {SessionId, Session#{Key => Value}, ExpiresAt});
        error ->
            ets:insert(?TABLE, {SessionId, #{Key => Value}, expires_at()})
    end,
    {noreply, State};
handle_info(#nova_pubsub{topic = "delete_value", payload = {SessionId, Key}}, State) ->
    case live_session(SessionId) of
        {ok, Session, ExpiresAt} ->
            ets:insert(?TABLE, {SessionId, maps:remove(Key, Session), ExpiresAt});
        error ->
            ok
    end,
    {noreply, State};
handle_info(#nova_pubsub{topic = "delete_value", payload = SessionId}, State) ->
    ets:delete(?TABLE, SessionId),
    {noreply, State};
handle_info(sweep, State) ->
    case max_age() of
        undefined -> ok;
        _ -> sweep()
    end,
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
code_change(_OldVsn, _State, _Extra) ->
    Old = ets:select(?TABLE, [{{'$1', '$2'}, [], [{{'$1', '$2'}}]}]),
    ets:insert(?TABLE, [{SessionId, Session, infinity} || {SessionId, Session} <- Old]),
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
        [{SessionId, Session, ExpiresAt}] ->
            case expired(ExpiresAt) of
                false -> {ok, Session, ExpiresAt};
                true -> error
            end;
        [] ->
            error
    end.

sweep() ->
    Now = erlang:system_time(millisecond),
    ets:select_delete(?TABLE, [{{'_', '_', '$1'}, [{is_integer, '$1'}, {'=<', '$1', Now}], [true]}]).

%% Sweeping walks the whole table, so a full table is swept at most once a
%% second however many writes it refuses.
reclaim(State = #state{reclaimed_at = ReclaimedAt}) ->
    Now = erlang:monotonic_time(millisecond),
    case ReclaimedAt =:= undefined orelse Now - ReclaimedAt >= ?RECLAIM_INTERVAL of
        true ->
            sweep(),
            State#state{reclaimed_at = Now};
        false ->
            State
    end.

warn_when_full(Size, Max, State = #state{warned_at = WarnedAt}) when Size >= Max ->
    Now = erlang:monotonic_time(millisecond),
    case WarnedAt =:= undefined orelse Now - WarnedAt >= ?SWEEP_INTERVAL of
        true ->
            ?LOG_WARNING(#{msg => ~"Session store is full, new sessions are refused",
                           sessions => Size, max_sessions => Max}),
            State#state{warned_at = Now};
        false ->
            State
    end;
warn_when_full(_Size, _Max, State) ->
    State.

expires_at() ->
    case max_age() of
        undefined -> infinity;
        MaxAge -> erlang:system_time(millisecond) + MaxAge * 1000
    end.

expired(infinity) -> false;
expired(ExpiresAt) -> ExpiresAt =< erlang:system_time(millisecond).

max_age() ->
    case nova_session:max_age() of
        infinity -> undefined;
        MaxAge -> MaxAge
    end.

schedule_sweep() ->
    erlang:send_after(?SWEEP_INTERVAL, self(), sweep).
