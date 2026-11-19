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
         delete_value/1,
         delete_value/2,
         session_exists/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-define(SERVER, ?MODULE).
-define(TABLE, nova_session_ets_entries).
-define(EXPIRY, nova_session_ets_expiry).
-define(CHANNEL, '__sessions').
-define(SWEEP_INTERVAL, 60000).

-include("../include/nova_pubsub.hrl").

-record(state, {}).

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

-spec set_value(SessionId :: binary(), Key :: binary(), Value :: binary()) -> ok | {error, Reason :: term()}.
set_value(SessionId, Key, Value) ->
    nova_pubsub:broadcast(?CHANNEL, "set_value", {SessionId, Key, Value}),
    sync().

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

-doc "Tells whether this node holds a session with this id that has not expired.".
-spec session_exists(SessionId :: binary()) -> boolean().
session_exists(SessionId) ->
    try ets:lookup(?TABLE, SessionId) of
        [{SessionId, _Session, ExpiresAt, _Class}] -> not expired(ExpiresAt);
        _ -> false
    catch
        error:badarg -> false
    end.

sync() ->
    ok = gen_server:call(?SERVER, sync).

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
    {noreply, store_live(kept, SessionId, Key, Value, State)};
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
    {noreply, State};
handle_info(sweep, State) ->
    sweep(),
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
    _ = ets:whereis(?EXPIRY) =:= undefined andalso ets:new(?EXPIRY, [ordered_set, named_table]),
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

store_live(Class, SessionId, Key, Value, State) ->
    case live_session(SessionId) of
        {ok, Session, ExpiresAt, Stored} ->
            ets:insert(?TABLE, {SessionId, Session#{Key => Value}, ExpiresAt, Stored}),
            State;
        error ->
            drop(SessionId),
            insert(SessionId, #{Key => Value}, expires_at(Class), Class),
            State
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

nearest_expiry(Class) ->
    Rank = rank(Class),
    case ets:next(?EXPIRY, {Rank, 0, 0}) of
        {Rank, ExpiresAt, SessionId} -> {ExpiresAt, SessionId};
        _ -> none
    end.

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

expires_at(kept) ->
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
