%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2021, Niclas Axelsson
%%% @doc
%%% <i>nova_cache</i> is a basic in-memory cache that uses ETS as backend.
%%% When a cache is initialized a process is spawned and associated for that
%%% particular cache. This process is kept alive through out the whole life
%%% time of the cache and is the owner of the ETS table that is used for storage.
%%%
%%% The TTLs is implemented using erlang:send_after/4.
%%% @end
%%% Created : 22 May 2021 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_cache).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         init_cache/1,
         remove_cache/1,
         get/2,
         set/4,
         set/3,
         delete/2,
         flush/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

-record(state, {
                table
               }).

-define(DEFAULT_TTL, 3600000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Cache :: atom()) -> {ok, Pid :: pid()} |
                                     {error, Error :: {already_started, pid()}} |
                                     {error, Error :: term()} |
                                     ignore.
start_link(Cache) ->
    gen_server:start_link(?MODULE, Cache, []).

%%--------------------------------------------------------------------
%% @doc
%% Removes the cache and frees up the memory.
%% @end
%%--------------------------------------------------------------------
-spec remove_cache(Cachename :: atom()) -> ok.
remove_cache(Cachename) ->
    nova_cache_sup:remove_cache(Cachename).

%%--------------------------------------------------------------------
%% @doc
%% Initializes a cache.
%% @end
%%--------------------------------------------------------------------
-spec init_cache(Cachename :: atom()) -> {ok, Child} |
                                         {ok, Child, Info :: term()} |
                                         {error, {already_started, pid()} | {shutdown, term()} | term()}
                                             when Child :: undefined | pid().
init_cache(Cachename) ->
    nova_cache_sup:init_cache(Cachename).

%%--------------------------------------------------------------------
%% @doc
%% Gets an entriy from the cache.
%% @end
%%--------------------------------------------------------------------
-spec get(Cache :: atom(), Key :: term()) -> {ok, Value :: any()} | {error, not_found}.
get(Cache, Key) ->
    %% Since the values is in a protected ETS table we are allowed to read but not write
    case ets:lookup(Cache, Key) of
        [] ->
            {error, not_found};
        [{_Key, Value, _TimerRef}] ->
            {ok, Value}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sets a cache value with a TTL. To refresh an entry one need to use
%% the set-function again.
%% @end
%%--------------------------------------------------------------------
-spec set(Cache :: atom(), Key :: term(), Value :: any(), TTL :: non_neg_integer()) -> ok.
set(Cache, Key, Value, TTL) ->
    {ok, Id} = nova_cache_sup:get_cache(Cache),
    gen_server:call(Id, {set, Key, Value, TTL}).

%%--------------------------------------------------------------------
%% @doc
%% Same as set/4 but uses the default time for TTL which is defined
%% as 3600000 (3600 seconds)
%% @end
%%--------------------------------------------------------------------
-spec set(Cache :: atom(), Key :: term(), Value :: any()) -> ok.
set(Cache, Key, Value) ->
    set(Cache, Key, Value, ?DEFAULT_TTL).

%%--------------------------------------------------------------------
%% @doc
%% Deletes an entry in the cache.
%% @end
%%--------------------------------------------------------------------
-spec delete(Cache :: atom(), Key :: term()) -> ok.
delete(Cache, Key) ->
    {ok, Id} = nova_cache_sup:get_cache(Cache),
    gen_server:call(Id, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Removes all entries for a cache.
%% @end
%%--------------------------------------------------------------------
-spec flush(Cache :: atom()) -> ok.
flush(Cache) ->
    {ok, Id} = nova_cache_sup:get_cache(Cache),
    gen_server:call(Id, flush).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init(Cache) ->
    process_flag(trap_exit, true),
    Tid = ets:new(Cache, [protected,
                          {read_concurrency, true},
                          {write_concurrency, false},
                          named_table,
                          set]),
    {ok, #state{table = Tid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({set, Key, Value, TTL}, _From, State = #state{table = Table}) ->
    case ets:lookup(Table, Key) of
        [] ->
            %% Start timer to invalidate the cache
            TimerRef = erlang:send_after(TTL, self(), {invalidate, Key}),
            %% Insert
            ets:insert(Table, {Key, Value, TimerRef});
        [{Key, _OtherValue, OldTimerRef}] ->
            %% Cancel old timer
            erlang:cancel_timer(OldTimerRef),
            TimerRef = erlang:send_after(TTL, self(), {invalidate, Key}),
            %% Insert
            ets:insert(Table, {Key, Value, TimerRef})
    end,
    {reply, ok, State};
handle_call(flush, _From, State = #state{table = Table}) ->
    [ erlang:cancel_timer(TimerRef) || {_, _, TimerRef} <- ets:tab2list(Table) ],
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({invalidate, Key}, State = #state{table = Table}) ->
    %% This is an event that should only be used by erlang:send_after/3 so no need to cancel the timer
    ets:delete(Table, Key),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, #state{table = Table}) ->
    %% Cancel all the timers
    [ erlang:cancel_timer(TimerRef) || {_, _, TimerRef} <- ets:tab2list(Table) ],
    %% Delete the table
    ets:delete(Table),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
