%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2019, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2019 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_session_ets).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(TABLE, nova_session_ets_entries).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec get_value(SessionId :: binary(), Key :: binary()) -> {ok, Value :: binary()} | {error, not_found}.
get_value(SessionId, Key) ->
    gen_server:call(?SERVER, {get_value, SessionId, Key}).

-spec set_value(SessionId :: binary(), Key :: binary(), Value :: binary()) -> ok | {error, Reason :: term()}.
set_value(SessionId, Key, Value) ->
    gen_server:call(?SERVER, {set_value, SessionId, Key, Value}).

-spec delete_value(SessionId :: binary()) -> ok | {error, Reason :: term()}.
delete_value(SessionId) ->
    gen_server:call(?SERVER, {delete_value, SessionId}).

-spec delete_value(SessionId :: binary(), Key :: binary()) -> ok | {error, Reason :: term()}.
delete_value(SessionId, Key) ->
    gen_server:call(?SERVER, {delete_value, SessionId, Key}).

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
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE, [set, named_table]),
    {ok, #state{}}.

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
handle_call({get_value, SessionId, Key}, _From, State) ->
    case ets:lookup(?TABLE, SessionId) of
        [] ->
            {reply, {error, not_found}, State};
        [{SessionId, Session}|_] ->
            case maps:get(Key, Session, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                Value ->
                    {reply, {ok, Value}, State}
            end
    end;
handle_call({set_value, SessionId, Key, Value}, _From, State) ->
    case ets:lookup(?TABLE, SessionId) of
        [] ->
            ets:insert(?TABLE, {SessionId, #{Key => Value}});
        [{_, Session}|_] ->
            ets:insert(?TABLE, {SessionId, Session#{Key => Value}})
    end,
    {reply, ok, State};
handle_call({delete_value, SessionId}, _From, State) ->
    Reply = ets:delete(?TABLE, SessionId),
    {reply, Reply, State};
handle_call({delete_value, SessionId, Key}, _From, State) ->
    case ets:lookup(?TABLE, SessionId) of
        [] ->
            {reply, {error, not_found}, State};
        [{SessionId, Session}|_] ->
            ets:insert(?TABLE, {SessionId, maps:remove(Key, Session)})
    end;
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
terminate(_Reason, _State) ->
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
