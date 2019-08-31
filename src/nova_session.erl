%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2018 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_session).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         get_session/1,
         new_session/1,
         put/3
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          session_table
         }).

-record(session_data, {
          id :: list(),
          expires :: integer(),
          data :: map()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_session(Req) ->
    case cowboy_req:match_cookies([{session_id, [], undefined}], Req) of
        undefined ->
            %% Create new session
            new_session(Req);
        #{session_id := SessionId} ->
            case ets:lookup(?SERVER, SessionId) of
                [#session_data{data = Data}] ->
                    {Data, Req};
                [] ->
                    new_session(Req)
            end
    end.

new_session(Req) ->
    gen_server:call(?SERVER, {new_session, Req}).

put(Key, Value, Req) ->
    gen_server:call(?SERVER, {put, Key, Value, Req}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SessionStorage = ets:new(?SERVER, [named_table,
                                       protected,
                                       {keypos, 2},
                                       {read_concurrency,true},
                                       compressed]),
    process_flag(trap_exit, true),
    {ok, #state{session_table = SessionStorage}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({new_session, Req}, _From, State) ->
    SessionId = generate_session_id(),
    ets:insert(?SERVER, #session_data{id = SessionId, data = #{}}),
    Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req, #{http_only => true}),
    {reply, {#{}, Req1}, State};
handle_call({put, Key, Value, Req}, _From, State) ->
    #{session_id := SessionId} = cowboy_req:match_cookies([{session_id, [], undefined}], Req),
    Data =
        case ets:lookup(?SERVER, SessionId) of
            [] -> #{};
            [#session_data{data = Data0}] -> Data0
        end,
    ets:insert(?SERVER, #session_data{id = SessionId, data = Data#{Key => Value}}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_session_id() ->
    uuid:uuid_to_string(uuid:get_v4()).
