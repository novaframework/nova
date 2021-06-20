%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2021, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2021 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_watcher).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         async_cast/4,
         async_cast/3,
         async_cast/2,
         async_cast/1,
         stop/0
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

-include_lib("nova/include/nova.hrl").

-define(SERVER, ?MODULE).

-record(state, {
                process_refs = [] :: [pid()]
               }).

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

async_cast(Application, Cmd, Args, Options) ->
    gen_server:cast(?SERVER, {async, Application, Cmd, Args, Options}).

async_cast(Application, Cmd, Options) ->
    async_cast(Application, Cmd, [], Options).

async_cast(Cmd, Options) ->
    async_cast(nova:get_main_app(), Cmd, Options).

async_cast(Cmd) ->
    async_cast(Cmd, #{}).


stop() ->
    gen_server:call(?SERVER, stop).

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
    CmdList = nova:get_env(watchers, []),
    [ async_cast(Cmd, Args) || {Cmd, Args} <- CmdList ],
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
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    Reply = ok,
    ?ERROR("Unknown request: ~p", [Request]),
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
handle_cast({async, Application, Cmd, Args, Options}, State = #state{process_refs = ProcessRefs}) ->
    LibDir = code:lib_dir(Application),
    Workdir =
        case maps:get(workdir, Options, undefined) of
            undefined ->
                LibDir;
            Subdir ->
                filename:join([LibDir, Subdir])
        end,
    %% Set working directory
    file:set_cwd(Workdir),
    ArgList = string:join(Args, " "),
    Port = erlang:open_port({spawn, Cmd ++ " " ++ ArgList}, []),
    ?NOTICE("Started command ~s with args ~s", [Cmd, ArgList]),
    {noreply, State#state{process_refs = [Port|ProcessRefs]}};
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
handle_info({_ProcessRef, {data, Data}}, State) ->
    ?DEBUG("[NOVA_WATCHER] ~s", [Data]),
    {noreply, State};
handle_info({'EXIT', Ref, Reason}, State = #state{process_refs = Refs}) ->
    %% Remove the port from our list
    Refs2 = lists:delete(Ref, Refs),
    case Reason of
        normal ->
            ok;
        _ ->
            ?WARNING("[NOVA_WATCHER] Process exited with reason: ~p", [Reason])
    end,
    {noreply, State#state{process_refs = Refs2}};
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
terminate(_Reason, #state{process_refs = Refs}) ->
    %% Clean up the ports
    lists:foreach(fun(PortRef) ->
                          erlang:port_close(PortRef)
                  end, Refs),
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






%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_ls_test_() ->
    {setup,
     fun() ->
             ?MODULE:start_link()
     end,
     fun(_) ->
             ?MODULE:stop()
     end,
     fun(_) ->
             [?_assertEqual(?MODULE:async_cast("ls"), ok)]
     end}.

-endif.
