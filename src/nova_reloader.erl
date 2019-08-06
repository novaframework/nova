%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2018 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_reloader).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         add_directory/1
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


-include_lib("kernel/include/file.hrl").


-define(TIMER_INTERVAL, 1000). %% 1000ms

-record(state, {
                files_state = #{}
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


add_directory(Dir) ->
    gen_server:cast(?MODULE, {add_dir, Dir}).

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
    timer:send_after(?TIMER_INTERVAL, self(), check_files),
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
handle_cast({add_dir, Dir}, State = #state{files_state = FS}) ->
    FS2 = recompile_newer([Dir], FS),
    {noreply, State#state{files_state = FS2}};
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
handle_info(check_files, State = #state{files_state = FS}) ->
    {ok, Apps} = application:get_env(nova_applications),
    Filepaths = [ code:lib_dir(App, src) || #{name := App} <- Apps ],
    FS2 = recompile_newer(Filepaths, FS),
    timer:send_after(?TIMER_INTERVAL, self(), check_files),
    {noreply, State#state{files_state = FS2}};
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
reload_module(Module) ->
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            logger:info("Reloaded module ~p", [Module]),
            ok;
        {error, Reason} ->
            logger:error("Could not reload module ~p. Exited with reason ~p", [Module, Reason]),
            error
    end.



recompile_newer([], State) -> State;
recompile_newer([File|Files], State) ->
    case maps:get(File, State, undefined) of
        undefined ->
            case filelib:is_dir(File) of
                true ->
                    {ok, DirFiles} = file:list_dir(File),
                    DirFiles2 = [ filename:join(File, X) || X <- DirFiles ],
                    SubState = recompile_newer(DirFiles2, State),
                    recompile_newer(Files, SubState);
                _ ->
                    recompile_newer(Files, State#{File => calendar:local_time()})
            end;
        Time ->
            case file:read_file_info(File) of
                {ok, #file_info{mtime = MTime}} when is_tuple(MTime) andalso
                                                     MTime >= Time ->
                    %% This file have been changed so recompile and reload.
                    {Module, _} = nova_compiler:do_compile([File]),
                    reload_module(Module),
                    recompile_newer(Files, State#{File := calendar:local_time()});
                _ ->
                    %% File have not changed
                    recompile_newer(Files, State)
            end
    end.
