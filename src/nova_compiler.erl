%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2018, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2018 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_compiler).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         compile_all/0,
         compile_all/1,
         do_compile/1,
         get_views/0,
         recompile_view/1
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

-include_lib("nova/include/nova.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          compiled_modules
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

compile_all(App) ->
    gen_server:cast(?SERVER, {compile_app_views, App}).

compile_all() ->
    Apps =
        case application:get_env(nova_applications) of
            undefined -> [];
            {ok, AppList} -> AppList
        end,
    lists:foreach(fun compile_all/1, Apps),
    ok.

get_views() ->
    gen_server:call(?SERVER, get_views).

recompile_view(Viewname) ->
    gen_server:call(?SERVER, {recompile, Viewname}).

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
    compile_all(),
    process_flag(trap_exit, true),
    {ok, #state{compiled_modules = #{}}}.

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
handle_call(get_views, _From, State = #state{compiled_modules = CM}) ->
    {reply, {ok, CM}, State};
handle_call({recompile, Viewname}, _From, State = #state{compiled_modules = CM}) ->
    case maps:get(Viewname, CM, undefined) of
        undefined ->
            ?WARNING("Tried to recompile a module (~p) that does not exists?", [Viewname]),
            {reply, {error, view_not_found}, State};
        File ->
            do_compile([File]),
            {reply, ok, State}
    end;
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
handle_cast({compile_app_views, App}, State = #state{compiled_modules = CM}) ->
    CM2 =
        case compile_files(App) of
            error ->
                CM;
            ModMap ->
                maps:merge(ModMap, CM)
        end,
    {noreply, State#state{compiled_modules = CM2}};
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
compile_files(#{name := App}) ->
    case code:lib_dir(App, src) of
        {error, _} ->
            ?WARNING("Could not find the src directory of app ~p", [App]),
            error;
        Filepath ->
            {ok, FilesAndDirs} = file:list_dir(Filepath),
            Result = [ do_compile(filename:join([Filepath, FileOrDir])) || FileOrDir <- FilesAndDirs ],
            maps:from_list(lists:flatten(Result))
    end.

do_compile(File) ->
    BeamDir =
        case application:get_application() of
            {ok, App} ->
                EbinDir = code:lib_dir(App, ebin),
                filelib:ensure_dir(EbinDir),
                EbinDir;
            _ ->
                %% We're running inside an unknown application - just put it in somewhere (This is ugly)
                [Dir|_] = code:get_path(),
                Dir
        end,

    case filelib:is_dir(File) of
        true ->
            {ok, SubFiles} = file:list_dir(File),
            [ do_compile(filename:join(File, SubFile)) || SubFile <- SubFiles ];
        _ ->
            case filename:extension(File) of
                ".dtl" ->
                    ModName = list_to_atom(filename:basename(File, ".dtl") ++ "_dtl"),
                    {module, _} = code:ensure_loaded(erlydtl),
                    try erlydtl:compile_file(File, ModName, [{out_dir, BeamDir}, return]) of
                        {ok, Module} ->
                            ?INFO("Compiled dtl view: ~p", [Module]);
                        {ok, Module, Binary} when is_binary(Binary) ->
                            ?INFO("Compiled dtl view: ~p", [Module]);
                        {ok, Module, Warnings} ->
                            ?WARNING("Compiled dtl view: ~p with warnings: ~p", [Module, Warnings]);
                        {ok, Module, _Binary, Warnings} ->
                            ?WARNING("Compiled dtl view: ~p with warnings: ~p", [Module, Warnings]);
                        {error, Error, Warnings} ->
                            format_msg(error, Error)
                    catch
                        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                        ?ERROR("Could not compile file ~s. Reason ~p~nStacktrace:~n~p", [File, Reason, Stacktrace])
                    end,
                    {ModName, File};
                ".erl" ->
                    try compile:file(File, [{out_dir, BeamDir}]) of
                        {error, Errors, Warnings} ->
                            ?WARNING("Got error when compiling ~p. Errors: ~p, Warnings: ~p", [File, Errors, Warnings]),
                            [];
                        error ->
                            ?WARNING("Could not compile file ~p", [File]),
                            [];
                        {ok, ModuleName, Warnings} ->
                            ?WARNING("Compiled erlang module from file: ~p with warnings: ~p", [File, Warnings]),
                            {ModuleName, File};
                        {ok, ModuleName} ->
                            ?INFO("Compiled erlang module from file: ~p to dir: ~p", [File, BeamDir]),
                            {ModuleName, File}
                    catch
                        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                          ?ERROR("Could not compile file ~s. Reason ~p", [File, Reason])
                    end;
                Unsupported ->
                    %% Not supported file
                    ?WARNING("Nova does not support files ending with ~s. If this was intentionally just disregard this warning.", [Unsupported]),
                    []
            end
    end.



format_msg(Level, [{File, [{Row, Module, [Msg, Param]}]}]) ->
    case Level of
        error ->
            ?ERROR("In file \"~s\", ~p:~p \"~s\", ~p", [File, Module, Row, Msg, Param]);
        _ ->
            ?WARNING("In file \"~s\", ~p:~p \"~s\", ~p", [File, Module, Row, Msg, Param])
    end;
format_msg(_, _) ->
    ok.
