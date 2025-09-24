%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Main manager for handling plugins
%%% @end
%%% Created : 10 Aug 2024 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_plugin_manager).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         add_plugin/1,
         add_plugin/3,
         get_state/1,
         set_state/2
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

-include_lib("kernel/include/logger.hrl").

-define(TABLE, '__nova_plugins__').
-define(SERVER, ?MODULE).

-record(state, {}).

-record(plugin, {
                 module :: atom(),
                 name :: binary(),
                 version :: binary(),
                 state :: nova:state()
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

%%--------------------------------------------------------------------
%% @doc
%% Adds a plugin to the manager. The plugin module must implement
%% the plugin_info/0 function returning #{title := Title, version := Version}.
%% Optionally it can implement init/0 returning the initial state
%% for the plugin, and stop/0 for cleaning up when the manager
%% terminates.
%% @end
-spec add_plugin({Class :: atom(), Module :: atom(), Opts :: map()} |
                 Module :: atom() | Callback :: function()) ->
            ok | {error, Reason :: term()}.
add_plugin({_Class, Module, _Opts}) ->
    add_plugin(Module);
add_plugin(Module) when is_atom(Module) ->
    #{title := Title, version := Version} =  case Module:plugin_info() of
                                                {Title, Version, _, _} -> #{title => Title, version => Version};
                                                PluginInfo -> PluginInfo
                                             end,
    add_plugin(Module, Title, Version);
add_plugin(Callback) when is_function(Callback) ->
    Info = erlang:fun_info(Callback),
    Module = proplists:get_value(module, Info),
    add_plugin(Module).

-spec add_plugin(Module :: atom(), Name :: binary(), Version :: binary()) ->
          ok | {error, Reason :: term()}.
add_plugin(Module, Name, Version) ->
    case ets:lookup(?TABLE, Module) of
        [#plugin{}] ->
            ?LOG_DEBUG("Plugin ~p already initialized.", [Module]),
            ok;
        [] ->
            gen_server:cast(?SERVER, {add_plugin, Module, Name, Version})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Gets the state of a plugin. The plugin can be specified
%% either as a module or as a callback function. The callback function
%% is used to extract the module name in order to retrieve the state.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Module :: atom() | Callback :: function()) ->
          {ok, State :: nova:state()} | {error, not_found}.
get_state(Module) when is_atom(Module) ->
    case ets:lookup(?TABLE, Module) of
        [#plugin{state = State}] ->
            {ok, State};
        _ ->
            ?LOG_DEBUG("Plugin ~p not found. get_state/1 failed.", [Module]),
            {error, not_found}
    end;
get_state(Callback) when is_function(Callback) ->
    Info = erlang:fun_info(Callback),
    Module = proplists:get_value(module, Info),
    get_state(Module).


%%--------------------------------------------------------------------
%% @doc
%% Sets the state of a plugin. The plugin can be specified
%% either as a module or as a callback function. The callback function
%% is used to extract the module name in order to set the state.
%% @end
%%--------------------------------------------------------------------
-spec set_state(Module :: atom() | Callback :: function(),
                          NewState :: term()) ->
          ok | {error, not_found}.
set_state(Module, NewState) when is_atom(Module) ->
    case ets:lookup(?TABLE, Module) of
        [#plugin{} = P] ->
            gen_server:call(?SERVER, {set_state, Module, P, NewState});
        _ ->
            ?LOG_DEBUG("Plugin ~p not found. set_state/2 failed.", [Module]),
            {error, not_found}
    end;
set_state(Callback, NewState) when is_function(Callback) ->
    Info = erlang:fun_info(Callback),
    Module = proplists:get_value(module, Info),
    set_state(Module, NewState).

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
    ets:new(?TABLE, [named_table, set, protected, {keypos, #plugin.module}]),
    Plugins = nova_router:plugins(),
    [ add_plugin(Callback) || Callback <- Plugins ],
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
handle_call({set_state, Module, P, NewState}, _From, State) ->
    ?LOG_DEBUG("Set state for plugin ~p; NewState: ~p", [Module, NewState]),
    ets:insert(?TABLE, P#plugin{state = NewState}),
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
handle_cast({add_plugin, Module, Name, Version}, State) ->
    PluginState =
        case erlang:function_exported(Module, init, 0) of
            true ->
                ?LOG_INFO("Initializing plugin ~p", [Module]),
                Module:init();
            _ ->
                ?LOG_DEBUG("Plugin ~p has no init/0 function. Defaulting to empty map #{}", [Module]),
                #{}
        end,
    ets:insert(?TABLE, #plugin{module = Module, name = Name, version = Version, state = PluginState}),
    {noreply, State};
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
    %% Stop all plugins and clean state
    Plugins = ets:tab2list(?TABLE),
    [ Plugin:stop() || #plugin{module = Plugin} <- Plugins,
                       erlang:function_exported(Plugin, stop, 0) ],
    ets:delete(?TABLE),
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


%%%===================================================================
%%% Internal functions
%%%===================================================================
