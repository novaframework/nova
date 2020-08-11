%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2020, Niclas Axelsson
%%% @doc
%%% Plugins can be run at two different times; either in the beginning or at
%% the end of a request. They can modify both the actual request or the nova-state.
%% A plugin is implemented with the <icode>nova_plugin</icode> behaviour
%%% and needs to implement three different functions: <icode>pre_request/2</icode>,
%% <icode>post_request/2</icode> and <icode>plugin_info/0</icode>.
%%%
%%%
%%% <code title="src/example_plugin.erl">
%%% -module(example_plugin).
%%% -behaviour(nova_plugin).
%%% -export([pre_request/2, post_request/2, plugin_info/0]).
%%%
%%% pre_request(Req, State) ->
%%%   Req0 = cowboy_req:set_resp_header(<<"x-nova-started">>, erlang:system_time(milli_seconds), Req),
%%%   {ok, Req0, State}.
%%%
%%% post_request(Req, State) ->
%%%   Started = cowboy_req:header(<<"x-nova-started">>, Req),
%%%   Now = erlang:system_time(milli_seconds),
%%%   ?INFO("Request ran for ~.B milliseconds", [Now-Started]),
%%%   {ok, Req, State}.
%%%
%%% plugin_info() ->
%%%   {<<"Execution time plugin">>, <<"1.0.0">>, <<"Niclas Axelsson <niclas@burbas.se>">>,
%%     <<"Example plugin for nova">>}.
%%% </code>
%%%
%%%
%%% To register the plugin above you have to call
%%  <icode>nova_plugin:register_plugin(RequestType, http, example_plugin).</icode> in order
%%% to run it. <icode>RequestType</icode> can either be <icode>pre_request</icode> or
%%  <icode>post_request</icode>.
%%% @end
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_plugin).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         register_plugin/3,
         register_plugin/4,
         unregister_plugin/1,
         get_all_plugins/0,
         get_plugins/2
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

-type request_type() :: pre_request | post_request.
-export_type([request_type/0]).

-type protocol() :: http | websocket.
-export_type([protocol/0]).


%% Define the callback functions for plugins
-callback pre_request(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
    {ok, State0 :: nova_http_handler:nova_http_state()} |
    {break, State0 :: nova_http_handler:nova_http_state()} |
    {stop, State0 :: nova_http_handler:nova_http_state()} |
    {error, Reason :: term()}.
-callback post_request(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
    {ok, State0 :: nova_http_handler:nova_http_state()} |
    {break, State0 :: nova_http_handler:nova_http_state()} |
    {stop, State0 :: nova_http_handler:nova_http_state()} |
    {error, Reason :: term()}.
-callback plugin_info() -> {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary()}.

-define(SERVER, ?MODULE).

-define(NOVA_PLUGIN_TABLE, nova_plugin_table).


%% We don't currently use the state, but its good to have :-).
-record(state, {
               }).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @hidden
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
%% Register a plugin. This operation is asyncronous so the atom
%% <icode>ok</icode> will always be returned. If an error occurs it will
%% be stated in the logs.
%% @end
%%--------------------------------------------------------------------
-spec register_plugin(RequestType :: request_type(), Protocol :: protocol(), Module :: atom()) -> ok.
register_plugin(RequestType, Protocol, Module) when RequestType == pre_request orelse
                                                             RequestType == post_request ->
    register_plugin(RequestType, Protocol, Module, #{}).

register_plugin(RequestType, Protocol, Module, Options) when RequestType == pre_request orelse
                                                             RequestType == post_request ->
    gen_server:cast(?SERVER, {register_plugin, RequestType, Protocol, Module, Options}).

%%--------------------------------------------------------------------
%% @doc
%% Unregisters a plugin with a given <icode>Id</icode>. The id can be retrieved
%% by calling either <icode>get_all_plugins/0</icode> or <icode>get_plugins/2</icode>
%% to find the specific plugin.
%% @end
%%--------------------------------------------------------------------
-spec unregister_plugin(Id :: binary()) -> ok.
unregister_plugin(Id) ->
    gen_server:call(?SERVER, {unregister_plugin, Id}).

%%--------------------------------------------------------------------
%% @doc
%% Returns all registered plugins.
%% @end
%%--------------------------------------------------------------------
-spec get_all_plugins() -> {ok, [{{PluginId :: binary(), ReqType :: pre_request | post_request,
                                   Protocol :: http}, Module :: atom()}]}.
get_all_plugins() ->
    {ok, ets:match(?NOVA_PLUGIN_TABLE, '$1')}.

%%--------------------------------------------------------------------
%% @doc
%% Get all plugins that is associated with a specific <icode>RequestType</icode> and
%% <icode>Protocol</icode>. Will return {ok, <icode>[{PluginModule, Options}]</icode>}.
%% @end
%%--------------------------------------------------------------------
-spec get_plugins(RequestType :: pre_request | post_request, Protocol :: http) ->
                         {ok, [{Module :: atom(), Options :: map()}]}.
get_plugins(RequestType, Protocol) when RequestType == pre_request orelse
                                       RequestType == post_request ->
    Plugins = ets:match(?NOVA_PLUGIN_TABLE, {{'_', RequestType, Protocol}, {'$1', '$2'}}),

    {ok, [{Module, Options} || [Module, Options] <- Plugins]}.

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
init(_Args) ->
    process_flag(trap_exit, true),
    ets:new(?NOVA_PLUGIN_TABLE, [named_table, bag, protected]),
    Plugins = application:get_env(nova, plugins, []),
    lists:foreach(fun({ReqType, Protocol, Module}) -> register_plugin(ReqType, Protocol, Module);
                     ({ReqType, Protocol, Module, Options}) -> register_plugin(ReqType, Protocol, Module, Options)
                  end, Plugins),
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
handle_call({unregister_plugin, ID}, _From, State) ->
    ?DEBUG("Removing plugin with ID: ~s", [ID]),
    true = ets:match_delete(?NOVA_PLUGIN_TABLE, {{ID, '_', '_'}, {'_', '_'}}),
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
handle_cast({register_plugin, ReqType, Protocol, Module, Options}, State) ->
    PluginId = uuid:get_v4(),
    ?DEBUG("Register plugin for protocol ~s request-type ~s with ID: ~s and options: ~p",
           [Protocol, ReqType, PluginId, Options]),
    ets:insert(?NOVA_PLUGIN_TABLE, {{PluginId, ReqType, Protocol}, {Module, Options}}),
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
