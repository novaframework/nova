%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2020, Niclas Axelsson
%%% @doc
%%% This module is responsible for all the different return types a controller have. <i>Nova</i> is constructed
%%% in such way that it's really easy to extend it by using <i>handlers</i>. A handler is basically a module consisting
%%% of a function of arity 4. We will show an example of this.
%%%
%%% If you implement the following module:
%%% <code title="src/my_handler.erl">
%%% -module(my_handler).
%%% -export([init/0,
%%%          handle_console]).
%%%
%%% init() ->
%%%    nova_handlers:register_handler(console, {my_handler, handle_console}).
%%%
%%% handle_console({console, Format, Args}, {Module, Function}, State) ->
%%%    io:format("~n=====================~n", []).
%%%    io:format("~p:~p was called.~n", []),
%%%    io:format("State: ~p~n", [State]),
%%%    io:format(Format, Args),
%%%    io:format("~n=====================~n", []),
%%%    {ok, 200, #{}, <binary></binary>}.
%%% </code>
%%%
%%% The <icode>init/0</icode> should be invoked from your applications <i>supervisor</i> and will register the module
%%% <icode>my_handler</icode> as handler of the return type <icode>{console, Format, Args}</icode>. This means that you
%%% can return this tuple in a controller which invokes <icode>my_handler:handle_console/4</icode>.
%%%
%%% <b>A handler can return two different types</b>
%%%
%%% <icode>{ok, StatusCode, Headers, Body}</icode> - This will return a proper reply to the requester.
%%%
%%% <icode>{error, Reason}</icode> - This will render a 500 page to the user.
%%% @end
%%% Created : 12 Feb 2020 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_handlers).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         register_handler/2,
         unregister_handler/1,
         get_handler/1
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

-define(HANDLERS_TABLE, nova_handlers_table).

-type handler_return() :: {ok, State2 :: nova:state()} |
                          {Module :: atom(), State :: nova:state()} |
                          {error, Reason :: any()}.

-export_type([handler_return/0]).

-type handler_callback() :: {Module :: atom(), Function :: atom()} |
                            fun((...) -> handler_return()).

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
%% Registers a new handler. This can then be used in a nova controller
%% by returning a tuple where the first element is the name of the handler.
%% @end
%%--------------------------------------------------------------------
-spec register_handler(Handle :: atom(), Callback :: handler_callback()) ->
                              ok | {error, Reason :: atom()}.
register_handler(Handle, Callback) ->
    gen_server:cast(?SERVER, {register_handler, Handle, Callback}).

%%--------------------------------------------------------------------
%% @doc
%% Unregisters a handler and makes it unavailable for all controllers.
%% @end
%%--------------------------------------------------------------------
-spec unregister_handler(Handle :: atom()) -> ok.
unregister_handler(Handle) ->
    gen_server:call(?SERVER, {unregister_handler, Handle}).

%%--------------------------------------------------------------------
%% @doc
%% Fetches the handler identified with 'Handle' and returns the callback
%% function for it.
%% @end
%%--------------------------------------------------------------------
-spec get_handler(Handle :: atom()) -> {ok, Callback :: handler_callback()} |
                                       {error, not_found}.
get_handler(Handle) ->
    case ets:lookup(?HANDLERS_TABLE, Handle) of
        [] ->
            {error, not_found};
        [{Handle, Callback}] ->
            {ok, Callback}
    end.

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
    ets:new(?HANDLERS_TABLE, [named_table, set, protected]),
    register_handler(json, fun nova_basic_handler:handle_json/3),
    register_handler(ok, fun nova_basic_handler:handle_ok/3),
    register_handler(status, fun nova_basic_handler:handle_status/3),
    register_handler(redirect, fun nova_basic_handler:handle_redirect/3),
    register_handler(sendfile, fun nova_basic_handler:handle_sendfile/3),
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
handle_call({unregister_handler, Handle}, _From, State) ->
    ets:delete(?HANDLERS_TABLE, Handle),
    ?DEBUG("Removed handler ~p", [Handle]),
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
handle_cast({register_handler, Handle, Callback}, State) ->
    Callback0 =
        case Callback of
            Callback when is_function(Callback) -> Callback;
            {Module, Function} -> fun Module:Function/4
        end,
    case ets:lookup(?HANDLERS_TABLE, Handle) of
        [] ->
            ?DEBUG("Registered handler '~p'", [Handle]),
            ets:insert(?HANDLERS_TABLE, {Handle, Callback0}),
            {noreply, State};
        _ ->
            ?ERROR("Could not register handler ~p since there's already another one registered on that name", [Handle]),
            {noreply, State}
    end;
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
