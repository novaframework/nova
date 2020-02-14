%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2020, Niclas Axelsson
%%% @doc
%%%
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

-type nova_handler_callback() :: {Module :: atom(), Function :: atom()} |
                                 fun((...) -> any()).

-record(state, {
                handlers :: [{Handle :: atom(), Callback :: nova_handler_callback()}]
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
%% Registers a new handler. This can then be used in a nova controller
%% by returning a tuple where the first element is the name of the handler.
%% @end
%%--------------------------------------------------------------------
-spec register_handler(Handle :: atom(), Callback :: nova_handler_callback()) ->
                              ok | {error, Reason :: atom()}.
register_handler(Handle, Callback) ->
    gen_server:cast(?SERVER, {register_handler, Handle, Callback}).

%%--------------------------------------------------------------------
%% @doc
%% Unregisters a handler and makes it unavailable for all controllers.
%% @end
%%--------------------------------------------------------------------
-spec unregister_handler(Handle :: atom()) -> ok | {error, Reason :: atom()}.
unregister_handler(Handle) ->
    gen_server:call(?SERVER, {unregister_handler, Handle}).

%%--------------------------------------------------------------------
%% @doc
%% Fetches the handler identified with 'Handle' and returns the callback
%% function for it.
%% @end
%%--------------------------------------------------------------------
-spec get_handler(Handle :: atom()) -> {ok, Callback :: nova_handler_callback()} |
                                       {error, Reason :: atom()}.
get_handler(Handle) ->
    gen_server:call(?SERVER, {get_handler, Handle}).

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
    register_handler(json, fun nova_basic_handler:handle_json/4),
    register_handler(ok, fun nova_basic_handler:handle_ok/4),
    register_handler(status, fun nova_basic_handler:handle_status/4),
    register_handler(redirect, fun nova_basic_handler:handle_redirect/4),
    register_handler(cowboy_req, fun nova_basic_handler:handle_cowboy_req/4),
    {ok, #state{handlers = []}}.

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
handle_call({unregister_handler, Handle}, _From, State = #state{handlers = Handlers}) ->
    case proplists:delete(Handle, Handlers) of
        Handlers ->
            ?WARNING("Error when unregistering handler: Could not find handler ~p", [Handle]),
            {reply, {error, not_found}, State};
        Handlers0 ->
            ?DEBUG("Removed handler ~p", [Handle]),
            {reply, ok, Handlers0}
    end;
handle_call({get_handler, Handle}, _From, State = #state{handlers = Handlers}) ->
    case proplists:get_value(Handle, Handlers) of
        undefined ->
            ?WARNING("Could not find handler ~p", [Handle]),
            {reply, {error, not_found}, State};
        Callback ->
            {reply, {ok, Callback}, State}
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
handle_cast({register_handler, Handle, Callback}, State = #state{handlers = Handlers}) ->
    Callback0 =
        case Callback of
            Callback when is_function(Callback) -> Callback;
            {Module, Function} -> fun Module:Function/4
        end,
    case proplists:get_value(Handle, Handlers) of
        undefined ->
            ?DEBUG("Registered handler ~p", [Handle]),
            {reply, ok, State#state{handlers = [{Handle, Callback0}|Handlers]}};
        _ ->
            ?ERROR("Could not register handler ~p since there's already another one registered on that name", [Handle]),
            {reply, {error, already_exists}, State}
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
