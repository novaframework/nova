%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2021, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2021 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_cache_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         init_cache/1,
         remove_cache/1,
         get_cache/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Initializes the cache.
%% @end
%%--------------------------------------------------------------------
-spec init_cache(Cachename :: atom()) -> {ok, Child} |
                                         {ok, Child, Info :: term()} |
                                         {error, {already_started, pid()} | {shutdown, term()} | term()}
                                             when Child :: undefined | pid().
init_cache(Cachename) when is_atom(Cachename) ->
    supervisor:start_child(?SERVER, child_spec(Cachename, nova_cache)).

%%--------------------------------------------------------------------
%% @doc
%% Removes a cache. This terminates the corresponding process.
%% @end
%%--------------------------------------------------------------------
-spec remove_cache(Cachename :: atom()) -> ok | {error, Reason :: not_found | simple_one_for_one}.
remove_cache(Cachename) when is_atom(Cachename) ->
    case get_cache(Cachename) of
        {ok, Id} ->
            %% Let's terminate the child
            supervisor:terminate_child(?SERVER, Id);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns the pid of a cache.
%% @end
%%--------------------------------------------------------------------
-spec get_cache(Cachename :: atom()) -> {ok, Id :: pid()} | {error, not_found}.
get_cache(Cachename) when is_atom(Cachename) ->
    Children = supervisor:which_children(?SERVER),
    case lists:keyfind(Cachename, 1, Children) of
        false ->
            {error, not_found};
        {_Id, Child, _Type, _Modules} ->
            {ok, Child}
    end.
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},



    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child_spec(Id, Module) ->
    #{id => Id,
      start => {Module, start_link, [Id]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Module]}.
