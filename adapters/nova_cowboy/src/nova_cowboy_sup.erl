-module(nova_cowboy_sup).
-moduledoc false.

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Without restarts: when nova_listener_owner goes, because `nova` went
%% down under it, the application goes too.
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 0},
          [#{id => nova_listener,
             start => {nova_listener_owner, start_link, [nova_cowboy]},
             shutdown => 5000}]}}.
