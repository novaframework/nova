-module(nova_roadrunner_sup).
-moduledoc false.

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% init/1 never answers ignore, which the application callback has no
%% place for.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% nova_listener_owner is not restarted, and the supervisor goes when it
%% does: it stops because `nova` went down under it, and the application
%% goes too.
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_one, auto_shutdown => any_significant},
          [#{id => nova_listener,
             start => {nova_listener_owner, start_link, [nova_roadrunner]},
             restart => temporary,
             significant => true,
             shutdown => 5000}]}}.
