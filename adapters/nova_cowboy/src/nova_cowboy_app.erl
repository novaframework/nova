-module(nova_cowboy_app).
-moduledoc false.

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    nova_cowboy_sup:start_link().

%% Before the supervisor goes: the listeners stop taking connections, the
%% open ones are waited for, and then the listeners are stopped.
-spec prep_stop(State) -> State.
prep_stop(State) ->
    ok = nova_listener_owner:shutdown(nova_cowboy),
    State.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
