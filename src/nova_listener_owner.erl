-module(nova_listener_owner).
-moduledoc """
The process a server package puts in its supervision tree to run Nova's
listeners on its server.

```erlang
#{id => nova_listener,
  start => {nova_listener_owner, start_link, [my_server]}}
```

It starts the listeners of the configuration as it starts, and a listener
that cannot be started stops the application of the package from starting.
The listeners stop when it does. They also stop when the `nova` application
goes down under them, since nothing they hand a request to is there any
longer: the process then exits, and takes the application of the package
with it when its supervisor allows no restarts.

`shutdown/0` is for the `prep_stop/1` of that application: it stops the
listeners the slow way, waiting for the connections that are open.
""".

-behaviour(gen_server).

-export([start_link/1, shutdown/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-spec start_link(Server :: module()) -> {ok, pid()} | {error, term()}.
start_link(Server) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Server, []).

-doc "Stop taking connections, wait for the open ones, and stop the listeners.".
-spec shutdown() -> ok.
shutdown() ->
    nova_sup:shutdown_listeners().

-doc false.
-spec init(module()) -> {ok, reference()} | {error, term()}.
init(Server) ->
    process_flag(trap_exit, true),
    Monitor = monitor(process, nova_sup),
    case nova_sup:start_listeners(Server) of
        ok -> {ok, Monitor};
        {error, Reason} -> {error, Reason}
    end.

-doc false.
-spec handle_call(term(), gen_server:from(), State) -> {reply, ok, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-doc false.
-spec handle_cast(term(), State) -> {noreply, State}.
handle_cast(_Request, State) ->
    {noreply, State}.

-doc false.
-spec handle_info(term(), State) -> {noreply, State} | {stop, {shutdown, nova_stopped}, State}.
handle_info({'DOWN', Monitor, process, _Pid, _Reason}, Monitor) ->
    {stop, {shutdown, nova_stopped}, Monitor};
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) ->
    nova_sup:stop_listeners().
