-module(nova_listener_owner).
-moduledoc """
The process a server package puts in its supervision tree to run Nova's
listeners on its server.

```erlang
{ok, {#{strategy => one_for_one, auto_shutdown => any_significant},
      [#{id => nova_listener,
         start => {nova_listener_owner, start_link, [my_server]},
         restart => temporary,
         significant => true}]}}
```

It starts the listeners of the configuration as it starts, and a listener
that cannot be started stops the application of the package from starting.
The listeners stop when it does. They also stop when the `nova` application
goes down under them, since nothing they hand a request to is there any
longer: the process then exits, and as a significant child that is not
restarted it takes its supervisor and the application of the package with
it.

`shutdown/1` is for the `prep_stop/1` of that application: it stops the
listeners the slow way, waiting for the connections that are open.
""".

-behaviour(gen_server).

-export([start_link/1, shutdown/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-spec start_link(Server :: module()) -> gen_server:start_ret().
start_link(Server) ->
    gen_server:start_link(?MODULE, Server, []).

-doc "Stop taking connections, wait for the open ones, and stop the listeners of the server.".
-spec shutdown(Server :: module()) -> ok.
shutdown(Server) ->
    nova_sup:shutdown_listeners(Server).

-doc false.
-spec init(module()) -> {ok, {module(), reference()}} | {error, term()}.
init(Server) ->
    process_flag(trap_exit, true),
    Monitor = monitor(process, nova_sup),
    case nova_sup:start_listeners(Server) of
        ok -> {ok, {Server, Monitor}};
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
-spec handle_info(term(), State) -> {noreply, State} | {stop, {shutdown, nova_stopped}, State}
              when State :: {module(), reference()}.
handle_info({'DOWN', Monitor, process, _Pid, _Reason}, {_Server, Monitor} = State) ->
    {stop, {shutdown, nova_stopped}, State};
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
-spec terminate(term(), {module(), reference()}) -> ok.
terminate(_Reason, {Server, _Monitor}) ->
    nova_sup:stop_listeners(Server).
