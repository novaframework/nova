-module(nova_websocket).
-moduledoc """
Behaviour for websocket controllers. Route to one with `protocol => ws`:

```erlang
{"/ws", my_ws_controller, #{protocol => ws}}
```

`init/1` runs in the request process before the upgrade. It gets the
controller data (with `req` set to the upgrade request, and `auth_data` set
to the `auth_data` of that request when it has one, which is what a
controller of an HTTP route finds in its request) and returns
`{ok, State}` to accept the connection. Any other return refuses the upgrade
and is logged as an error; the client gets a `500`, rendered by the status
page route for `500` when the application has one.

After the upgrade, `websocket_init/1`, `websocket_handle/2` and
`websocket_info/2` return one of:

- `{ok, State}`, `{ok, State, hibernate}`
- `{reply, Frame | [Frame], State}`, `{reply, Frame | [Frame], State, hibernate}`
- `{stop, State}`, which closes the connection

`terminate/3` is optional. It is called with the reason, the partial request
and the state when the connection goes away. A crash in it is logged and does
not stop the `post_ws_connection` plugins from running.

A route can hand the controller a term of its own with `extra_state`, set on
the route or on its route group. It is found in the upgrade request, under
`req` in the controller data:

```erlang
{"/ws", my_ws_controller, #{protocol => ws, extra_state => #{room => lobby}}}
```

The upgrade request runs the route's `pre_request` plugins like any HTTP
request. `nova_ws_handler` has plugin phases of its own for the time after
the upgrade: `pre_ws_request` and `post_ws_request` run around every
`websocket_init/1`, `websocket_handle/2` and `websocket_info/2` call, and
`post_ws_connection` runs once when the connection goes away, after
`terminate/3` and whether or not the controller exports it. It does not run
for an upgrade that was refused. A plugin exports the phase as a
function of `(ControllerState, State, Options)` and returns
`{ok, ControllerState, State}`, `{break, State}`, `{stop, State}` or
`{error, Reason}`.

```erlang
-module(my_ws_controller).
-behaviour(nova_websocket).
-export([init/1, websocket_handle/2, websocket_info/2]).

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, ~"ping"}, State) ->
    {reply, {text, ~"pong"}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({broadcast, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Msg, State) ->
    {ok, State}.
```
""".

-doc """
What `websocket_init/1`, `websocket_handle/2` and `websocket_info/2` return.

- `{ok, State}` sends nothing.
- `{reply, Frame, State}` sends one frame, or each frame of a list in order.
- `{stop, State}` closes the connection.

With `hibernate` as the last element the connection process hibernates until
the next frame or message. Any other return is logged as an error and closes
the connection.
""".
-type call_result() :: {ok, State :: map()} |
                       {ok, State :: map(), hibernate} |
                       {reply, OutFrame :: cow_ws:frame() | [OutFrame :: cow_ws:frame()],
                        State :: map()} |
                       {reply, OutFrame :: cow_ws:frame() | [OutFrame :: cow_ws:frame()],
                        State :: map(), hibernate} |
                       {stop, State :: map()}.
-export_type([call_result/0]).

-doc "A frame from the client, as `websocket_handle/2` gets it.".
-type in_frame() :: ping | pong | {text | binary | ping | pong, binary()}.
-export_type([in_frame/0]).

-doc """
Why a connection went away, as `terminate/3` gets it. The values are those of
`cowboy_websocket`: `remote` when the client closed the connection, `stop`
when a callback returned `{stop, State}`, `timeout` when the connection was
idle for too long, `{error, Reason}` for a socket or protocol error, and
`{crash, Class, Reason}` when the connection process crashed.
""".
-type reason() ::  normal | stop | timeout | remote |
                   {remote, cow_ws:close_code(), binary()} |
                   {error, badencoding | badframe | closed | atom()} |
                   {crash, error | exit | throw, any()}.
-export_type([reason/0]).

-doc """
The options of a connection, as `init/1` returns them in
`{ok, State, Options}`: `idle_timeout`, `max_frame_size` and `compress`, and
whatever else the server package takes.
""".
-type opts() :: nova_server:ws_opts().
-export_type([opts/0]).

-doc """
Called in the request process, before the upgrade, with the controller data:
a map with the upgrade request under `req`, and under `auth_data` the
`auth_data` of that request when it has one: what the security callback of
the route returned in `{true, AuthData}`, or what a plugin wrote there.

`{ok, State}` accepts the connection, and `{ok, State, Options}` accepts it
with the given `t:opts/0`. `State` is what the other callbacks get. Any other
return refuses the upgrade: it is logged as an error and the client gets the
`500` page.
""".
-callback init(State :: map()) -> {ok, State0 :: map()} |
                                  {ok, State0 :: map(), WebsocketOptions :: opts()} |
                                  any().
-doc """
Called in the connection process once the connection is open, with the state
`init/1` returned and the pid of the connection process under
`ws_handler_process`. Optional. Use it for what belongs to the connection
process, such as joining a `nova_pubsub` channel.
""".
-callback websocket_init(State :: map()) -> Result :: call_result().
-optional_callbacks([websocket_init/1]).

-doc """
Called for every frame the client sends. A crash is logged as
`Controller crashed` and closes the connection.
""".
-callback websocket_handle(Frame :: in_frame(), State :: map()) -> call_result().
-doc """
Called for every Erlang message the connection process receives, a
`nova_pubsub` broadcast for instance. A crash is logged as
`Controller crashed` and closes the connection.
""".
-callback websocket_info(Info :: any(), State :: map()) -> call_result().
-doc """
Called when the connection goes away, with the reason, what is left of the
upgrade request and the state. Optional. The return value is not used, and a
crash is logged and does not stop the `post_ws_connection` plugins.
""".
-callback terminate(Reason :: reason(), PartialReq :: map(), State :: map()) -> ok.
-optional_callbacks([terminate/3]).
