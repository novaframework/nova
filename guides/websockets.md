# Websockets

A websocket endpoint is a module with the `nova_websocket` behaviour. The
router upgrades the request and the module handles the frames of the
connection.

## Routing

A websocket route names the module and sets `protocol => ws`:

```erlang
#{prefix => "/",
  security => false,
  routes => [
      {"/ws", my_app_ws_controller, #{protocol => ws}}
  ]}
```

The upgrade is an HTTP request. It passes the `pre_request` plugins and the
security callback of the route group like any other request, so a refused
upgrade never reaches the controller. See [Routing](routing.md).

## The controller

```erlang
-module(my_app_ws_controller).
-behaviour(nova_websocket).
-export([init/1, websocket_handle/2, websocket_info/2]).

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, ~"ping"}, State) ->
    {reply, {text, ~"pong"}, State};
websocket_handle({text, ~"bye"}, State) ->
    {stop, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({broadcast, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Msg, State) ->
    {ok, State}.
```

### Callbacks

| Callback | Called |
|---|---|
| `init/1` | In the request process, before the upgrade. Required |
| `websocket_init/1` | In the connection process, once the connection is open. Optional |
| `websocket_handle/2` | For every frame the client sends. Required |
| `websocket_info/2` | For every Erlang message the connection process receives. Required |
| `terminate/3` | When the connection goes away, with the reason, the partial request and the state. Optional |

`init/1` gets the controller data, a map with:

| Key | Value |
|---|---|
| `req` | The upgrade request. The `extra_state` of the route, or of its route group, is in it, and so is `auth_data` |
| `auth_data` | What the security callback returned in `{true, AuthData}`. Not there when the callback returned `true` or the route has no callback |

It returns `{ok, State}` to accept the connection, or `{ok, State, Options}`
with a map of
[`cowboy_websocket` options](https://ninenines.eu/docs/en/cowboy/2.21/manual/cowboy_websocket/)
such as `idle_timeout` and `max_frame_size`. Any other return refuses the
upgrade with a `500` and is logged as an error. `State` is a map and is what
the other callbacks get.

A session written in `init/1` gets its cookie on the `101` response. See
[Sessions](sessions.md).

### Return values

`websocket_init/1`, `websocket_handle/2` and `websocket_info/2` return one
of:

| Return | Effect |
|---|---|
| `{ok, State}` | Nothing is sent |
| `{ok, State, hibernate}` | As above, and the process hibernates |
| `{reply, Frame, State}`, `{reply, [Frame], State}` | Send one frame, or several |
| `{reply, Frame, State, hibernate}` | As above, and the process hibernates |
| `{stop, State}` | Close the connection |

A frame is `{text, Binary}`, `{binary, Binary}`, `ping`, `pong`,
`{ping, Binary}`, `{pong, Binary}`, `close` or `{close, Code, Reason}`.

A callback that crashes is logged as `Controller crashed` and the connection
is closed.

## Authentication

The security callback of the route group runs on the upgrade request. Its
`AuthData` is in the controller data:

```erlang
init(#{auth_data := #{user_id := UserId}} = ControllerData) ->
    {ok, ControllerData#{user_id => UserId}}.
```

A browser sends the cookies of your site with an upgrade that a page on
another site started. A websocket that trusts the session cookie should check
the `Origin` header with `nova_ws_origin_plugin`:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_ws_origin_plugin, #{allowed_origins => [~"https://example.com"]}}
    ]}
]}
```

| `allowed_origins` | Accepts |
|---|---|
| `same_host` | An origin with the host and port of the request. This is the default |
| `same_origin` | An origin with the scheme, host and port of the request |
| A list of origins | Exactly those origins. Use it behind a proxy that rewrites the `Host` header or terminates TLS |

An upgrade from another origin is answered with `403`. A request without an
`Origin` header is let through, since only browsers send it. The plugin is
not active unless you configure it.

## Sending to a connection

The connection process receives Erlang messages in `websocket_info/2`. Send
to it from anywhere to push a frame to the client. The pid of the connection
process is `self()` in `websocket_init/1` and the callbacks after it, and is
in the state under `ws_handler_process`. `init/1` runs in another process.

### Pubsub

Join a `nova_pubsub` channel in `websocket_init/1` and forward what is
broadcast on it:

```erlang
-module(my_app_chat_controller).
-behaviour(nova_websocket).
-export([init/1, websocket_init/1, websocket_handle/2, websocket_info/2]).

-include_lib("nova/include/nova_pubsub.hrl").

init(ControllerData) ->
    {ok, ControllerData}.

websocket_init(State) ->
    ok = nova_pubsub:join(chat),
    {ok, State}.

websocket_handle({text, Text}, State) ->
    ok = nova_pubsub:broadcast(chat, "message", Text),
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(#nova_pubsub{topic = "message", payload = Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Msg, State) ->
    {ok, State}.
```

Every connection, on every connected node, gets what any of them sends. See
[Pubsub](pubsub.md).

## Plugins

The `pre_request` plugins of the route run on the upgrade request. After the
upgrade a websocket route has three phases of its own:

| Phase | Runs |
|---|---|
| `pre_ws_request` | Before every `websocket_init/1`, `websocket_handle/2` and `websocket_info/2` call |
| `post_ws_request` | After every such call |
| `post_ws_connection` | Once when the connection goes away, after `terminate/3`, whether or not the controller exports it. Not for an upgrade that was refused |

A plugin exports the phase as a function of
`(ControllerState, State, Options)`. `ControllerState` is a map with
`module`, `function` and `arguments`, and `State` is the state of the
connection, with the state of the controller under `controller_data`. It
returns:

| Return | Effect |
|---|---|
| `{ok, ControllerState, State}` | Carry on with the next plugin |
| `{break, State}` | Skip the remaining plugins of the phase |
| `{stop, State}` | Close the connection |
| `{error, Reason}` | Log the reason and close the connection |

A plugin that crashes is logged as `Plugin crashed` and the connection is
closed.

## Telemetry

Nova emits a span around each controller callback and one for the lifetime of
a connection. See [Telemetry](telemetry.md).
