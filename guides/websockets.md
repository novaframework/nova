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

The upgrade is an HTTP request. It passes [the origin check](#the-origin-check),
the `pre_request` plugins and the security callback of the route group like
any other request, so a refused upgrade never reaches the controller. See
[Routing](routing.md).

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
| `auth_data` | The `auth_data` of the upgrade request: what the security callback returned in `{true, AuthData}`, or what a plugin wrote there when the callback returned `true` or the route has no callback. Not there otherwise |

It returns `{ok, State}` to accept the connection, or `{ok, State, Options}`
with a map of
[`cowboy_websocket` options](https://ninenines.eu/docs/en/cowboy/2.21/manual/cowboy_websocket/)
such as `idle_timeout` and `max_frame_size`. Any other return refuses the
upgrade with a `500` and is logged as an error. `State` is a map and is what
the other callbacks get.

A client that sent no session cookie gets one on the `101` response, and so
does a session written in `init/1`. See
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
is closed. A callback that returns something that is not in the table is
logged as `Websocket handler failed on what the controller returned`, and
the connection is closed as well. The one exception is a bare `ok`, which is
still taken as `{ok, State}` with the state unchanged.

## Authentication

The security callback of the route group runs on the upgrade request. Its
`AuthData` is in the controller data:

```erlang
init(#{auth_data := #{user_id := UserId}} = ControllerData) ->
    {ok, ControllerData#{user_id => UserId}}.
```

The `auth_data` key of the controller data is the `auth_data` of the upgrade
request, which is also under `req` there, so both are the same value. It is
what the security callback returned as `{true, AuthData}`, whatever a plugin
wrote. What a plugin wrote under `auth_data` is there only when the callback
returned a plain `true`, or the route has no callback, see
[Plugins](plugins.md#before-and-after-the-security-callback).

## The origin check

A browser sends the cookies of your site with an upgrade that a page on
another site started. Nova therefore checks where every request to a
websocket route comes from, before anything else of the route runs. A
request that is not allowed is answered with `403` and the text
`Origin not allowed`. No plugin and no security callback sees it.

The check reads two request headers that a page cannot set:

| The request has | Result |
|---|---|
| No `Origin`, and `Sec-Fetch-Site` is `same-origin`, `none` or not sent | Let through. A client that is not a browser sends neither header |
| An `Origin`, and `Sec-Fetch-Site` is `same-origin`, `none` or not sent | The `Origin` decides, by `trusted_origins` |
| A `Sec-Fetch-Site` with any other value, such as `cross-site` or `same-site` | Refused, unless the `Origin` is in a list of `trusted_origins`, or `trusted_origins` is `any` |

The check goes by `trusted_origins` of the `nova` environment:

```erlang
{nova, [
    {trusted_origins, [~"https://example.com"]}
]}
```

| `trusted_origins` | Accepts |
|---|---|
| `same_host` | An origin with the host and port the request was sent to. Over TLS the origin has to be `https`. On a listener without TLS the scheme is not compared, and no forwarded header is read. This is the default |
| `same_origin` | An origin with the scheme, host and port of the request as Nova received it |
| A list of origins | Exactly those origins, also from a page the browser says is on another site. Use it behind a proxy that rewrites the `Host` header or terminates TLS |
| `any` | Every origin, whatever `Sec-Fetch-Site` says |

The [Configuration](configuration.md#trusted-origins) guide says how ports are
compared. `nova_csrf_plugin` reads the same setting, except that it takes
`any` of the `nova` environment as `same_host`: `any` there opens websockets
and nothing else.

To give one route group other origins, list `nova_ws_origin_plugin` in its
plugins with the option `trusted_origins`. The group is then checked against
those alone. The check still runs first, wherever in the list the plugin is:

```erlang
#{prefix => "/widget",
  security => fun my_app_auth:session/1,
  plugins => [
      {pre_request, nova_ws_origin_plugin, #{trusted_origins => [~"https://example.com",
                                                                 ~"https://partner.example.org"]}}
  ],
  routes => [
      {"/ws", my_app_widget_ws_controller, #{protocol => ws}}
  ]}
```

A route group that declares `plugins` uses those and not the global ones, so
list every other plugin the group needs with it. The option takes the values
of the setting. Any other value is refused when the routes are compiled, and
the application does not start, with
`{invalid_setting, trusted_origins, Value}`.

`same_host` and `same_origin` do not stop DNS rebinding, where another name is
pointed at the address of the server: the origin and the host of such a
request are the same name. A list of origins does.

### Behind a proxy that terminates TLS

The listener of Nova is then one without TLS. `same_host` does not compare
the scheme there, and `same_origin` compares it with `http`, which is not the
scheme of the site.

A page loaded from `http://example.com` has the host of a site served at
`https://example.com`, and it is not the site: anybody on the network between
a visitor and the server can answer for `http://example.com` and write that
page, unless the browser already knows by HSTS that the host is only to be
asked over TLS. Such a page that opens `wss://example.com` sends the cookies
of the visitor with the upgrade.

A browser that sends `Sec-Fetch-Site` with the upgrade says that this page is
another site, and the upgrade is refused. Not every browser sends the header
with a websocket upgrade: Firefox does and Safari does from 16.4, while
Chrome and the browsers built on it have been reported not to. A browser that
does not send it is taken by its `Origin` alone, and under `same_host` that
page is let in: it can open the websocket as the visitor and read what the
websocket sends. So the header narrows this risk and does not end it.

To close it for every browser:

- List the origins of the site, with their scheme, in `trusted_origins`:
  `{trusted_origins, [~"https://example.com"]}`.
- Send `Strict-Transport-Security` with `includeSubDomains` from the proxy,
  so that a browser that has been to the site does not load a page of it
  without TLS at all.

Nova reads no `X-Forwarded-Proto` or `Forwarded` header to find out what the
proxy did, since it has no setting that says which proxy to believe.

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

The `pre_request` plugins of the route run on the upgrade request: their
`pre_security/4` ahead of the security callback and their `pre_request/4`
behind it, see [Plugins](plugins.md#before-and-after-the-security-callback).
After the upgrade a websocket route has three phases of its own:

| Phase | Runs |
|---|---|
| `pre_ws_request` | Before every `websocket_init/1`, `websocket_handle/2` and `websocket_info/2` call |
| `post_ws_request` | After every such call that does not close the connection |
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

Nova emits a span around each `websocket_init/1`, `websocket_handle/2` and
`websocket_info/2` call, and one for the lifetime of a connection. See [Telemetry](telemetry.md).
