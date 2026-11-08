# Telemetry

Nova emits [`telemetry`](https://hex.pm/packages/telemetry) events for the
work it does on top of Cowboy: controllers, plugins, websocket controllers and
pubsub broadcasts. Attach a handler to turn them into metrics, traces or logs.
Instrumentation libraries such as
[opentelemetry_nova](https://github.com/novaframework/opentelemetry_nova) can
consume them.

```erlang
ok = telemetry:attach(my_app_controller_stop,
                      [nova, controller, stop],
                      fun my_app_metrics:handle_event/4,
                      #{}).
```

```erlang
handle_event([nova, controller, stop], #{duration := Duration}, Metadata, _Config) ->
    #{app := App, route := Route, status := Status} = Metadata,
    Ms = erlang:convert_time_unit(Duration, native, millisecond),
    my_metrics:observe(App, Route, Status, Ms).
```

Nova does not emit request-level events (`[nova, request, *]`). The HTTP
request as a whole belongs to Cowboy; use
[cowboy_telemetry](https://hex.pm/packages/cowboy_telemetry) or a Cowboy stream
handler for it.

## Conventions

- Measurements are in `native` time units. Convert with
  `erlang:convert_time_unit/3`.
- `start` events carry `monotonic_time` and `system_time`.
- `stop` and `exception` events carry `duration` and `monotonic_time`.
- Every event of a span carries the same `telemetry_span_context`, so a
  handler can pair a `start` with its `stop` or `exception`.
- `exception` events add `kind`, `reason` and `stacktrace` to the start
  metadata. Argument lists in the stacktrace are replaced by their arity, so
  a crash never exports the request, plugin options or state, or a frame
  through it. Nova's own crash handling still sees the original stacktrace.
- `route` is the matched route template, as described in
  [The matched route](routing.md#the-matched-route). It is `undefined` when no
  route matched, for example on a `404` rendered by the router.

Metadata never contains the request map, request or response bodies, plugin
options or plugin state, websocket frame payloads or pubsub payloads. Those
can hold credentials (`authorization` and `cookie` headers, decoded form and
JSON bodies, uploaded files) and other secrets.

The catalogue below is a public contract. New metadata keys may be added in a
minor release; renaming or removing an event or a key is a breaking change.

## Controller

`[nova, controller, start | stop | exception]`

A span around the controller function and the handling of what it returned.
Plugins and the security callback are outside it.

| Event | Measurements | Metadata |
|-------|--------------|----------|
| `start` | `monotonic_time`, `system_time` | `app`, `controller`, `action`, `route`, `status_page`, `method`, `path`, `host`, `scheme`, `version`, `peer` |
| `stop` | `duration`, `monotonic_time` | start metadata, `status` |
| `exception` | `duration`, `monotonic_time` | start metadata, `kind`, `reason`, `stacktrace` |

- `controller` is the module and `action` the function name of the route's
  callback.
- `method`, `path`, `host`, `scheme`, `version` and `peer` are copied from
  the Cowboy request. `path` is the raw path; use `route` as a label.
- `status` is the response status code.
- A controller that raises `{Status, Reason}` with an integer `Status`, as a
  throw, error or exit, is asking Nova for a status page. That ends the span
  with `stop` and `status => Status`, not with `exception`.
- `status_page` is `true` for the span of an error page Nova renders after a
  controller failed, and `false` otherwise. Those spans have `app => nova`.
  - A crash emits `exception` for the controller, followed by a `stop` for
    the `500` page with `status_page => true`.
  - A status raised as `{Status, Reason}` emits `stop` for the controller,
    followed by a `stop` for the status page when a route exists for that
    status code. Nova routes `404` and `500` itself.
  - A controller that returns something Nova cannot handle emits the status
    page `stop` nested inside its own span.

  One request can therefore produce two controller spans. To count requests,
  filter out `status_page => true`.
- A `404` or `405` rendered by the router and the `500` after a plugin crash
  are the only controller span of their request. They have
  `status_page => false`, and `route` is `undefined` for `404` and `405`.
  A request the security callback rejects never reaches a controller and
  emits no controller span.

## Plugin

`[nova, plugin, start | stop | exception]`

A span around each `pre_request` and `post_request` plugin call.

| Event | Measurements | Metadata |
|-------|--------------|----------|
| `start` | `monotonic_time`, `system_time` | `plugin`, `callback`, `app`, `route` |
| `stop` | `duration`, `monotonic_time` | start metadata, `result` |
| `exception` | `duration`, `monotonic_time` | start metadata, `kind`, `reason`, `stacktrace` |

- `plugin` is the plugin module.
- `callback` is `pre_request` or `post_request`.
- `result` is `ok`, `break` or `stop`, the first element of what the plugin
  returned, or `invalid` when the plugin returned anything else.
- A crashing plugin still renders a `500` page after the `exception` event.

## Websocket

`[nova, websocket, handle, start | stop | exception]`

A span around each `websocket_init/1`, `websocket_handle/2` and
`websocket_info/2` call into a websocket controller. The controller's
`init/1`, which runs before the upgrade, and `terminate/3` are not wrapped.

| Event | Measurements | Metadata |
|-------|--------------|----------|
| `start` | `monotonic_time`, `system_time` | `callback`, `module`, `app`, `route` |
| `stop` | `duration`, `monotonic_time` | start metadata |
| `exception` | `duration`, `monotonic_time` | start metadata, `kind`, `reason`, `stacktrace` |

- `callback` is `websocket_init`, `websocket_handle` or `websocket_info`.
- `module` is the websocket controller.
- A crash in the controller closes the connection. The `exception` event is
  emitted before it does.

`[nova, websocket, connection, start | stop]`

The lifetime of a websocket connection, from `websocket_init` to `terminate`.

| Event | Measurements | Metadata |
|-------|--------------|----------|
| `start` | `monotonic_time`, `system_time` | `module`, `app`, `route` |
| `stop` | `duration`, `monotonic_time` | start metadata, `reason` |

- `reason` is the tag of the termination reason Cowboy passes to
  `terminate/3`: the reason itself when it is an atom (`normal`, `stop`,
  `timeout`, `remote`), otherwise the first element of the tuple (`remote`
  for `{remote, Code, Payload}`, `error`, `crash`), or `unknown`. The rest of
  the reason is left out because it can carry what the client sent.

## Pubsub

`[nova, pubsub, broadcast]`

A single event for each `nova_pubsub:broadcast/3` and
`nova_pubsub:local_broadcast/3`.

| Measurements | Metadata |
|--------------|----------|
| `receivers` | `channel`, `topic`, `scope` |

- `receivers` is the number of processes the message was sent to.
- `scope` is `global` for `broadcast/3` and `local` for `local_broadcast/3`.
- `topic` is whatever the caller passed and is unbounded, so it is not safe
  as a metric label.
