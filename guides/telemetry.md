# Telemetry

Nova emits [`telemetry`](https://hex.pm/packages/telemetry) events for the
work it does on top of Cowboy: controllers, plugins, websocket controllers and
pubsub broadcasts. Attach a handler to turn them into metrics, traces or logs.
[opentelemetry_nova](https://github.com/novaframework/opentelemetry_nova) is
built on these events.

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

> #### Do not log `req` wholesale {: .warning}
>
> The `req` in controller metadata is the full Cowboy request map. It carries
> headers, including `authorization` and `cookie`. Pick the fields a handler
> needs and never log or export the whole map.

## Conventions

- Measurements are in `native` time units. Convert with
  `erlang:convert_time_unit/3`.
- `start` events carry `monotonic_time` and `system_time`.
- `stop` and `exception` events carry `duration` and `monotonic_time`.
- Every event of a span carries the same `telemetry_span_context`, so a
  handler can pair a `start` with its `stop` or `exception`.
- `exception` events add `kind`, `reason` and `stacktrace` to the start
  metadata.
- `route` is the matched route template, as described in
  [The matched route](routing.md#the-matched-route). It is `undefined` when no
  route matched, for example on a `404` rendered by the router.

Metadata never contains plugin options or plugin state, websocket frame
payloads or pubsub payloads. Those can hold secrets.

The catalogue below is a public contract. New metadata keys may be added in a
minor release; renaming or removing an event or a key is a breaking change.

## Controller

`[nova, controller, start | stop | exception]`

A span around the controller function and the handling of what it returned.
Plugins and the security callback are outside it.

| Event | Measurements | Metadata |
|-------|--------------|----------|
| `start` | `monotonic_time`, `system_time` | `app`, `controller`, `action`, `route`, `req` |
| `stop` | `duration`, `monotonic_time` | start metadata, `status`; `req` is the request after the controller ran |
| `exception` | `duration`, `monotonic_time` | start metadata, `kind`, `reason`, `stacktrace` |

- `controller` is the module and `action` the function name of the route's
  callback.
- `status` is the response status code.
- A controller that throws `{Status, Reason}` is asking Nova for a status
  page. That ends the span with `stop` and `status => Status`, not with
  `exception`.
- An error page Nova renders after a controller is a controller span of its
  own, with `app => nova`. When a controller crashes, the failing span ends
  with `exception` and the `500` page follows as a separate span. When a
  controller returns something Nova cannot handle, the error page span is
  nested inside the controller span.
- A `404` or `405` rendered by the router is a controller span for the status
  page, with `route => undefined`.

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
- `result` is `ok`, `break` or `stop`: the first element of what the plugin
  returned.
- A crashing plugin still renders a `500` page after the `exception` event.

## Websocket

`[nova, websocket, handle, start | stop | exception]`

A span around each call into a websocket controller.

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

- `reason` is the termination reason Cowboy passes to `terminate/3`.

## Pubsub

`[nova, pubsub, broadcast]`

A single event for each `nova_pubsub:broadcast/3` and
`nova_pubsub:local_broadcast/3`.

| Measurements | Metadata |
|--------------|----------|
| `receivers` | `channel`, `topic`, `scope` |

- `receivers` is the number of processes the message was sent to.
- `scope` is `global` for `broadcast/3` and `local` for `local_broadcast/3`.
