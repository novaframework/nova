# Logging and observability

Nova logs through the OTP `logger` and emits `telemetry` events. This guide
shows how to get structured logs, how to tie log lines to a request, what
Nova keeps out of the logs and what its own log messages mean.

## Logging from your application

Use the `logger` macros with a map as the report:

```erlang
-module(my_app_orders_controller).
-export([create/1]).

-include_lib("kernel/include/logger.hrl").

create(#{json := Order}) ->
    ?LOG_INFO(#{msg => ~"Order received", items => length(maps:get(~"items", Order, []))}),
    {json, #{status => ~"received"}}.
```

Nova logs the same way: every message is a map with a `msg` key, or for some
older messages a `reason` or `action` key, and the details under keys of
their own.

## JSON logs

`nova_jsonlogger` is a `logger` formatter that writes one JSON object per
line. Set it as the formatter of a handler in *sys.config*:

```erlang
{kernel, [
    {logger_level, info},
    {logger, [
        {handler, default, logger_std_h,
         #{formatter => {nova_jsonlogger, #{new_line => true, redact => true}}}}
    ]}
]}
```

| Option | Description |
|---|---|
| `new_line` | End every event with a newline. `false` by default |
| `new_line_type` | `nl`, `crlf`, `cr`, `unix`, `windows` or `macos9` |
| `key_mapping` | A map that renames keys of the output, such as `#{level => severity}` |
| `format_funs` | A map from a key to a function that formats its value |
| `redact` | Apply the redaction rules below to every report and its metadata. `false` by default |

The report and the logger metadata are merged into one object.

## Correlation ids

`nova_correlation_plugin` gives every request an id and adds it to the logger
metadata of the request process, so every line logged while the request is
served carries it:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_correlation_plugin, #{request_correlation_header => ~"x-correlation-id",
                                                 logger_metadata_key => correlation_id}}
    ]}
]}
```

The id is also in the `x-correlation-id` response header and in the request
under `correlation_id`, for passing on to the services a controller calls.
See [Nova correlation](plugins.md#nova-correlation).

## Redaction

Crash reasons and stacktraces can hold request data. Before Nova writes them
to a log, or shows them on an error page, it:

* replaces a Cowboy request map with its method and path,
* replaces the argument list of a stacktrace frame with its arity,
* replaces the value of a key that looks sensitive, such as `password`,
  `authorization`, `cookie` or `access_token`, with `redacted`.

`redact_keys` adds key fragments of your own. The rules are listed in
[Configuration](configuration.md#redaction-in-logs-and-error-pages).

This covers what Nova logs. What your application and other libraries log is
only covered when the handler uses `nova_jsonlogger` with `redact => true`.
A secret under a key that does not look sensitive, or inside a binary, is
logged as it is.

## Telemetry and tracing

Nova emits `telemetry` events for controllers, plugins, websocket callbacks
and pubsub broadcasts, see [Telemetry](telemetry.md). The metadata has the
route template, the application and the status code, and never the request.
[opentelemetry_nova](https://github.com/novaframework/opentelemetry_nova)
turns the events into OpenTelemetry traces and metrics.

## Log messages

The messages an operator is most likely to meet. `msg` is the text to search
for.

### Start and stop

| Message | Level | Meaning |
|---|---|---|
| `Starting nova` | notice | Nova is starting, with the `environment` it uses |
| `Nova is running` | notice | The listener is accepting connections |
| `The nova environment is not set and is read as dev ...` | warning | `environment` is not set. Routers get `dev`. Set it in every release |
| `You need to define bootstrap_application option in configuration` | error | `bootstrap_application` is missing. Nova does not start |
| `Cowboy could not start` | error | The listener could not be started, for instance because the port is in use. Nova does not start |
| `Could not start cowboy with SSL` | error | The TLS listener could not be started, for instance because a certificate cannot be read. Nova does not start |
| `A security value must be false, a fun/1 or {Mod, Fun}.` | error | A `security`, `secure` or `override_secure` has another value. Nova does not start |
| `Conflicting route` | warning | Two routes have the same path and method. The first is kept. With `use_strict_routing` it is an error and Nova does not start |
| `Unknown plugin_strategy, falling back to local_or_global` | error | The `plugin_strategy` of a `nova_apps` entry is not one Nova knows |
| `plugin_strategy on a route entry is ignored ...` | warning | `plugin_strategy` belongs in the `nova_apps` entry of the application |
| `dispatch_backend must be a module` | error | `dispatch_backend` is not a module name. `persistent_term` is used |
| `Another handler is already registered on that name` | error | `nova_handlers:register_handler/2` was called for a name that is taken. The handler that was there is kept |
| `Could not start watcher` | error | A watcher could not be started and is skipped |
| `Graceful shutdown started`, `Suspending listeners`, `Draining connections`, `Stopping listeners` | notice | The steps of a [graceful shutdown](graceful-shutdown.md) |
| `Drain timeout reached` | warning | Connections were still open after `shutdown_drain_timeout` and are closed |

### Requests

| Message | Level | Meaning |
|---|---|---|
| `Controller crashed` | error | A controller, or the handler for what it returned, raised an exception. The request got the `500` page |
| `Controller threw an exception` | error | A controller threw `{StatusCode, Reason}`. The request got the page for that status |
| `Status page crashed` | error | A status code route raised an exception |
| `Plugin crashed` | error | A plugin raised an exception. The request got the `500` page |
| `Security handler crashed` | error | A security callback raised an exception. The request got the `500` page |
| `Request error in controller`, `Request error in plugin` | warning | The client sent something that could not be parsed, or sent the body too slowly. The request got `400`, `408` or `413` |
| `Controller refused to redirect to a location that is not a local path` | warning | `{redirect_local, Path}` was given something that is not a path on the site. The request got `400` |
| `Controller redirected to an invalid location` | error | `{redirect, Location}` was given something that cannot be a `location` header |
| `Nova could not render template` | error | The template of a view does not exist. The request got a `404` |
| `Invalid nova_request_plugin option, using the default` | warning | `max_body_length` or `body_read_timeout` has a value that is not valid |
| `nova_multipart_plugin needs handler => {Mod, InitArgs}.` | error | The plugin is configured without a handler |
| `Deprecation warning. Since version: ...` | warning | Something [deprecated](deprecations.md) is used |

### Sessions

| Message | Level | Meaning |
|---|---|---|
| `Session manager does not implement session_exists/1, so presented session ids are accepted unchecked` | warning | The session backend cannot tell if an id exists, so a client can choose its own session id. Implement `session_exists/1` |
| `Session cookie not set: session_cookie_name or session_cookie_opts is invalid` | error | The cookie settings were changed to something invalid while Nova runs. Clients get no session cookie |
| `Request carried more than one session cookie, treated as no session ...` | warning | A cookie of the same name is set for a parent domain or another path. Use a `__Host-` cookie name |
| `Session store is full, new sessions are refused` | warning | `max_sessions` is reached |
| `Session called but the 'use_sessions' option is set to false` | error | `nova_session` was called with sessions turned off |
