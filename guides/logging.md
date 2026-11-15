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

Nova logs the same way: every message is a map with a `msg` key, or for a
few older messages a `reason` or `action` key, and the details under keys of
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
| `meta_with` | A list of metadata keys. Only these are written next to the report. `level` is one of them |
| `meta_without` | A list of metadata keys to leave out. `[report_cb]` by default |
| `redact` | Apply the redaction rules below to every report, its metadata and the arguments of a format string. `false` by default |

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

`logger_metadata_key` is an atom, as every key of the logger metadata is.
Without the option the id is logged under the atom `'correlation-id'`. A
binary is taken as the atom of that name when the node knows one. When it
does not, the id is logged under `'correlation-id'` and the error
`logger_metadata_key of nova_correlation_plugin is a binary that names no atom this node knows ...`
is logged once.

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

A `csrf_secret` that Nova refuses at boot is not written anywhere: the log
entry and the reason `nova_sup` fails with have `redacted` in its place.

This covers what Nova logs. What your application and other libraries log is
only covered when the handler uses `nova_jsonlogger` with `redact => true`.
The formatter then applies the rules to:

* every report and its metadata,
* the arguments of an event logged as a format string with arguments, such
  as `?LOG_ERROR("login failed: ~p", [Params])`, and of the format reports
  of `error_logger`. The arguments are redacted before they are put into the
  text. When they no longer fit the format string, the format string and the
  arguments are printed as a term.

It does not find a secret that is text already:

* a message logged as a plain string, and the format string itself,
* anything inside a binary or a string, such as the unparsed bytes of a
  request that a crash report holds,
* a value under a key that does not look sensitive.

Those are logged as they are.

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
| `The environment key of the nova application is not set and is read as prod: ...` | notice | `environment` is not set. Routers get `prod` and nothing meant for development is on. The report has `key` and `read_as`. Set `{environment, dev}` for development |
| `non_canonical_paths must be reject or resolve`, `allowed_origins must be same_host, same_origin, any or a list of origins`, `csrf_token must be stateless or session`, `csrf_secret must be a binary of at least 32 bytes, or a list of such`, `max_sessions must be infinity or an integer above 0`, `plugins_before_security must be all or a list of plugin modules that have pre_request/4` | error | A [setting checked at boot](configuration.md#settings-checked-at-boot) has a value that cannot be used. The report has the `setting` and the `value`, which is `redacted` for `csrf_secret`. Nova does not start. The one for `allowed_origins` is also logged for the option of `nova_csrf_plugin` and `nova_ws_origin_plugin`, with the `app`, the `prefix` and the `plugin` |
| `csrf_token is stateless and csrf_secret is not set` | error | Signed tokens are asked for and there is no secret. Nova does not start |
| `plugins_before_security of a route group must be all or a list of its pre_request plugins that have pre_request/4` | error | The key of a route group names a module that is not one of its `pre_request` plugins or has no `pre_request/4`. The report has the `app`, the `prefix` and the `value`. Nova does not start |
| `plugins_after_security must be a list of plugin modules` | error | The key of a route group or of the `nova` environment is not a list of module names. Nova does not start |
| `csrf_secret is not set, so nova_csrf_plugin keeps its tokens in the session store and starts a session for every visitor. ...` | notice | `nova_csrf_plugin` is used and neither `csrf_secret` nor `csrf_token` is set. See [CSRF token modes](plugins.md#csrf-token-modes) |
| `The security callback of this route group runs before nova_request_plugin reads the body, ...` | warning | See [Warnings about the order](plugins.md#warnings-about-the-order). The report has the `app`, the `prefix` and the `plugins` |
| `pre_request/4 of these plugins runs once the security callback of this route group has let a request through, ...` | warning | The same |
| `pre_request/4 of these plugins does not run for a CORS preflight to a route of this group that does not take OPTIONS. ...` | warning | The same |
| `You need to define bootstrap_application option in configuration` | error | `bootstrap_application` is missing. Nova does not start |
| `Cowboy could not start` | error | The listener could not be started, for instance because the port is in use. Nova does not start |
| `Could not start cowboy with SSL` | error | The TLS listener could not be started, for instance because a certificate cannot be read. Nova does not start |
| `A security value must be false, a fun/1 or {Mod, Fun}.` | error | A `security`, `secure` or `override_secure` has another value. Nova does not start |
| `This key is not read here, so it protects nothing. ...` | warning | A route group has `secure => false`, the options of a route have `security => false` or `secure => false`, or a `nova_apps` entry has `security => false`. The report has the `app`, the `path` and the `key`. Remove the key |
| `This key is not read here, so the callback would protect nothing. ...` | error | The same keys with any other value. Nova does not start. Declare the callback as `security` on the route group, or as `secure` on the `nova_apps` entry |
| `Application is mounted by two nova_apps entries that ask for different security callbacks. ...` | error | Two `nova_apps` entries mount the application named under `app`, with different `secure` or `override_secure`. Nova does not start |
| `Application has an entry for itself in its nova_apps, and is compiled with its options` | notice | The application that starts the listener lists itself in its own `nova_apps`. The report has the `app` and the keys of the entry under `options` |
| `Could not find the router module of the application` | error | An application has no `router_module` set and its `<app>_router` module is not in the code path. Nova does not start |
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
| `Request path is not canonical` | debug | The path has an empty, `.` or `..` segment, or no leading slash. The request got `400`. See [Routing](routing.md#paths-that-are-not-canonical) |
| `Request error in controller`, `Request error in plugin` | warning | The client sent something that could not be parsed, or sent the body too slowly. The request got `400`, `408` or `413` |
| `Controller refused to redirect to a location that is not a local path`, `Security callback refused to redirect ...`, `Plugin refused to redirect ...` | warning | `{redirect_local, Path}` was given something that is not a path on the site. The request got `400` |
| `Controller redirected to an invalid location` | error | `{redirect, Location}` was given something that cannot be a `location` header |
| `The request was answered already, so this response is not sent. ...` | warning | A request that had its response was answered again, and the second response was dropped. A controller, a plugin or a security callback answered with `cowboy_req` and did not hand back the request that call returned. Logged once per request. The report has the `status` of the dropped response and the `method`, and not the controller |
| `Plugin replied after the response was sent. The reply is dropped` | warning | A plugin returned `stop` with a reply for a request that was answered already. Logged once per plugin |
| `Stacktraces are disabled. Call nova:use_stacktrace(true) ...` | warning | In the `dev` environment, a `500` page was rendered for a crash that was caught without its stacktrace. Logged once per node |
| `The response had begun when the controller failed, so no error page is sent` | warning | A controller or a handler failed after it began to stream the response. The client keeps what was sent |
| `Nova could not render template` | error | The template of a view does not exist. The request got the `500` page |
| `Websocket handler failed on what the controller returned` | error | A websocket callback returned something that is not one of its [return values](websockets.md#return-values). The connection is closed. The report has the `controller` and the `function` |
| `Template could not find application` | warning | A `url` tag names an application Nova has not compiled, or none. It rendered `#` |
| `Static directory cannot be served` | error | The directory of a static route is gone, cannot be read or cannot be resolved. Logged once per directory, with the first request under it. The request got a `404` |
| `Static file cannot be served` | error | The file of a static route to a single file is gone or cannot be read. Logged once per file. The request got a `404` |
| `Invalid nova_request_plugin option, using the default` | warning | `max_body_length` or `body_read_timeout` has a value that is not valid |
| `nova_multipart_plugin needs handler => {Mod, InitArgs}.` | error | The plugin is configured without a handler |
| `logger_metadata_key of nova_correlation_plugin is a binary that names no atom this node knows. ...` | error | The option is a binary that is not the name of an existing atom. The id is logged under `'correlation-id'`. Logged once per key, with the first request |
| `Deprecation warning. Since version: ...` | warning | Something [deprecated](deprecations.md) is used |

### Sessions

| Message | Level | Meaning |
|---|---|---|
| `Session manager does not implement session_exists/1, so presented session ids are accepted unchecked` | warning | The session backend cannot tell if an id exists, so a client can choose its own session id. Implement `session_exists/1` |
| `Session cookie not set: session_cookie_name or session_cookie_opts is invalid` | error | The cookie settings were changed to something invalid while Nova runs. Clients get no session cookie |
| `Request carried more than one session cookie, treated as no session ...` | warning | A cookie of the same name is set for a parent domain or another path. Use a `__Host-` cookie name |
| `Session store is full. Sessions are dropped to make room for new ones, provisional ones first, and a provisional one is refused when there is none to drop` | warning | `max_sessions` is reached, and a session that had not expired was dropped or a write was refused. Logged at most once a minute, with the number of `sessions` and `max_sessions`. See [Sessions](sessions.md#when-the-session-store-is-full) |
| `Session backend failed to look up a session` | warning | `session_exists/1` of the session backend raised an exception. The presented session id is treated as unknown |
| `Session called but the 'use_sessions' option is set to false` | error | `nova_session` was called with sessions turned off |
