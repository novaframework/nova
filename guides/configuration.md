# Configuration

Nova is configured through the application environment, normally in *sys.config*. This page lists every key Nova reads.

## Cowboy configuration

Nova uses Cowboy as the webserver. The listener is configured with a map under the `cowboy_configuration` key of the `nova` application:

```erlang
{nova, [
    {cowboy_configuration, #{port => 8080}}
]}
```

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `stream_handlers` | The Cowboy stream handlers, in order. `nova_stream_h` sets the `server` header and issues the session cookie. A list of your own replaces the default, so keep `nova_stream_h` in it when you use sessions | `list()` | `[nova_stream_h, cowboy_compress_h, cowboy_stream_h]` |
| `middleware_handlers` | The Cowboy middlewares, in order. The default is the Nova request pipeline, see [Request lifecycle](request-lifecycle.md). A list of your own replaces it | `list()` | `[nova_router, nova_plugin_handler, nova_security_handler, nova_handler, nova_plugin_handler]` |
| `options` | Cowboy protocol options, see [`cowboy_http`](https://ninenines.eu/docs/en/cowboy/2.21/manual/cowboy_http/). Nova sets `stream_handlers`, `middlewares` and its own keys in `env` | `map()` | `#{compress => true}` |
| `ip` | IP to bind to | `tuple` | `{0, 0, 0, 0}` |
| `port` | Port to bind to | `integer()` | `8080` |
| `use_ssl` | Serve TLS on `ssl_port` instead of plain HTTP on `port`. There is one listener | `boolean()` | `false` |
| `ssl_options` | Transport options for the TLS listener, as a map. Nova passes them to `ranch_ssl`, see [its manual](https://ninenines.eu/docs/en/ranch/2.4/manual/ranch_ssl/). A TLS listener needs at least a certificate and a key, for instance `#{certfile => "/path/to/fullchain.pem", keyfile => "/path/to/privkey.pem"}` | `map()` | `#{}` |
| `ssl_port` | Port to bind to when using SSL | `integer()` | `8443` |
| `ca_cert` | Path to CA-cert *Deprecated since 0.10.3 - Replaced with `ssl_options`*| `string()` | `undefined` |
| `cert` | Path to cert *Deprecated since 0.10.3 - Replaced with `ssl_options`*| `string()` | `undefined` |

The listener is started last, after everything a request needs is running. If it cannot be started, for instance because the port is in use or a certificate cannot be read, Nova logs `Cowboy could not start` and fails to start with `{listener_not_started, Reason}`. Earlier releases logged the error and kept running without a listener.

The listener belongs to ranch, so it is still there when `nova_sup` goes down without the `nova` application being stopped, for instance when `nova_sup` runs under a supervisor of your own. When `nova_sup` starts again it stops that listener and binds a new one. A listener named `nova_listener` that Nova did not start is left alone and stops the boot.


## Nova specific configurations

Following parameters should be defined under the `nova`-key in your *sys.config*.

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `use_persistent_term` | When `false`, the compiled routing table is also added to the Cowboy environment under `dispatch` | `boolean()` | `true` |
| `dispatch_backend` | Module that stores the routing table and the plugin list. It has to export `get/1`, `get/2` and `put/2` as `persistent_term` does. Anything but a module name logs `dispatch_backend must be a module` and the default is used | `module()` | `persistent_term` |
| `use_stacktrace` | If Nova should include the stacktrace in crash logs and, in the `dev` environment, on error pages. Read when the listener starts. What is redacted from both is listed under *Redaction in logs and error pages*. | `boolean()` | `false` |
| `redact_keys` | Extra key fragments whose values are redacted from crash logs and error pages, on top of the built-in list. Atoms, binaries or strings; a key matches when its name contains the fragment, see *Redaction in logs and error pages*. | `[atom() \| binary() \| string()]` | `[]` |
| `render_error_pages` | With `false`, the `404` and `500` pages that come with Nova send the bare status code to every client, without a body. The status code routes of your application run either way. See [Error handling](error-handling.md#the-default-pages) | `boolean()` | `true` |
| `use_sessions` | Turn off/on support for sessions | `boolean()` | `true` |
| `session_manager` | Specify a module to use as the session manager. Defaults to `nova_session_ets`, which keeps sessions in memory: they do not survive a restart, and with several nodes and no sticky sessions users are signed out when they reach another node. Use a shared session store for those cases; see the [Sessions](sessions.md) guide. | `atom()` | `nova_session_ets` |
| `session_cookie_name` | Name of the session cookie. It must be an RFC 6265 token: ASCII letters, digits and the symbols a token allows such as `-`, `_` and `.`, but no spaces, `:`, `/`, `@`, `=` or quotes. Nova refuses to start with any other name, including an empty binary or a value that is not a binary. `<<"__Host-session_id">>` is recommended for new applications; see the [Sessions](sessions.md) guide. | `binary()` | `<<"session_id">>` |
| `session_cookie_opts` | Options for the session cookie, merged over the defaults. The cookie is `Secure` unless `{nova, [{environment, dev}]}` is set explicitly; an unset `environment` counts as not `dev` here, although `nova:get_environment/0` returns `dev` for it. Developing over plain HTTP without that setting loses the session cookie. `path` and `domain` must be ASCII without control characters or `;`, or Nova refuses to start. See the [Sessions](sessions.md) guide. | `cow_cookie:cookie_opts()` | `#{path => <<"/">>, http_only => true, same_site => lax, secure => <not dev>, max_age => <session_max_age>}` |
| `session_max_age` | Seconds a session lives after it was created. Sets the cookie's `Max-Age` and expires `nova_session_ets` entries. `infinity` turns the limit off. | `pos_integer() \| infinity` | `1209600` (14 days) |
| `max_sessions` | Sessions the `nova_session_ets` backend holds at most, on each node. In a full table a write that starts a session drops another one to make room: an expired one, then a provisional one, then the one nearest its expiry. A provisional write returns `{error, session_store_full}` when only sessions in use are left. `infinity` turns the limit off. See the [Sessions](sessions.md#when-the-session-store-is-full) guide. | `non_neg_integer() \| infinity` | `1000000` |
| `use_strict_routing`    | If the routing module should work under the strict mode. Using strict mode will cause errors if non-deterministic paths are detected. This is a beta-function so use with caution. | `boolean()` | `false` |
| `environment` | The environment Nova runs in. It is passed to the `routes/1` callback of every router and returned by `nova:get_environment/0`. When it is not set it is read as `dev` there, so routes a router only returns for `dev` are served, and Nova logs a warning at boot: set it in every release, usually to `prod`. The session cookie and the error pages only treat it as `dev` when it is set to `dev` (`nova:is_dev_environment/0`). | `atom()` | not set, read as `dev` |
| `bootstrap_application` | Define which application to bootstrap with Nova. This should be the name of your application. | `atom()` | *Will crash if not defined* |
| `cowboy_configuration` | The listener configuration, see *Cowboy configuration* above. | `map()` | `#{}` |
| `plugins` | The global plugins, a list of `{Phase, Module, Options}`. They run for every route group that declares no `plugins` of its own. See the [Plugins](plugins.md) guide. | `[{atom(), module(), map()}]` | `[]` |
| `plugins_after_security` | `pre_request` plugin modules that run after the route's security callback and not before it, for route groups that do not set the key themselves. The security callback of such a route cannot read what those plugins produce (`json`, `params`, `files`). See the [Plugins](plugins.md#running-a-plugin-after-the-security-callback) guide. | `[module()]` | `[]` |
| `multipart_spool` | Bounds on the upload files spooled at one time by `nova_multipart_file_handler`: `max_files` and `max_bytes`, each an integer or `infinity`. A part past a bound gets a `503`. See the [Plugins](plugins.md#nova-multipart) guide. | `map()` | `#{max_files => infinity, max_bytes => 8000000000}` |
| `telemetry_request_fields` | Request fields to add to the controller telemetry metadata. `path` (the raw request path) and `peer` (the client's address) are the only ones accepted and are left out by default. See the [Telemetry](telemetry.md) guide. | `[path \| peer]` | `[]` |
| `shutdown_delay` | Milliseconds to wait before suspending the listener during shutdown. Useful for letting load balancers drain traffic. See the [Graceful shutdown](graceful-shutdown.md) guide. | `integer()` | `0` |
| `shutdown_drain_timeout` | Maximum milliseconds to wait for active connections to finish during shutdown. See the [Graceful shutdown](graceful-shutdown.md) guide. | `integer()` | `15000` |

### Where Nova reads a setting

`json_lib`, `use_sessions`, `session_cookie_name`, `session_cookie_opts`, `session_max_age`, `max_sessions` and `redact_keys` are read from the bootstrap application's environment first, then from the `nova` environment, then the default. A value set in your own application always wins:

```erlang
[{nova, [{bootstrap_application, my_app}]},
 {my_app, [{json_lib, json}, {use_sessions, true}]}].
```

The other keys in the table above are only read from `nova`.

### Redaction in logs and error pages

Crash reasons, plugin and controller results, and stacktraces can hold request data. Before Nova writes them to a log line or a crash report, or renders them on an error page, it:

- replaces every Cowboy request map with its method and path, so headers, cookies, the query string and the body are left out;
- replaces the argument list of every stacktrace frame with its arity;
- replaces the value of every sensitive key with `redacted`, in maps and in lists of `{Key, Value}` pairs such as proplists and header or query string lists.

A key is sensitive when its name contains one of these fragments: `passw`, `secret`, `token`, `apikey`, `authorization`, `cookie`, `session`, `csrf`, `privatekey`, `credential`, or a fragment listed in `redact_keys`. Two more match as whole names only: `qs` (so the query string of the partial request a websocket's `terminate/3` gets is redacted too) and `otp`. Keys can be atoms, binaries or strings. Names are compared in lower case with `-` and `_` removed, so `access_token`, `X-Api-Key`, `clientSecret`, `password_confirmation` and `Set-Cookie` are all covered. Only the first 64 bytes of a name are looked at.

A fragment in `redact_keys` matches everywhere it occurs: `pin` also redacts `shipping`. Prefer the longest fragment that still covers your keys.

A `{Key, Value}` tuple that is not an element of a list has the same shape as an error tuple, so its value is redacted only when the whole name is one of the fragments above, or is `password`, `session_id`, `set-cookie`, `_csrf_token`, `x-csrf-token`, `qs` or `otp`: `{token, T}` is redacted, `{invalid_token, Reason}` is not.

Anything else is logged as is: a secret held under another key, or in a plain tuple or a binary, still reaches the log. Add such keys to `redact_keys`. Values your own code logs are not touched, unless the logger formats with `nova_jsonlogger` and its `redact` option is set: `{nova_jsonlogger, #{redact => true}}` applies the same rules to every report and its metadata, and to the arguments of an event logged as a format string with arguments, before they are put into the text. A secret that is text already is not found: a message logged as a plain string, the format string itself, and anything inside a binary, such as the unparsed bytes of a request in a crash report. The option is off by default.

## Application parameters

These parameters are set in the environment of your own application, not under `nova`. `watchers` is only read from the application named in `bootstrap_application`. `router_module` and `nova_apps` are read from every application Nova mounts.

| Key | Description | Value |
|-----|-------------|-------|
| `json_lib` | JSON lib to use. Read more in the subsection *Configure json lib* | `atom()` |
| `watchers` | Watchers are external programs that will run together with Nova, in every environment. Each one is a tuple `{Command}`, `{Command, Arguments}`, `{Command, Options}`, `{App, Command, Options}` or `{App, Command, Arguments, Options}` (Like `[{my_app, "npm", ["run", "watch"], #{workdir => "priv/assets/js/my-app"}}]`). See the [Watchers](watchers.md) guide. | `[{string()}] | [{string(), [string()]}] | [{string(), map()}] | [{atom(), string(), map()}] | [{atom(), string(), [string()], map()}]` |
| `router_module` | Module that contains the `routes/1` callback. Defaults to the application name followed by `_router`, such as `my_app_router` | `atom()` |
| `nova_apps` | Other Nova applications to serve from the same listener, each an application name or `{Name, Options}`. See [Multiple applications](multi-app.md) | `[atom() \| {atom(), map()}]` |

Nova writes a key `apps` to the environment of the bootstrap application for its own use. Do not set it.


### Configure json_lib

One can configure which json library to use for encoding/decoding json structures. The default is `thoas`. Set `json_lib` in your bootstrap application's environment; Nova also reads it from the `nova` environment when the bootstrap application does not set it.

```erlang
{my_app, [{json_lib, json}]}
```

The module defined for this should expose two different functions:

`encode(Structure) -> binary() | iolist()`

`decode(JsonString) -> {ok, Structure} | {error, Reason} | Structure`

`decode/1` may return `{ok, Structure}` (like `thoas`) or the structure itself and raise on bad input (like OTP's `json` module), so `{json_lib, json}` works without an adapter.


## Handling errors in Nova

A request that fails is answered with a status page. Which page, what it
contains and how to replace it is described in
[Error handling](error-handling.md). The settings involved are
`render_error_pages`, `use_stacktrace` and `environment` in the table above.
