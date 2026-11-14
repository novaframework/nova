# Configuration

There's a lot of parameters that can be configured in Nova. This document will try to explain them all.

## Cowboy configuration

Nova uses Cowboy as the webserver. Cowboy is a very flexible webserver and Nova tries to expose as much of this flexibility as possible. This means that you can configure Cowboy in a lot of different ways. The configuration is done in the `nova`-application under `cowboy_configuration`-key in your *sys.config*.

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `stream_handlers` | Stream handlers are used to handle streaming requests. You can configure multiple stream handlers. Read more in the subsection *Stream handlers* | `list()` | `[nova_stream_h, cowboy_compress_h, cowboy_stream_h]` |
| `middleware_handlers` | Middleware handlers are used to handle middleware requests. You can configure multiple middleware handlers. Read more in the subsection *Middleware handlers* | `list()` | `[nova_router, nova_plugin_handler, nova_security_handler, nova_handler, nova_plugin_handler]` |
| `options` | Cowboy options. Read more in the subsection *Cowboy options* | `map()` | `#{compress => true}` |
| `ip` | IP to bind to | `tuple` | `{0, 0, 0, 0}` |
| `port` | Port to bind to | `integer()` | `8080` |
| `use_ssl` | If SSL should be used | `boolean()` | `false` |
| `ssl_options` | Transport options for SSL. Nova uses ranch_ssl module so read about available options on [their page](https://ninenines.eu/docs/en/ranch/2.0/manual/ranch_ssl/). | `ranch_ssl:opts()` | `#{cert => "/path/to/fullchain.pem", key => "/path/to/privkey.pem"}` |
| `ssl_port` | Port to bind to when using SSL | `integer()` | `8443` |
| `ca_cert` | Path to CA-cert *Deprecated since 0.10.3 - Read with `ssl_options`*| `string()` | `undefined` |
| `cert` | Path to cert *Deprecated since 0.10.3 - Replaced with `ssl_options`*| `string()` | `undefined` |

The listener is started last, after everything a request needs is running. If it cannot be started, for instance because the port is in use or a certificate cannot be read, Nova logs `Cowboy could not start` and fails to start with `{listener_not_started, Reason}`. Earlier releases logged the error and kept running without a listener.


## Nova specific configurations

Following parameters should be defined under the `nova`-key in your *sys.config*.

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `use_persistent_term` | Use `persistent_term` module to store routing tree | `boolean()` | `true` |
| `use_stacktrace` | If Nova should include stacktrace in error-pages and crash logs. What is redacted from both is listed under *Redaction in logs and error pages*. | `boolean()` | `false` |
| `redact_keys` | Extra key fragments whose values are redacted from crash logs and error pages, on top of the built-in list. Atoms, binaries or strings; a key matches when its name contains the fragment, see *Redaction in logs and error pages*. | `[atom() \| binary() \| string()]` | `[]` |
| `render_error_pages` | If Nova should render error-pages for HTML-request | `boolean()` | `true` |
| `use_sessions` | Turn off/on support for sessions | `boolean()` | `true` |
| `session_manager` | Specify a module to use as the session manager. Defaults to `nova_session_ets`, which keeps sessions in memory: they do not survive a restart, and with several nodes and no sticky sessions users are signed out when they reach another node. Use a shared session store for those cases; see the [Sessions](sessions.md) guide. | `atom()` | `nova_session_ets` |
| `session_cookie_name` | Name of the session cookie. It must be an RFC 6265 token: ASCII letters, digits and the symbols a token allows such as `-`, `_` and `.`, but no spaces, `:`, `/`, `@`, `=` or quotes. Nova refuses to start with any other name, including an empty binary or a value that is not a binary. `<<"__Host-session_id">>` is recommended for new applications; see the [Sessions](sessions.md) guide. | `binary()` | `<<"session_id">>` |
| `session_cookie_opts` | Options for the session cookie, merged over the defaults. The cookie is `Secure` unless `{nova, [{environment, dev}]}` is set explicitly; an unset `environment` counts as not `dev` here, although `nova:get_environment/0` returns `dev` for it. Developing over plain HTTP without that setting loses the session cookie. `path` and `domain` must be ASCII without control characters or `;`, or Nova refuses to start. See the [Sessions](sessions.md) guide. | `cow_cookie:cookie_opts()` | `#{path => <<"/">>, http_only => true, same_site => lax, secure => <not dev>, max_age => <session_max_age>}` |
| `session_max_age` | Seconds a session lives after it was created. Sets the cookie's `Max-Age` and expires `nova_session_ets` entries. `infinity` turns the limit off. | `pos_integer() \| infinity` | `1209600` (14 days) |
| `max_sessions` | Sessions the `nova_session_ets` backend holds at most. A write that would start one more returns `{error, session_store_full}`. `infinity` turns the limit off. See the [Sessions](sessions.md) guide. | `non_neg_integer() \| infinity` | `1000000` |
| `use_strict_routing`    | If the routing module should work under the strict mode. Using strict mode will cause errors if non-deterministic paths are detected. This is a beta-function so use with caution. | `boolean()` | `false` |
| `environment` | The environment Nova runs in. It is passed to the `routes/1` callback of every router and returned by `nova:get_environment/0`. When it is not set it is read as `dev` there, so routes a router only returns for `dev` are served, and Nova logs a warning at boot: set it in every release, usually to `prod`. The session cookie and the error pages only treat it as `dev` when it is set to `dev` (`nova:is_dev_environment/0`). | `atom()` | not set, read as `dev` |
| `bootstrap_application` | Define which application to bootstrap with Nova. This should be the name of your application. | `atom()` | *Will crash if not defined* |
| `cowboy_configuration` | If you need some additional configuration done to Cowboy this is the place. Check `nova_sup` module to learn which keys that can be defined. | `map()` | `#{}` |
| `plugins_after_security` | `pre_request` plugin modules that run after the route's security callback and not before it, for route groups that do not set the key themselves. The security callback of such a route cannot read what those plugins produce (`json`, `params`, `files`). See the [Plugins](plugins.md#running-a-plugin-after-the-security-callback) guide. | `[module()]` | `[]` |
| `multipart_spool` | Bounds on the upload files spooled at one time by `nova_multipart_file_handler`: `max_files` and `max_bytes`, each an integer or `infinity`. A part past a bound gets a `503`. See the [Plugins](plugins.md#nova-multipart) guide. | `map()` | `#{max_files => 1024, max_bytes => 8000000000}` |
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

Anything else is logged as is: a secret held under another key, or in a plain tuple or a binary, still reaches the log. Add such keys to `redact_keys`. Values your own code logs are not touched, unless the logger formats with `nova_jsonlogger` and its `redact` option is set: `{nova_jsonlogger, #{redact => true}}` applies the same rules to every report and its metadata. The option is off by default.

## Application parameters

These parameters can be specified in your *main* application (Eg the one you've specified in the `bootstrap`-section).

| Key | Description | Value |
|-----|-------------|-------|
| `json_lib` | JSON lib to use. Read more in the subsection *Configure json lib* | `atom()` |
| `watchers` | Watchers are external programs that will run together with Nova, in every environment. Each one is a tuple `{Command}`, `{Command, Arguments}`, `{Command, Options}`, `{App, Command, Options}` or `{App, Command, Arguments, Options}` (Like `[{my_app, "npm", ["run", "watch"], #{workdir => "priv/assets/js/my-app"}}]`). See the [Watchers](watchers.md) guide. | `[{string()}] | [{string(), [string()]}] | [{string(), map()}] | [{atom(), string(), map()}] | [{atom(), string(), [string()], map()}]` |
| `router_module` | Module that contains the `routes/1` callback | `atom()` |


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

Nova will by default render a error page if an error occurs. This page will be rendered using the `nova_error`-template. This template can be overridden by defining a template with the same name in your application.
By defauly Nova outputs a lot of information, including the stacktrace. This might not be a good approach in production. To turn off stacktraces in production you can add the following to your *sys.config*:

```erlang
{nova, [{render_error_pages, false}]}
```

This will exclude stacktrace from the error page.

The details of a crash (the message describing what went wrong and the stacktrace) are only part of the error page, HTML or JSON, when `nova`'s `environment` is set to `dev`. With any other value, and when `environment` is not set at all, the page only has its title and a generic message. `nova:get_environment/0` returns `dev` for an unset `environment`, but the error page does not go by that: it wants the explicit setting.

*Note* Nova is aware about which `accept`-headers the request is sent with and will respond with the correct content-type. If the request is sent with `application/json` Nova will respond with a JSON-structure instead of a HTML-page.
