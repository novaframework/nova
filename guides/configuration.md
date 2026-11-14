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

The listener belongs to ranch, so it is still there when `nova_sup` goes down without the `nova` application being stopped, for instance when `nova_sup` runs under a supervisor of your own. When `nova_sup` starts again it stops that listener and binds a new one. A listener named `nova_listener` that Nova did not start is left alone and stops the boot.


## Nova specific configurations

Following parameters should be defined under the `nova`-key in your *sys.config*.

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `use_persistent_term` | Use `persistent_term` module to store routing tree | `boolean()` | `true` |
| `use_stacktrace` | If Nova should include stacktrace in error-pages | `boolean()` | `false` |
| `render_error_pages` | If Nova should render error-pages for HTML-request | `boolean()` | `true` |
| `use_sessions` | Turn off/on support for sessions | `boolean()` | `true` |
| `session_manager` | Specifify a module to use as the session manager. Defaults to `nova_session_ets` | `atom()` | `nova_session_ets` |
| `use_strict_routing`    | If the routing module should work under the strict mode. Using strict mode will cause errors if non-deterministic paths are detected. This is a beta-function so use with caution. | `boolean()` | `false` |
| `bootstrap_application` | Define which application to bootstrap with Nova. This should be the name of your application. | `atom()` | *Will crash if not defined* |
| `cowboy_configuration` | If you need some additional configuration done to Cowboy this is the place. Check `nova_sup` module to learn which keys that can be defined. | `map()` | `#{}` |
| `shutdown_delay` | Milliseconds to wait before suspending the listener during shutdown. Useful for letting load balancers drain traffic. See the [Graceful shutdown](graceful-shutdown.md) guide. | `integer()` | `0` |
| `shutdown_drain_timeout` | Maximum milliseconds to wait for active connections to finish during shutdown. See the [Graceful shutdown](graceful-shutdown.md) guide. | `integer()` | `15000` |

## Application parameters

These parameters can be specified in your *main* application (Eg the one you've specified in the `bootstrap`-section).

| Key | Description | Value |
|-----|-------------|-------|
| `json_lib` | JSON lib to use. Read more in the subsection *Configure json lib* | `atom()` |
| `watchers` | Watchers are external programs that will run together with Nova, in every environment. Each one is a tuple `{Command}`, `{Command, Arguments}`, `{Command, Options}`, `{App, Command, Options}` or `{App, Command, Arguments, Options}` (Like `[{my_app, "npm", ["run", "watch"], #{workdir => "priv/assets/js/my-app"}}]`). See the [Watchers](watchers.md) guide. | `[{string()}] | [{string(), [string()]}] | [{string(), map()}] | [{atom(), string(), map()}] | [{atom(), string(), [string()], map()}]` |
| `router_module` | Module that contains the `routes/1` callback | `atom()` |


### Configure json_lib

One can configure which json library to use for encoding/decoding json structures. The module defined for this should expose two different functions:

`encode(Structure) -> binary() | iolist()`

`decode(JsonString) -> {ok, Structure}`


## Handling errors in Nova

Nova will by default render a error page if an error occurs. This page will be rendered using the `nova_error`-template. This template can be overridden by defining a template with the same name in your application.
By defauly Nova outputs a lot of information, including the stacktrace. This might not be a good approach in production. To turn off stacktraces in production you can add the following to your *sys.config*:

```erlang
{nova, [{render_error_pages, false}]}
```

This will exclude stacktrace from the error page.

*Note* Nova is aware about which `accept`-headers the request is sent with and will respond with the correct content-type. If the request is sent with `application/json` Nova will respond with a JSON-structure instead of a HTML-page.
