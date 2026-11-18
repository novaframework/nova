# Servers and listeners

Nova has no HTTP server of its own. It runs on a server package, and that
package starts the listeners of the node: where Nova accepts connections,
and which HTTP versions each of them speaks.

## Choosing a server

| Package | Server | HTTP versions | WebSockets over |
|---|---|---|---|
| `nova_cowboy` | [Cowboy](https://github.com/ninenines/cowboy) | HTTP/1.1 and HTTP/2 | HTTP/1.1, and HTTP/2 when turned on |
| `nova_roadrunner` | [Roadrunner](https://github.com/arizona-framework/roadrunner) | HTTP/1.1, HTTP/2 and HTTP/3 | HTTP/1.1 |

Add one of them to the dependencies and to the `applications` of your
application, next to `nova`:

```erlang
{deps, [nova, nova_cowboy]}.
```

```erlang
{applications, [kernel, stdlib, nova, nova_cowboy]}
```

The listeners start when the application of the server package starts, and
stop when it stops. A node without a server package starts and serves
nothing.

With one server package in the release there is nothing more to say. With
both, `nova_cowboy` is used unless the `server` key names the other:

```erlang
{nova, [{server, nova_roadrunner}]}
```

Controllers, plugins and WebSocket controllers that read and answer through
`nova_req` run on either server. What differs is listed with each package,
in `nova_cowboy` and `nova_roadrunner`.

## Listeners

A listener is configured under a name of your choice, in the `listeners` key
of the `nova` application:

```erlang
{nova, [
    {bootstrap_application, my_app},
    {listeners, #{
        public => #{port => 8443,
                    tls => #{certfile => "cert.pem", keyfile => "key.pem"},
                    protocols => [http1, http2],
                    applications => [my_app]},
        admin => #{ip => {127, 0, 0, 1},
                   port => 9090,
                   applications => [my_admin]}
    }}
]}
```

| Setting | Description | Default |
|---|---|---|
| `port` | The port to bind | `8080`, or `8443` with `tls` |
| `ip` | The address to bind | `{0, 0, 0, 0}` |
| `tls` | The options of the `ssl` application for a server, as a map or a list. A listener without it is not encrypted | `false` |
| `protocols` | The HTTP versions the listener serves, out of `http1`, `http2` and `http3`. `{http2, Options}` gives a version options of the server | `[http1, http2]` with `tls`, `[http1]` without |
| `applications` | The Nova applications whose routes the listener serves, each with what its `nova_apps` mounts | The bootstrap application, for the only listener of a node |
| `steps` | The steps a request goes through, see [Request lifecycle](request-lifecycle.md) | Those of `nova_pipeline` |
| `server_opts` | Options of the server package, as it documents them | `#{}` |

A node without `listeners` has one listener named `default`, on port `8080`,
that serves the bootstrap application.

Each listener has a routing table of its own. An application that two
listeners list is served by both.

HTTP/1.1 and HTTP/2 share the TCP port. HTTP/3 is served over UDP on the
same port number and needs `tls`. Roadrunner tells clients of HTTP/1.1 and
HTTP/2 about it in an `alt-svc` header, which is how a browser finds it.

## What is checked at boot

A listener that cannot do what it is asked for, or a route that could not be
reached, stops the node from starting. The application of the server package
fails with the reason, and an error in the log names the listener or the
route.

| Reason | What it says |
|---|---|
| `{unknown_listener_settings, Name, Keys}` | The listener has settings Nova does not know, such as those of `cowboy_configuration` |
| `{invalid_listener_setting, Name, Setting, Value}` | A setting has a value it cannot take |
| `{listeners_on_one_port, Ip, Port}` | Two listeners bind the same address and port |
| `{listener_protocols_not_supported, Name, Protocols, Server}` | The server does not serve these versions, as Cowboy does not serve `http3` |
| `{listener_protocols_need_tls, Name, Protocols}` | The server serves these versions with `tls` alone, as every server does `http3` |
| `{listener_without_tls_serves_one_protocol, Name, Protocols}` | Roadrunner serves `http1` or `http2` on a listener without `tls`, not both |
| `{listener_not_started, Reason}` | The server could not start the listener, for instance because the port is taken |
| `{route_group_not_served, App, Prefix, Listener}` | A route group names a listener that is not configured, or one that does not serve its application |
| `{route_http_versions_not_served, Path, Versions, Listener}` | A route asks for HTTP versions its listener serves none of |
| `{invalid_http_versions, Path, Versions}` | `http_versions` is not a list of `http1`, `http2` and `http3` |
| `{websocket_route_not_served, Path, Listener}` | A WebSocket route is on a listener that serves no version the server takes WebSockets over |

A listener that serves `http3` alone starts, with a notice: browsers only
come to it when an `HTTPS` record in DNS says it is there.

## Routes and listeners

A router says where and how its routes are served with two keys, see
[Routing](routing.md#the-listeners-of-a-route-group):

- `listener` in a route group names the listeners the group is served on.
- `http_versions` in a route group, or in the options of a route, names the
  HTTP versions a route is served over.

In a controller, `nova_req:listener/1` is the name of the listener a request
came in on, and `nova_req:http_version/1` is `http1`, `http2` or `http3`.

## At runtime

`nova_sup:listeners/0` lists the names of the listeners that are started.

`nova_sup:add_application/2` serves an application that was not configured:
on a listener that is running, or on a new one.

```erlang
{ok, _App, _Host, _Port} = nova_sup:add_application(my_other_app, #{listener => admin}).
{ok, _App, _Host, _Port} = nova_sup:add_application(my_other_app, #{port => 8081}).
```

The second form takes the settings of a listener. On a port Nova already
listens on, the application is added to that listener. See
[Multiple applications](multi-app.md#starting-an-application-at-runtime).

## `cowboy_configuration`

Before 1.0 the one listener of a node was configured under
`cowboy_configuration`. Nova still reads the key when `listeners` is not
set, as one listener named `default` that serves `[http1, http2]`, and logs
a notice at boot.

| `cowboy_configuration` | `listeners` |
|---|---|
| `port`, `ip` | `port`, `ip` |
| `use_ssl => true`, `ssl_port`, `ssl_options` | `tls => Options`, `port` |
| `options` | `server_opts` |
| `stream_handlers` | `stream_handlers` in `server_opts`, on Cowboy |
| `middleware_handlers` | `steps` |

Without TLS, the `[http1, http2]` it stands for is what Cowboy served. On
Roadrunner such a listener is refused, so write the listener out.

## Writing a server package

A server package is an application that implements the `nova_server`
behaviour and supervises `nova_listener_owner` with its module as the
argument. `nova_cowboy` is the one to read.
