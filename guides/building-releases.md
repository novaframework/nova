# Building releases

Nova applications are released with [relx](https://github.com/erlware/relx),
which is part of rebar3. A project created with `rebar3 new nova` has the
release configured in `rebar.config`: a development release in the default
profile and a `prod` profile for production.

## Building

```bash
rebar3 as prod tar
```

This builds the release with the `prod` profile and archives it in
`_build/prod/rel/<name>/<name>-<version>.tar.gz`. `rebar3 as prod release`
builds it without the archive. `rebar3 nova release` regenerates the OpenAPI
specification when the project has schemas and then builds the release, see
[Rebar3 Nova](rebar3_nova.md).

The rebar3 manual describes the release options:
[Releases](https://www.rebar3.org/docs/deployment/releases/).

## What the prod profile uses

| File | Purpose |
|---|---|
| `config/prod_sys.config.src` | The application environment of the release |
| `config/vm.args.src` | The VM arguments. Takes the distribution cookie from `RELEASE_COOKIE` |
| `rel/hooks/require_cookie` | A start hook that stops the release from starting when `RELEASE_COOKIE` is not set |

Both configuration files are templates: `${NAME}` in them is replaced with the
value of the environment variable `NAME` when the release starts.

## Before the first deployment

Set these in `config/prod_sys.config.src`, under `nova`:

| Key | Why |
|---|---|
| `environment` | Set it, usually to `prod`. When it is not set it is read as `prod`, and Nova logs a notice at boot |
| `csrf_secret` | For an application that uses `nova_csrf_plugin`: `~"${CSRF_SECRET}"`, the same value on every node. See [Plugins](plugins.md#csrf-token-modes) |
| `allowed_origins` | Leave it out when the `Host` header reaches Nova as the browser sent it. Otherwise list the public origins of the site, see [Security](security.md#behind-a-proxy-that-terminates-tls) |
| `use_stacktrace` | Leave it `false` |
| `cowboy_configuration` | The port, and TLS when the node terminates it, see below |
| `shutdown_delay`, `shutdown_drain_timeout` | See [Graceful shutdown](graceful-shutdown.md) |

Set `RELEASE_COOKIE` to the same long random value on every node of a
cluster:

```bash
export RELEASE_COOKIE="$(openssl rand -hex 32)"
bin/my_app foreground
```

The [security guide](security.md#before-going-to-production) has the full
list of things to check.

## TLS

To serve HTTPS from the node itself:

```erlang
{nova, [
    {cowboy_configuration, #{
        use_ssl => true,
        ssl_port => 8443,
        ssl_options => #{
            certfile => "/etc/ssl/my_app/fullchain.pem",
            keyfile => "/etc/ssl/my_app/privkey.pem"
        }
    }}
]}
```

With `use_ssl` the listener serves TLS on `ssl_port` and nothing on `port`.

## Behind a reverse proxy

When a proxy or load balancer terminates TLS and forwards plain HTTP:

* Nova does not read `x-forwarded-for`, `x-forwarded-proto` or `forwarded`.
  `cowboy_req:peer/1` is the address of the proxy and `cowboy_req:scheme/1`
  is `http`. Read the headers your proxy sets in your own code, and only
  trust them when the node is reachable through the proxy alone.
* The session cookie is still marked `Secure` unless `environment` is set to
  `dev`, so sessions work as long as the client talks HTTPS to the proxy.
* The origin checks of websocket routes and of `nova_csrf_plugin` compare the
  `Origin` of a request with its `Host` header. See
  [Security](security.md#behind-a-proxy-that-terminates-tls) for what the
  proxy has to pass on.
* Bind the listener to an address only the proxy reaches, with the `ip` key
  of `cowboy_configuration`.
