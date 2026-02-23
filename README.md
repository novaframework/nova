<p align="center">
  <img src="https://raw.githubusercontent.com/novaframework/nova/master/priv/static/nova.png" alt="Nova" width="200">
</p>

<h1 align="center">Nova</h1>

<p align="center">
  <strong>A web framework for Erlang/OTP</strong><br>
  Build fault-tolerant, distributed web applications in pure Erlang.
</p>

<p align="center">
  <a href="https://github.com/novaframework/nova/actions/workflows/erlang.yml"><img src="https://github.com/novaframework/nova/actions/workflows/erlang.yml/badge.svg" alt="Build Status"></a>
  <a href="https://hex.pm/packages/nova"><img src="https://img.shields.io/hexpm/v/nova.svg" alt="Hex.pm"></a>
  <a href="https://hexdocs.pm/nova/"><img src="https://img.shields.io/badge/docs-hexdocs-blue.svg" alt="Docs"></a>
  <a href="https://erlef.org/slack-invite/erlanger"><img src="https://img.shields.io/badge/chat-Erlang%20Slack-4A154B.svg" alt="Slack"></a>
</p>

---

## Why Nova?

Nova is the web framework for developers who **think in Erlang**. If you want the productivity of a modern web framework without leaving the language you love, Nova gives you:

- **Routing, controllers, and views** — familiar MVC patterns that map naturally to Erlang modules
- **Plugin pipeline** — composable middleware for auth, CORS, logging, and custom concerns
- **WebSockets and Pub/Sub** — real-time features backed by OTP's `pg` module, distributed out of the box
- **Session management** — ETS-backed by default, pluggable for custom backends
- **Template rendering** — ErlyDTL (Django Template Language) with hot reload
- **OTP-native** — your web app is a proper OTP application with supervision trees, hot code upgrades, and releases

Nova runs on [Cowboy](https://github.com/ninenines/cowboy) and supports **Erlang**, **Elixir**, and **LFE**.

## Quick start

Install the rebar3 plugin:

```bash
# One-line install
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"

# Or add to ~/.config/rebar3/rebar.config
{project_plugins, [rebar3_nova]}.
```

Create and run your app:

```bash
rebar3 new nova my_app
cd my_app
rebar3 nova serve
```

Open [localhost:8080](http://localhost:8080) — you're running Nova.

## What a controller looks like

```erlang
-module(my_app_main_controller).
-export([index/1, create/1]).

index(#{method := <<"GET">>} = _Req) ->
    {json, #{message => <<"Hello from Nova!">>}}.

create(#{method := <<"POST">>, json := Body} = _Req) ->
    %% Validate, persist, respond
    {json, #{status => <<"created">>, data => Body}}.
```

Route it:

```erlang
routes(_Environment) ->
    [#{prefix => "/api",
       security => false,
       routes => [
           {"/", {my_app_main_controller, index}},
           {"/items", {my_app_main_controller, create}}
       ]}].
```

## Features

| Feature | Details |
|---------|---------|
| **Routing** | Path parameters, prefixes, per-route security, environment-based routing |
| **Controllers** | Return `{json, Map}`, `{ok, Variables}`, `{status, Code}`, `{redirect, Path}`, `{sendfile, ...}` |
| **Plugins** | Pre/post request hooks — built-in CORS, CSRF, correlation ID, or write your own |
| **WebSockets** | Full WebSocket support via `nova_websocket` behaviour |
| **Pub/Sub** | Distributed messaging via `nova_pubsub` — channels, topics, broadcast |
| **Sessions** | ETS-backed sessions with pluggable backends |
| **Templates** | ErlyDTL (Django-style) with hot reload via `rebar3 nova serve` |
| **Security** | Per-route security functions for authentication and authorization |
| **Static files** | Built-in file controller with range request support |
| **Observability** | [OpenTelemetry integration](https://github.com/novaframework/opentelemetry_nova) for tracing and metrics |
| **Database** | [Kura](https://github.com/novaframework/kura) — an Ecto-inspired database layer for Erlang |

## Request pipeline

```
Request → Cowboy → Nova Router → Plugins (pre) → Security → Controller → Plugins (post) → Response
```

## Ecosystem

| Package | Description |
|---------|-------------|
| [nova](https://hex.pm/packages/nova) | Core framework |
| [rebar3_nova](https://github.com/novaframework/rebar3_nova) | Project scaffolding and generators |
| [nova_test](https://github.com/novaframework/nova_test) | Testing utilities and request builder |
| [kura](https://hex.pm/packages/kura) | Database layer — schemas, migrations, changesets, queries |
| [rebar3_kura](https://github.com/novaframework/rebar3_kura) | Migration generator for Kura |
| [opentelemetry_nova](https://github.com/novaframework/opentelemetry_nova) | Automatic OpenTelemetry instrumentation |

## Documentation

- **[The Nova Book](https://novaframework.github.io/nova-book)** — step-by-step tutorial building a complete blog platform
- **[API Reference](https://hexdocs.pm/nova/)** — module documentation on HexDocs
- **[Quick Start Guide](https://hexdocs.pm/nova/quick-start.html)** — get up and running in 5 minutes

## Requirements

- Erlang/OTP 23+
- Rebar3

## Community

- [Erlang Slack](https://erlef.org/slack-invite/erlanger) — `#erlanger` channel
- [Nova Forum](https://erlangforums.com/c/erlang-frameworks/nova-forum/65) — questions and discussion
- [Issue Tracker](https://github.com/novaframework/nova/issues) — bugs and feature requests

## Contributing

Contributions are welcome! Check [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) and look for issues labeled `good first issue`.

## License

[Apache 2.0](LICENSE)
