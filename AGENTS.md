# AGENTS.md

Working agreement for agents and contributors on **nova** - a web framework for
the BEAM (Erlang, Elixir, LFE) built directly on [Cowboy](https://github.com/ninenines/cowboy).
Nova gives you routing, controllers, ErlyDTL views, a pre/post plugin pipeline,
sessions, WebSockets, and `pg`-backed Pub/Sub, all as a proper OTP application.
Co-maintained with Niclas Axelsson. Public, Apache 2.0, ships on Hex.

## Framework pillars

- **Cowboy-clean.** Nova is a thin, idiomatic layer over Cowboy. Don't
  reimplement what Cowboy already does; don't leak Cowboy internals into public
  API where a Nova primitive belongs.
- **Plugin pipeline, order is a security contract.** Requests flow through
  ordered `pre_request` plugins, security, the controller, then `post_request`
  plugins. Reordering plugins (e.g. CSRF vs auth vs body-parse) changes the
  threat model. Never reorder for ergonomics.
- **Multi-app aware.** One Nova node hosts several OTP apps, each mounted under
  a prefix. Routes from all apps compile together. Nothing may assume a single
  app owns the node.
- **Behaviours over hardcoded modules.** Handlers, plugins, sessions, security,
  and WebSocket endpoints are all behaviours + registries. Extend by
  implementing a behaviour and registering it, not by editing core dispatch.

Any architectural, public-API, routing, plugin-contract, session, or pubsub
change goes past the **nova-architecture-guardian** before it lands.

## Scope

- **In:** the router, the handler registry + built-in handlers, the plugin
  manager + behaviour + built-in plugins (request/cors/csrf/correlation),
  sessions (behaviour + ETS backend), `nova_pubsub`, WebSocket support, ErlyDTL
  view rendering, error + file controllers, watchers, `nova_jsonlogger`.
- **Out:** single-consumer convenience helpers (a tenant-route helper, a
  dashboard-specific plugin) - those live in the consuming app. Phoenix feature
  ports that don't fit Cowboy/Erlang idioms. A LiveView layer (that is Arizona).
  A database layer (that is Kura).

## Commands

```bash
rebar3 compile
rebar3 eunit            # unit + PropEr property suites
rebar3 xref
rebar3 dialyzer
rebar3 ex_doc           # fix every new warning before pushing
rebar3 mutate           # rebar3_mutate is wired in; use it on changed modules
```

New apps are scaffolded with `rebar3 new nova my_app` then `rebar3 nova serve`
(the `rebar3_nova` plugin) - never hand-scaffold, never raw `erl`.

## Pre-push checklist

Run all of these green before `git push`:

`rebar3 compile` -> `rebar3 xref` -> `rebar3 dialyzer` -> `rebar3 eunit` ->
`rebar3 ex_doc`. This mirrors what CI (`.github/workflows/erlang.yml`) enforces
across the OTP matrix **27.3 / 28.5 / 29.0** on rebar3 3.25.0. There are no CT
suites here - tests are EUnit + PropEr. (erlfmt/eqwalizer/elvis are not yet
wired into this repo; if you add them, add the CI step too, don't fake it.)

## Conventions

- OTP 27+. Use the `~"..."` sigil for binaries, never `<<"...">>`.
- No `lists:foldl/foldr` - list comprehensions + `maps:from_list`, or explicit
  named recursion.
- Logging: `?LOG_*` macros with `#{...}` map reports, never `logger:info/error`
  format strings. `nova_jsonlogger` is the JSON formatter for OTP `logger`.
- JSON: `json_lib` is configurable and currently defaults to `thoas`. New code
  should not hard-depend on a JSON lib; go through the configured `json_lib`.
  (Nova 1.0 drops `thoas` for OTP's built-in `json` once three OTP releases ship
  it.)
- Docs: OTP `-moduledoc` / `-doc`; ex_doc guides live under `guides/`. Update
  the relevant guide whenever you change a feature.
- `{vsn, git}` in `nova.app.src` - version derives from git tags. Never
  hand-edit it; never publish to Hex (the maintainers do that manually).

## Architecture

Supervision (`nova_sup`, `one_for_one`): `nova_handlers` (handler registry),
`nova_plugin_manager` (plugin lifecycle), `nova_watcher` (external commands),
the session manager (default `nova_session_ets`), and the Cowboy listener.

Cowboy middleware chain:

```
nova_router -> nova_plugin_handler(pre) -> nova_security_handler
            -> nova_handler -> nova_plugin_handler(post)
```

- **Router** (`nova_router`): compiles every mounted app's `routes/1` into a
  `routing_tree` (optionally in `persistent_term`). Per-route `security` fn,
  prefixes, path params, environment-based routing.
- **Handlers** (`nova_handlers` + `nova_basic_handler`): controller return
  tuples (`{json, _}`, `{ok, Vars}`, `{status, _}`, `{redirect, _}`,
  `{sendfile, _}`, `{view, _}`, `ws`) resolve through an ETS registry to a
  handler fn. Register new return types, don't branch core dispatch.
- **Plugins** (`nova_plugin` behaviour, `nova_plugin_manager`): `pre_request` /
  `post_request` callbacks returning `{ok,...} | {break,...} | {stop,...}`.
  Built-ins: `nova_request_plugin` (body/qs parsing), `nova_cors_plugin`,
  `nova_csrf_plugin` (OWASP synchronizer token, constant-time compare),
  `nova_correlation_plugin` (request tracing IDs). Order is configured, and it
  is load-bearing security-wise.
- **Sessions** (`nova_session` behaviour, `nova_session_ets` default): cookie
  session id; ETS backend broadcasts set/delete over `nova_pubsub` on the
  `'__sessions'` channel for cross-node sync.
- **Pub/Sub** (`nova_pubsub`): a purely functional wrapper over OTP `pg` (scope
  `nova_scope`) - no gen_server, dead pids auto-reaped, distributed by default.
  Atom channels; `#nova_pubsub{}` record messages. `broadcast` (all nodes) vs
  `local_broadcast` (this node).

## Gotchas

- **Default branch is `master`, not `main`.** Branch from and PR into `master`.
  Nova deps should pin `{branch, "master"}`.
- **Static assets go in `routes`, not a `statics` key.** Add a plain route tuple
  `{"/assets/[...]", "assets"}` (two list strings). The `feat/priv-dir-routes`
  branch that introduced `statics` is abandoned - never use it.
- **`nova:get_env/2` reads the bootstrap app, not `{nova, [...]}`.** Options
  read via `nova:get_env/2` (e.g. `use_sessions`) must sit under the
  `bootstrap_application`'s env block; putting them under `{nova, [...]}`
  silently returns the default. Options read via `application:get_env(nova, ...)`
  (`cowboy_configuration`, `environment`, `use_stacktrace`, `plugins`,
  `bootstrap_application`) stay under `{nova, [...]}`. When unsure, set both.

## Tests

`test/` is EUnit + PropEr. Suites are `*_tests.erl` (EUnit) and `*_prop_tests.erl`
(PropEr). `nova_test_helper` builds mock Cowboy reqs (`mock_req`, `with_json_body`,
`with_bindings`, `with_header`, `with_qs`, `setup_nova_env`, ...). Always add or
update tests alongside any code change and run `rebar3 eunit` before pushing.

## Git and PRs

Conventional commits (`feat:`, `fix:`, `chore:`, `docs:`, `test:`, `refactor:`).
Always work on a feature branch and open a PR against `master` - never commit to
`master` directly. No `Co-Authored-By` trailer and no "Generated with ..."
footer on any commit or PR. Push to `novaframework/nova`.
