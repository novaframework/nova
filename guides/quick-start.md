# Quick start

Nova is a web framework for the BEAM with support for **Erlang**, **Elixir**
and **LFE**. This guide creates a project, starts it and adds a JSON endpoint.

Nova needs Erlang/OTP 27 or later.

## Erlang

> #### Erlang and rebar3 {: .info}
>
> For help installing Erlang and rebar3 see
> [Adopting Erlang](https://adoptingerlang.org/docs/development/setup/).

### Install the rebar3 plugin

The installer adds `rebar3_nova` to the `plugins` of your global rebar3
configuration, `~/.config/rebar3/rebar.config`.

With curl:

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

With wget:

```bash
sh -c "$(wget -O- https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

To do it by hand, open `~/.config/rebar3/rebar.config`, create it if it does
not exist, and add the plugin to the `plugins` list:

```erlang
{plugins, [rebar3_nova]}.
```

### Create the project

```bash
rebar3 new nova my_first_nova
```

This writes a directory `my_first_nova` with:

| File | Purpose |
|---|---|
| `rebar.config` | Dependencies, the `prod` and `test` profiles, the ErlyDTL compile hook |
| `config/dev_sys.config.src` | Configuration used by `rebar3 shell` and `rebar3 nova serve` |
| `config/prod_sys.config.src` | Configuration used by a `prod` release |
| `config/vm.args.src`, `config/dev_vm.args.src` | VM arguments for a `prod` release and for development |
| `src/my_first_nova.app.src` | The application resource file |
| `src/my_first_nova_app.erl`, `src/my_first_nova_sup.erl` | The application and its supervisor |
| `src/my_first_nova_router.erl` | The routes |
| `src/controllers/my_first_nova_main_controller.erl` | A controller |
| `src/views/my_first_nova_main.dtl` | The view that controller renders |
| `priv/assets/favicon.ico` | A static file |
| `rel/hooks/require_cookie` | Stops a `prod` release from starting without `RELEASE_COOKIE` |

### Start it

```bash
cd my_first_nova
rebar3 nova serve
```

`rebar3 nova serve` compiles the project, starts it in a shell, and recompiles
and reloads modules when a source file changes. Open
[http://localhost:8080](http://localhost:8080) to see the page the main
controller renders.

> #### File watching {: .info}
>
> On Linux `rebar3 nova serve` uses `inotifywait` to watch for changes. It is
> part of the `inotify-tools` package of most distributions. `rebar3 shell`
> starts the application without watching for changes and needs nothing extra.

### Your first JSON endpoint

Add a controller, `src/controllers/my_first_nova_items_controller.erl`:

```erlang
-module(my_first_nova_items_controller).
-export([show/1, create/1]).

show(#{bindings := #{~"id" := Id}}) ->
    {json, #{id => Id}}.

create(#{json := Item}) ->
    {json, #{created => Item}}.
```

A controller function takes the request map and returns a tuple that says what
to answer. `bindings` holds the path parameters. `json` holds the decoded
request body; it is there because the generated `config/dev_sys.config.src`
configures `nova_request_plugin` with `decode_json_body => true` for every
route.

Add a route group for it to the list `routes/1` returns in
`src/my_first_nova_router.erl`:

```erlang
#{prefix => "/api",
  security => false,
  routes => [
      {"/items/:id", fun my_first_nova_items_controller:show/1, #{methods => [get]}},
      {"/items", fun my_first_nova_items_controller:create/1, #{methods => [post]}}
  ]}
```

Try it:

```bash
$ curl http://localhost:8080/api/items/42
{"id":"42"}

$ curl -i -H 'content-type: application/json' -d '{"name":"book"}' http://localhost:8080/api/items
HTTP/1.1 201 Created
...
{"created":{"name":"book"}}
```

`{json, Term}` answers `201` to a `POST` and `200` to every other method. A
body that is not valid JSON is answered with `400` before the controller runs.

Where to go next:

* [Routing](routing.md) and [Controllers](controllers.md) for everything a
  route and a controller can do.
* [Security](security.md) before the application is reachable by others.
* [Testing](testing.md) to put the endpoint under test.

## Elixir

Install the mix task:

```bash
mix archive.install hex nova_new
```

Create a project and start it:

```bash
mix nova.new my_app
cd my_app
mix deps.get
iex -S mix
```

Open [http://localhost:8080](http://localhost:8080) to see the start page.
