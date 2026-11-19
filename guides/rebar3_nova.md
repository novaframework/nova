# Rebar3 Nova

The `rebar3_nova` plugin adds project templates and a `nova` namespace of
commands to [rebar3](https://www.rebar3.org/). The
[quick start](quick-start.md) shows how to install it. This page lists the
commands of `rebar3_nova` 0.14.0. `rebar3 help nova <command>` prints the
options of a command.

## Creating a project

| Command | Description |
|---|---|
| `rebar3 new nova <name>` | Create a project from the basic template |
| `rebar3 new nova_fullstack <name>` | Create a project with live views and a database layer |
| `rebar3 new nova_lfe <name>` | Create an LFE project |
| `rebar3 nova new <name>` | Create a project and choose what goes into it with flags: `--kura`, `--pgo`, `--arizona`, `--lfe`, `--ci`, `--docker`, `--otel` |

## Running

| Command | Description |
|---|---|
| `rebar3 nova serve` | Start the project in a shell and recompile and reload modules when a source file changes. Uses the configuration file of `{shell, [{config, File}]}` in `rebar.config` unless `--config` names another. Takes `--name`, `--sname`, `--setcookie`, `--script` and `--apps` as `rebar3 shell` does. Needs `inotifywait` (inotify-tools) on Linux |
| `rebar3 nova release` | Regenerate the OpenAPI specification when the project has schemas, then build a release. `--profile` selects the rebar3 profile, `prod` by default |

## Inspecting

| Command | Description |
|---|---|
| `rebar3 nova routes` | Print the routes of the project, including those of the applications it includes |
| `rebar3 nova middleware` | Print the global plugins and the plugins of each route group |
| `rebar3 nova config` | Print the Nova configuration keys with their values or defaults |
| `rebar3 nova audit` | Check the routes for security issues such as mutating routes without a security callback and routes that accept every method. `--env` sets the environment the routes are compiled for (`dev` by default), `--config` the configuration file to read `nova_apps` from, and `--strict` makes warnings fail the command |
| `rebar3 nova doctor` | Run read-only checks of the toolchain, project, configuration, routes, dependencies, security and build, and report the findings. `--only` and `--skip` take a comma-separated list of sections, `--strict` exits with 1 on warnings, `--verbose` prints every finding |
| `rebar3 nova openapi` | Generate an OpenAPI 3.0.3 specification from the routes. Written to `priv/assets/openapi.json` unless `--output` names another file. `--title`, `--api-version` and `--prefix` set the title, the version and the path prefix to document |

## Generating code

| Command | Description |
|---|---|
| `rebar3 nova gen_controller --name <name>` | Generate a controller with stub actions. `--actions` takes a comma-separated list, `list,show,create,update,delete` by default |
| `rebar3 nova gen_resource --name <name>` | Generate a controller and a JSON schema, and print the routes to add to the router. Takes `--actions` as `gen_controller` does |
| `rebar3 nova gen_test --name <name>` | Generate a Common Test suite for a controller, see [Testing](testing.md) |
| `rebar3 nova gen_auth` | Generate schemas, controllers and context modules for email and password authentication |

## Example

```bash
rebar3 new nova my_app
cd my_app
rebar3 nova gen_controller --name items --actions list,show
rebar3 nova routes
rebar3 nova audit --env prod --strict
rebar3 nova serve
```
