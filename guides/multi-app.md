# Running several Nova applications

Nova can serve more than one Nova application from the same node. An
application can be mounted into another one's listener at startup, or started
and stopped on its own listener at runtime.

## Including another application at startup

Add the application as a dependency in `rebar.config`, then list it under the
`nova_apps` key in your own application's configuration. `nova_apps` takes
application names, or `{Name, Options}` two-tuples when you want to configure
how it is mounted.

Included applications are resolved depth-first, so an application that
declares `nova_apps` of its own has those registered too.

### Options

| Key | Value | Description |
|---|---|---|
| `prefix` | `string()` | Mount the application's routes under this path |
| `secure` | `false` \| `fun/1` | Security callback for the application's routes |
| `override_secure` | `false` \| `fun/1` | Replace the security callback the application declares for itself |
| `plugin_strategy` | see the [routing guide](routing.md) | How the application's route-local plugins combine with the global ones |

### Example

*rebar.config*:

```erlang
{deps, [
        {another_nova_app, "1.0.0"}
       ]}.
```

*sys.config*:

```erlang
{my_nova_app, [
   {nova_apps, [{another_nova_app, #{prefix => "/another"}}]}
  ]}.
```

`another_nova_app` now shares the listener, and the routing table, of
`my_nova_app`. Its routes answer under `/another`.

## Starting an application at runtime

`nova_sup:add_application/2` starts a Nova application while the node is
running. The second argument takes the same shape as the `cowboy_configuration`
environment key.

```erlang
{ok, App, Host, Port} = nova_sup:add_application(my_other_app, #{port => 8081}).
```

What happens depends on whether the host and port are already bound:

- **A free port.** A new Cowboy listener is started with a routing table of its
  own, holding that application, anything in its `nova_apps`, and Nova's own
  error pages. The listener serves only those routes.
- **A port Nova already listens on.** The application's routes are added to
  that listener's existing routing table, and the two applications are served
  side by side.

Starting an application that is already running returns
`{error, {already_started, App}}`.

Because each listener has its own routing table, two applications on two ports
do not serve each other's routes. That is the point of binding a second port:
an admin interface on `8081` is not reachable on the public `8080` just
because both are running in the same node.

## Stopping an application

```erlang
ok = nova_sup:remove_application(my_other_app).
```

The application's routes are removed from the listener serving it. If that
leaves the listener with no applications, the listener is stopped and its
routing table discarded, releasing the port. Other applications on the same
listener are unaffected.

Removing an application that was never started returns `{error, not_found}`.

## Inspecting what is running

```erlang
nova_sup:get_started_applications().
%% [#{app => my_app, host => {0,0,0,0}, port => 8080, listener => nova_listener},
%%  #{app => my_other_app, host => {0,0,0,0}, port => 8081,
%%    listener => {nova_listener, my_other_app, 8081}}]
```

`nova_router:compiled_apps/0` lists the applications compiled into the default
listener's routing table, and `nova_router:compiled_apps/1` does the same for
any other.

## Graceful shutdown

Every listener Nova has started is suspended, drained and stopped on shutdown,
including ones added at runtime. See the
[graceful shutdown guide](graceful-shutdown.md).
