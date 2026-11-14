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

Your own application's routes are registered before those of the applications
it includes. Where both declare the same path and method, or a page for the
same status code, yours is served and the included application's is logged as
a conflicting route and ignored. The warning names the application whose route
was ignored, the path or status code, and the method. With
`use_strict_routing` the clash stops the application from starting instead. Status code routes are not prefixed, so one
declared by an included application applies to the whole listener when yours
declares none for that code. Earlier releases registered included applications
first, which let them replace a route of yours together with its security.

### Options

| Key | Value | Description |
|---|---|---|
| `prefix` | `string()` | Mount the application's routes under this path, in front of the prefix each route group declares |
| `secure` | `false` \| `fun/1` | Security callback for the application's route groups that declare none |
| `override_secure` | `false` \| `fun/1` | Replace the security callback of every route group, including one the application declares for itself |
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

### Prefix of a mounted application

The `prefix` in the `nova_apps` entry and the `prefix` of a route group add
up. A group declared with `prefix => "/users"` in an application mounted with
`prefix => "/another"` answers under `/another/users`.

The application can look up where it was mounted, for instance to build
links:

```erlang
{another_nova_app, MountPrefix} = lists:keyfind(another_nova_app, 1, nova:get_apps()).
```

An application mounted without a prefix is listed with `"/"`.
`nova:get_apps/0` reads the default listener's table; use
`nova_router:compiled_apps/1` for any other.

### Security of a mounted application

A route group that declares a `security` callback keeps it when the
application is mounted. `secure` only applies to the groups that declare none
(or `false`), so mounting an application can put a callback in front of its
routes but cannot take one away. `secure => false` leaves every group as the
application declared it.

Replacing a callback the application declares takes `override_secure`, see
the [routing guide](routing.md#overriding-security-for-an-included-application).

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
any other. `nova_router:lookup_url/1,2,3` and `nova:get_apps/0` also read the
default table. Inside a request, `nova_router:dispatch_key(Req)` gives the key
of the listener it came in on, for `nova_router:compiled_apps/1` and
`nova_router:lookup_url/4`.

The page for a status code is looked up in the routing table of the listener
the request came in on, also when a controller crashes or throws
`{Status, Reason}`. Earlier releases rendered the default listener's page for
those on every listener.

## Graceful shutdown

Every listener Nova has started is suspended, drained and stopped on shutdown,
including ones added at runtime. See the
[graceful shutdown guide](graceful-shutdown.md).
