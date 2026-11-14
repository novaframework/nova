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
declares `nova_apps` of its own has those registered too. What they take over
from the entry of their parent is listed
[below](#applications-included-by-an-included-application).

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
| `secure` | `false` \| `fun/1` | Security callback that runs in front of every route group of the application, before the callback a group declares for itself |
| `override_secure` | `false` \| `fun/1` | Replace the security callback of every route group, including one the application declares for itself, and of every route group of the applications it includes |
| `plugin_strategy` | see the [routing guide](routing.md) | How the application's route-local plugins combine with the global ones |

An application that starts a listener, the one named in
`bootstrap_application` or one started with `nova_sup:add_application/2`,
can list itself in its own `nova_apps`. Its routes are then compiled with
the options of that entry:

```erlang
{my_nova_app, [
   {nova_apps, [{my_nova_app, #{plugin_strategy => global_first}},
                {another_nova_app, #{prefix => "/another"}}]}
  ]}.
```

An entry an included application writes for itself is not used. Such an
application gets its options from the entry that includes it.

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

`secure` puts a callback in front of every route group of the mounted
application. A route group that declares a `security` callback keeps it, so
such a group has two, and a request has to be allowed by both. Mounting an
application can add a callback to its routes but cannot take one away, and
the application cannot opt a group out of the callback it is mounted with.

| `secure` of the entry | `security` of the route group | What runs |
|---|---|---|
| a callback | a callback | the callback of the entry, then the one of the group |
| a callback | none, or `false` | the callback of the entry |
| none, or `false` | a callback | the callback of the group |
| none, or `false` | none, or `false` | nothing |

The first callback that does not allow the request answers it, with `401`, a
redirect, or the status, headers and body it returns. The other callback is
not run then. A callback that raises is answered with `500`.

When both allow the request, the controller gets the `auth_data` of the
group's callback if that returns `{true, Data}`, and otherwise the data of
the entry's callback. A group's callback that returns `true` leaves the data
of the entry's callback in place.

The group's callback is given the request with the data of the entry's
callback under `auth_data`, whether the route leads to a controller or to a
websocket.

A callback that is both the `secure` of the entry and the `security` of the
group runs once. Websocket routes follow the same rules.

Replacing a callback the application declares takes `override_secure`, see
the [routing guide](routing.md#overriding-security-for-an-included-application).
With `override_secure`, neither `secure` nor the callback of a group runs.
They are still checked when the routes are compiled, and a value that is not
a callback stops the start with `{invalid_security, Value}`.

### Applications included by an included application

An included application can declare `nova_apps` of its own. Those
applications share the listener too. Each is mounted with the options of its
own entry, not with the options of the entry that included its parent:

| Option in the entry of the parent | Applies to the nested application |
|---|---|
| `prefix` | No. It answers under the `prefix` of its own entry, counted from the root of the listener, and at the root when it has none |
| `secure` | No |
| `plugin_strategy` | No. Its route groups use the plugins they declare or the global ones, as any route group does |
| `override_secure` | Yes |

With `{admin_app, #{prefix => "/admin", secure => fun my_auth:check/1}}`,
where `admin_app` declares `{nova_apps, [dashboard_app]}`, the routes of
`dashboard_app` answer at the root of the listener and without
`my_auth:check/1`. Give `dashboard_app` an entry with its own `prefix` and
`secure` in the `nova_apps` of `admin_app`, or use `override_secure`.

`override_secure` is the option that covers everything an application brings
in. When the entry of the nested application sets an `override_secure` too,
both callbacks run, the outer one first. The request goes on when both allow
it. The first that does not answers it, and the other one is not run. The
controller gets the `auth_data` of the nested callback when that returns
some, and otherwise that of the outer one.

An application is mounted once. When it is listed more than once, the entry
that comes first is used, going through `nova_apps` depth-first.

A route group can include applications as well, with the `apps` key:

```erlang
#{prefix => "/tools",
  security => fun my_auth:check/1,
  apps => [tools_app, {reports_app, #{prefix => "/reports"}}]}
```

Those are compiled with the options the application of the group was mounted
with, and the prefix of the group as their `prefix`. `tools_app` answers
under `/tools`, with the `secure`, `override_secure` and `plugin_strategy` of
that entry. Options given with an application replace the inherited ones, so
`reports_app` answers under `/reports`, not `/tools/reports`. An
`override_secure` is the exception: an inherited one stays in force and one
given with the application is added to it, as above.

The `security` and `plugins` of the group are not inherited. They apply to
the `routes` of the group, and `my_auth:check/1` above does not run for
`tools_app` or `reports_app`.

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
