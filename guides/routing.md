# Routing

Each Nova application has a router module. Its `routes/1` callback returns the routes of the application as a list of *route groups*. The module is named after the application with `_router` appended, such as `my_app_router`; `router_module` in the application environment names another one.

## Basic components

A simple route file could look something like this:

```erlang
-module(my_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
  [#{prefix => "/admin",
    security => false,
    routes => [
      {"/", fun my_controller:main/1, #{methods => [get]}}
    ]
  }].
```

A `GET` request to `/admin` calls `my_controller:main/1`. The argument of `routes/1` is the value of `nova:get_environment/0`, which lets a router return different routes for different environments. Set it with `{environment, Env}` under the `nova` application in *sys.config*. When it is not set it is read as `dev`.

### Route groups

A route group is a map. Its keys are all optional:

| Key | Description |
|---|---|
| `prefix` | A path put in front of every route of the group |
| `security` | The security callback of the group, `false` or a `fun/1`. A group without the key has none, see [Secure routing](#secure-routing) |
| `plugins` | The plugins of the group, see [Using plugins local to a set of endpoints](#using-plugins-local-to-a-set-of-endpoints) |
| `plugins_after_security` | Plugin modules that wait for the security callback, see [Plugins](plugins.md#running-a-plugin-after-the-security-callback) |
| `routes` | The routes |
| `host` | Serve the group only for requests to this host, a string or a binary. A host that has groups of its own is served by those alone: groups without `host` are not consulted for it |
| `extra_state` | A term handed to the controllers of the routes of the group that set no `extra_state` themselves |
| `apps` | Other Nova applications to mount under the prefix of the group, see [Multiple applications](multi-app.md) |

You can also defer the routing information to the controllers, like this:
```erlang
-module(my_app_router).
-behaviour(nova_router).

-export([controllers/1]).

controllers(_Environment) ->
  [my_controller,
   {my_file_controller, #{root => "/pub"}},
   {my_file_controller, #{root => "/internal"}}
  ].
```

The controller modules then need to provide the `routes/2` callback, like
this (note the extra `Options` argument):

```erlang
-module(my_controller).
-behaviour(nova_controller).

-export([routes/2]).

routes(_Environment, _Options) ->
  [#{prefix => "/admin",
    security => false,
    routes => [
      {"/", fun ?MODULE:main/1, #{methods => [get]}}
    ]
  }].
```

If a controller module is listed as `{Module, Options}`, then `Options`
will be passed as the second argument to `routes/2`, so you can have
multiple uses of the same controller module with different configurations.
If a controller is listed by name only, the options will be an empty map
`#{}`.

These methods are not exclusive - it is possible to specify routes both in
the main router module and in the controllers.

### HTTP routes

```
{Path :: list(), ControllerCallback :: function(), Options :: map()}
```

`Options` takes `methods`, the list of methods the route answers, and
`extra_state`. A route without `methods` answers every method.
`{Path, ControllerCallback}` is the same as an empty options map.

A segment that starts with `:` is a path parameter. It matches one segment
and reaches the controller under `bindings`, with the name as a binary key:

```erlang
{"/items/:id", fun my_app_items_controller:show/1, #{methods => [get]}}
```

```erlang
show(#{bindings := #{~"id" := Id}}) ->
    {json, #{id => Id}}.
```

`extra_state` hands a term of your own to the controller, under
`extra_state` in the request. A route without the option gets the
`extra_state` of its route group, and `undefined` when the group has none:

```erlang
{"/reports", fun my_app_reports_controller:index/1, #{methods => [get], extra_state => #{role => admin}}}
```

```erlang
index(#{extra_state := #{role := Role}}) ->
    {json, #{role => Role}}.
```

A route ending in `[...]` matches any number of trailing segments. They reach the controller as a list of binaries under `pathinfo` in the `extra_state` of the `Req`. If the route declares an `extra_state` that is not a map it is passed on as it is, without `pathinfo`. A websocket controller finds the same under `req` in its controller data.

### Websocket routes

```
{Path :: list(), Controller :: module(), Options :: map()}
```

A websocket route takes `extra_state` like an HTTP route does, and falls back to the `extra_state` of its route group. The controller finds it in the upgrade request, under `req` in its controller data.

The `init/1` callback of a websocket controller returns `{ok, State}` to accept the connection. Any other return refuses the upgrade with a `500`. It can return `{ok, State, Options}` to also set the options of the connection, where `Options` is a `nova_websocket:opts()` map such as `#{max_frame_size => 65536, idle_timeout => 30000}`. It takes the options of [cowboy_websocket](https://ninenines.eu/docs/en/cowboy/2.21/manual/cowboy_websocket/).

A browser sends the cookies of your site with a websocket upgrade started by any page. When the connection relies on a session cookie, add `nova_ws_origin_plugin` as a `pre_request` plugin to refuse upgrades from other origins, in the global `plugins` or on the route group together with the other plugins the group needs. It is not on by default.

The [Websockets](websockets.md) guide describes the controller.

> #### Important {: .tip}
>
> One needs to define `protocol => ws` in the options-map in order to enable websocket communications.


### Static files

A route whose second element is a string serves static files. The first element is the URL path and the second the path of a file or a directory, relative to the `priv` directory of the application. A third element takes options. End the URL path with `[...]` to serve a directory:

```
{Path :: list(), LocalPath :: list()}
{Path :: list(), LocalPath :: list(), Options :: map()}
```

The options are:

- `mimetype` - which mimetype the file should be served as. If not defined nova will try to guess the mimetype based on the file extension. XML, XHTML, compressed SVG (`.svgz`) and other `+xml` types are served as `application/octet-stream` unless set here, since a browser would run scripts in them on your origin. Static files are sent with `x-content-type-options: nosniff`.
- `index_files` - a list of filenames that are served for a request to a directory. The default is `["index.html"]`.
- `list_dir` - set to `true` to answer a request to a directory that has no index file with a listing of it. Without it such a request gets a `403`.

Example:
```erlang
{"/assets/[...]", "static"},
{"/downloads/[...]", "downloads", #{list_dir => true}},
{"/robots.txt", "static/robots.txt"}
```

A directory route only serves what is inside that directory. A request whose trailing segments are empty, `.` or `..`, or contain `/`, `\` or a NUL byte, is answered with `404`, whatever the router in front of it let through. Symbolic links are resolved first: a link that points inside the directory is served, one that points out of it is a `404`. Set `follow_external_symlinks => true` in the options of the route to serve links that leave the directory. The directory itself is resolved once, when the routes are compiled, and files are checked against and served from where it was then. A relative path means the directory it named in the working directory the node had at that time, also after the working directory has changed. If a symbolic link on the way to the directory is changed while Nova runs, files are still served from where it pointed before, so restart Nova after such a change. With `follow_external_symlinks` no link is resolved, and such a change takes effect at once. A route to a single file given as a relative path also serves the file that path named when the routes were compiled. A directory that has been removed since, or cannot be resolved, is answered with `404` and logged once as an error.

A `Range` request for one byte range is answered with `206` and that part of the file, a suffix range such as `bytes=-500` included. A range that starts past the end of the file, or a suffix of no bytes, gets `416`. A range that is not valid, such as `bytes= 0-1` or `bytes=5-2`, a range unit other than `bytes`, and a request for several ranges are ignored, so the whole file is sent with `200`. A range sent with `If-Range` is ignored too: static files carry no `ETag` or `Last-Modified` for it to match. The local path of a static route may be relative to the working directory of the node.

## How to create routes

A route consists of three different components:

* `Path` - This is the actual path to the endpoint. Eg `"/admin"`
* `Method` - What method you want to support for this endpoint: `get`, `post`, `put`, `delete`, `options`, `head`, `connect`, `trace` or `patch`, as an atom in either case. Any other method, such as `PURGE` or `PROPFIND`, is written as a binary or a string. Those are matched as they are written, since a method is case-sensitive: write `<<"PURGE">>`, not `<<"purge">>`. If you want to support all methods you can use the `'_'`-atom. Any other atom, a misspelling included, or a binary or string that is not a method token ([RFC 9110](https://www.rfc-editor.org/rfc/rfc9110#section-9.1)), stops the application from starting with `{invalid_method, Method}`. Earlier releases silently treated it as `'_'`, so the route answered every method.
* `ControllerCallback` - What erlang callback should be called on when the path gets called. This is defined as a function reference, eg `fun my_controller:main/1`.

## Using prefix

You can group paths where you prefix them with a path. This is especially useful when having several different nova applications running. A very common example would be if you had an administration interface. Then the prefix would be `"/admin"`. Another example is versioned APIs.

Prefix is defined at top level of a route-entry which can be seen in the [Basic components](#basic-components) section of this chapter.

## Secure routing

A route is protected by the security callback of its route group. Where the callback is declared matters:

| Where | Key | Effect |
|---|---|---|
| Route group map | `security` | The callback runs for every route of the group |
| `nova_apps` entry of an application | `secure` | The callback runs in front of every route group of that application |
| `nova_apps` entry of an application | `override_secure` | The callback replaces the ones the application declares |

Nothing else is read:

* A route group without a `security` key has no callback. Its routes are public, unless the `nova_apps` entry of the application has `secure` or `override_secure`.
* A `security` or `secure` key in the options map of a single route is not read. Put routes that need another callback in a route group of their own.
* A `secure` key in a route group is not read. The key of a route group is `security`.

Each such key is logged as a warning when the routes are compiled, with the application, the path and the key. It adds no callback to the routes.

`rebar3 nova audit` lists the routes that change state and have no callback, see [Rebar3 Nova](rebar3_nova.md). The [Security](security.md) guide has complete examples.

### Invoking security functions for a set of endpoints

A security function is called before the controller, to decide if the request is allowed. It returns `true` to let the request through and `false` to answer it with `401`; the other return values are listed below.

The following code will invoke `security_controller:do_security/1` before calling the actual controller to do the security check.

```erlang
-module(my_example_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
    [#{prefix => "/admin",
        security => fun security_controller:do_security/1,
        routes => [
            {"/", fun my_controller:main/1, #{methods => [get]}}
        ]
    }].
```

The callback runs after the `pre_request` plugins of the route and before the controller. The [request pipeline](plugins.md#request-pipeline) section of the plugins guide has the whole order.

`security` takes `false`, a `fun/1` or the deprecated `{Module, Function}`. Any other value, `true` included, is logged as an error and stops the application from starting with `{invalid_security, Value}`. The same goes for `secure` and `override_secure` in `nova_apps`.

The security function runs after the group's `pre_request` plugins, so a plugin that reads the body has read it before the request is checked. `plugins_after_security => [Module]` in the route group makes the listed plugins wait for the security function; see [Running a plugin after the security callback](plugins.md#running-a-plugin-after-the-security-callback).

### The security-function callback

A security function takes the request and returns one of the values below. With `{true, Data}` it hands data to the controller, such as the user and its roles.

#### Return values for the secure function

| **Return value** | **Description** |
| ---------------- | --------------- |
| `true` | The request will continue to the controller. |
| `{true, Data :: term()}` | The request will continue to the controller and the data will be available in the `Req` object under the `auth_data` key. |
| `{redirect, Url :: binary() \| string()}` | The request will be redirected to the specified URL with HTTP status 302. |
| `false` | The request will be stopped and a `401` status code will be returned. |
| `{false, Headers :: map()}` | Same as above but also adds additional headers to the response. |
| `{false, StatusCode :: integer(), Headers :: map()}` | Same as above but uses a custom status code. |
| `{false, StatusCode :: integer(), Headers :: map(), Body :: iodata()}` | Same as above but also adds a custom body to the response. |

Any other return value is answered with `401`, and a function that crashes with `500`.

*Note* If `false` is returned in any form the request will be stopped from executing the controller. The reply has an empty body unless you return one with `{false, StatusCode, Headers, Body}`: status-code routes (below) are not rendered for a refused request, and the `post_request` plugins do not run for it.


```erlang
-module(security_controller).
-export([do_security/1]).

do_security(Req) ->
    case get_user(Req) of
        {ok, User} ->
            {true, #{user => User}};
        _ ->
            false
    end.

get_user(Req) ->
    case nova_session:get(Req, ~"user_id") of
        {ok, UserId} -> {ok, #{id => UserId}};
        {error, _} -> not_found
    end.
```

Once the security function has been called, the data will be available in the `Req` object under the `auth_data` key. A websocket controller gets it under `auth_data` in its controller data, and in the upgrade request under `req` there.

A route group of an [included application](multi-app.md#security-of-a-mounted-application) can have two callbacks, the `secure` of the application that includes it and its own `security`. Both have to allow the request, and the controller gets the data of the group's callback when that returns some, otherwise the data of the other one.

```erlang
-module(my_controller).
-export([main/1]).

main(#{auth_data := #{user := #{id := UserId}}}) ->
    {json, #{user_id => UserId}}.
```

## Using plugins local to a set of endpoints

It's possible to configure a small set of endpoints with a specific plugin. This is done by adding a `plugins` key to the route entry. The value of this key should be a list of plugins to use for this route entry.

```erlang
#{prefix => "/admin",
  plugins => [
    {pre_request, nova_json_schemas, #{render_errors => true}}
  ],
  routes => [
    {"/", fun my_controller:main/1, #{methods => [get]}}
  ]
}
```

In the example above we have enabled the *pre-request*-plugin `nova_json_schemas` for all routes under the `/admin` prefix. This will cause all requests to be validated against the JSON schema defined in the `nova_json_schemas` plugin.
You can also include *post-request*-plugins in the same way.

By default a route entry that declares `plugins` uses exactly those, and one
that does not uses the plugins configured globally. Set `plugin_strategy` in
the application's `nova_apps` options to combine them instead (available since
0.17.0):

```erlang
{nova_apps, [{my_app, #{plugin_strategy => global_first}}]}
```

`plugin_strategy` is an option of a `nova_apps` entry and applies to every
route entry of the application the entry names. The application that starts
the listener sets it for its own routes with an entry for itself in its own
`nova_apps`, as above, and for an application it includes in the entry of
that application. A
`plugin_strategy` key on a single route entry is ignored, and a warning is
logged once per application.

| Value | Plugins used |
|---|---|
| `local_or_global` | The entry's own plugins if it declares any, otherwise the global ones. This is the default. |
| `local_first` | The entry's own plugins, then the global ones. |
| `global_first` | The global plugins, then the entry's own. |
| `local_only` | Only the entry's own plugins. An entry with none gets none. |
| `global_only` | Only the global plugins. |
| `{override, List}` | Exactly `List`. |

When both sets are combined, a plugin appearing in both is kept once, at its
first position, so ordering within a phase is preserved.

### Overriding security for an included application

When you include another Nova application, you may want to put your own
security callback in front of its routes rather than the one it declares for
itself. Set `override_secure` in the options for that application:

```erlang
{nova_apps, [{another_nova_app, #{prefix => "/admin",
                                  override_secure => fun my_security:check/1}}]}
```

It takes the same values as `secure`: `false` for no override, a `fun/1`, or
the deprecated `{Module, Function}`.

It also covers the applications that the included application includes
itself. One of those can have an `override_secure` of its own, and then both
callbacks have to allow a request. The
[multi-app guide](multi-app.md#applications-included-by-an-included-application)
has the details.

`override_secure` is the only option that replaces a callback a route group
declares. `secure` in the same place adds one: its callback runs in front of
every route group of the included application. A group that declares
`security` keeps its callback, which runs second, and a request has to be
allowed by both. The first of the two that does not allow it answers the
request, with the redirect, status, headers and body it returns, and the
other one is not run. The controller gets the `auth_data` of the group's
callback when that returns `{true, Data}`, and otherwise the data of the
`secure` callback. To open up a group that declares a callback, override it
with a callback that returns `true`.


## Route precedence

More than one route can match a request. Nova resolves that the same way every
time, at each path segment in turn:

1. A literal segment.
2. A binding, `:name`. When several bindings sit at the same depth they are
   tried in name order.
3. A `[...]` catch-all.

Matching backtracks, so a route is only skipped if nothing below it can match.
`/users/new` and `/users/:id` can therefore both be declared: `/users/new`
serves the literal path and `/users/:id` serves everything else.

A request path is matched after empty segments are dropped and `.` and `..`
are resolved, so `/public/../admin//users` is routed as `/admin/users`. The
`path` in the request map is still what the client sent. A plugin or security
callback that decides by path must use `nova_router:canonical_path(Req)`, and
compare whole segments: `/api` is not a prefix of `/apikeys`. The
`excluded_paths` of `nova_csrf_plugin` are matched this way. Earlier releases
matched them as a byte prefix of the raw path.

Once a path matches, the method is resolved. An exact method wins over a route
declared with `'_'`. If the path matches but the method does not, Nova answers
`405` with an `allow` header listing the methods that path does accept. No
plugin runs for such a request, with one exception: a CORS preflight runs the
plugins of the route for the method it asks about before the `405`, so that
`nova_cors_plugin` can answer it.

If two routes declare the same path *and* the same method, the first one
registered wins and the second is logged and ignored. Set
`use_strict_routing` to `true` in the `nova` application environment to make
Nova refuse to start on a conflict instead, which also reports overlapping
literal and binding routes.

## Status-code routes

A route whose path is an integer handles that status code:

```erlang
#{routes => [{404, fun my_error_controller:not_found/1, #{}},
             {500, fun my_error_controller:server_error/1, #{}}]}
```

Nova registers its own 404 and 500 handlers; an application's routes for the same codes replace them. A status-code route is rendered when no route matches (404), when the method is not allowed (405), when a controller, a plugin or the security function crashes or a plugin returns `{error, Reason}` (500), when a controller throws `{Status, Reason}`, and when a controller returns something no handler understands. It is *not* rendered for a `{status, Code}` returned by a controller, nor for a request the security function refuses or a plugin stops.

A status-code route that crashes, or throws `{Status, Reason}` itself, is not rendered a second time. The request gets the 500 page; when it was the application's 500 page that failed, Nova's own; and a `500` without a body should that fail as well.

## The matched route

When a request matches a route, Nova stores the route's template in the
request map under `nova_route`. Controllers, plugins, Cowboy handlers and
websocket controllers (through `req` in their controller data) can all read
it:

```erlang
show(#{nova_route := Route}) ->
    %% Route is <<"/api/users/:id">> for a request to /api/users/42
    {json, #{route => Route}}.
```

The template is:

- A binary, in Nova's own route syntax: `:name` for bindings and `[...]` for
  a catch-all, for example `<<"/users/:id">>` or `<<"/assets/[...]">>`.
- The declared path joined with every prefix above it: the route group's
  `prefix` and, for an included application, the prefix it is mounted under.
  An application mounted under `/sub` with a route `/hello` gives
  `<<"/sub/hello">>`.
- Set for controller routes, static file routes and websocket routes.
- Not set when nothing matched, so it is absent on `404` and `405` responses
  rendered by the router.
- Kept when a plugin or controller crashes and Nova renders a status page, so
  an error page or a `post_request` plugin still sees the route that failed.

The host is not part of the template. Two route groups on different hosts, or
two applications mounted under prefixes that produce the same path, share a
template, so pair it with `app` when you need to tell them apart.

The template has low cardinality, which makes it the right label for metrics
and trace span names. Use it instead of the raw path, which carries IDs.

## Adding routes programatically

You can also add routes programatically by calling `nova_router:add_routes/2`. This is useful if you want to add routes dynamically. The spec for it is:

```erlang
%% nova_router:add_routes/2 specification
-spec add_routes(App :: atom(), Routes :: map() | [map()]) -> ok.
```

First argument is the application you want to add the route to. The second argument is the route or a list of routes you want to add - it uses the same structure as in the regular routers.

```erlang
nova_router:add_routes(my_app, #{prefix => "/admin", routes => [{"/", fun my_controller:main/1, #{methods => [get]}}]}).
```

This will add the routes defined in the second argument to the `my_app` application.

**Note**: If a route already exists it will be overwritten. This is the one
place where a later route wins; routes compiled at startup keep the first.

Routes added this way live in the routing table of the running listener. When
Nova starts, each listener compiles its table from the routers again, so
routes added at runtime, and routes a router no longer returns, are gone
after a restart.

If the application has a router module you can leave the routes out and let
Nova call it for you:

```erlang
nova_router:add_routes(my_app).
```

To take an application's routes back out again:

```erlang
nova_router:remove_application(my_app).
```

Both work on the routing table of the default listener. If you started the
application on its own listener with `nova_sup:add_application/2`, use
`nova_sup:remove_application/1` instead, which also stops the listener once
nothing is left on it. See the [multi-app guide](multi-app.md).
