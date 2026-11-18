# Routing

Each Nova application has a router module. Its `routes/1` callback returns the routes of the application as a list of *route groups*. The module is named after the application with `_router` appended, such as `my_app_router`; `router_module` in the application environment names another one. In an Elixir application the router is `MyApp.Router`, for an application named `:my_app` as well as for one named by its module. When `router_module` is not set and no module of that name is found, Nova does not start, with `{router_module_not_found, App}`.

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

A `GET` request to `/admin` calls `my_controller:main/1`. The argument of `routes/1` is the value of `nova:get_environment/0`, which lets a router return different routes for different environments. Set it with `{environment, Env}` under the `nova` application in *sys.config*. When it is not set it is read as `prod`, so routes a router only returns for `dev` are not served.

### Route groups

A route group is a map. Its keys are all optional:

| Key | Description |
|---|---|
| `prefix` | A path put in front of every route of the group |
| `security` | The security callback of the group, `false` or a `fun/1`. A group without the key has none, see [Secure routing](#secure-routing) |
| `plugins` | The plugins of the group, see [Using plugins local to a set of endpoints](#using-plugins-local-to-a-set-of-endpoints) |
| `plugins_before_security` | Plugin modules whose `pre_request/4` runs ahead of the security callback, see [Plugins](plugins.md#running-a-plugin-ahead-of-the-security-callback) |
| `security_reads_body` | Whether the security callback of the group reads the body that `nova_request_plugin` decodes. `false` says that it does not, and ends the warning and the notice about a body that is decoded behind the callback. It answers for the callback the group declares as its `security` only, not for one the application is mounted behind. `true` says that it does, and the routes then only compile with the plugin listed under `plugins_before_security`. See [Plugins](plugins.md#saying-whether-the-security-callback-reads-the-body) |
| `routes` | The routes |
| `host` | Serve the group only for requests to this host, a string or a binary. A host that has groups of its own is served by those alone: groups without `host` are not consulted for it |
| `extra_state` | A term handed to the controllers of the routes of the group that set no `extra_state` themselves |
| `apps` | Other Nova applications to mount under the prefix of the group, see [Multiple applications](multi-app.md) |
| `listener` | The listeners the group is served on, a name or a list of names, see [The listeners of a route group](#the-listeners-of-a-route-group) |
| `http_versions` | The HTTP versions the routes of the group are served over, see [The HTTP versions of a route](#the-http-versions-of-a-route) |

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

The `init/1` callback of a websocket controller returns `{ok, State}` to accept the connection. Any other return refuses the upgrade with a `500`. It can return `{ok, State, Options}` to also set the options of the connection, where `Options` is a `nova_websocket:opts()` map such as `#{max_frame_size => 65536, idle_timeout => 30000}`. `idle_timeout`, `max_frame_size` and `compress` are taken by every server. Other keys are the server's own, such as those of [cowboy_websocket](https://ninenines.eu/docs/en/cowboy/2.21/manual/cowboy_websocket/) on Cowboy.

A browser sends the cookies of your site with a websocket upgrade started by any page. Nova therefore checks where every request to a websocket route comes from, by its `Origin` and `Sec-Fetch-Site` headers, against `trusted_origins`, and answers one from another origin with `403`. See [The origin check](websockets.md#the-origin-check).

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

A directory route only serves what is inside that directory. A request whose trailing segments are empty, `.` or `..`, or contain `/`, `\` or a NUL byte, is answered with `404`, whatever the router in front of it let through. Symbolic links are resolved first: a link that points inside the directory is served, one that points out of it is a `404`. Set `follow_external_symlinks => true` in the options of the route to serve links that leave the directory. The directory itself is resolved once, when the routes are compiled, and files are checked against and served from where it was then. A relative path means the directory it named in the working directory the node had at that time, also after the working directory has changed. If a symbolic link on the way to the directory is changed while Nova runs, files are still served from where it pointed before, so restart Nova after such a change. With `follow_external_symlinks` no link is resolved, and such a change takes effect at once. A route to a single file given as a relative path also serves the file that path named when the routes were compiled. A request under a directory that has been removed since, or cannot be resolved, is answered with `404`, and the first one is logged as the error `Static directory cannot be served`. A file that is missing from a directory that is there is a `404` without a log entry. A route to a single file whose file is gone or cannot be read is answered with `404` and logged once as the error `Static file cannot be served`.

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

* A route group without a `security` key has no callback. Its routes are public, unless a `nova_apps` entry the application is mounted through has `secure` or `override_secure`, or the application is listed under the `apps` key of a route group with `security`.
* A `security` or `secure` key in the options map of a single route is not read. Put routes that need another callback in a route group of their own.
* A `secure` key in a route group is not read. The key of a route group is `security`.
* A `security` key in a `nova_apps` entry is not read. The key of an entry is `secure`.

A callback written in one of those places would protect nothing, so Nova refuses the routes: with any value but `false` it logs the error `This key is not read here, so the callback would protect nothing ...` for each such key, with the application, the path and the key, and the application does not start, with `{misplaced_security, App, Key, Path}`. `nova_router:add_routes/2,3` throw `{error, {misplaced_security, App, Key, Path}}` for the same routes. With `false` as the value the key is logged as a warning and the routes are compiled.

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

The callback runs after `pre_security/4` of the `pre_request` plugins of the route, and before their `pre_request/4` and the controller. The [request pipeline](plugins.md#request-pipeline) section of the plugins guide has the whole order.

`security` takes `false`, a `fun/1` or the deprecated `{Module, Function}`. Any other value, `true` included, is logged as an error and stops the application from starting with `{invalid_security, Value}`. The same goes for `secure` and `override_secure` in `nova_apps`.

The security function runs before a plugin reads the body, so `json`, `params` and `files` are not in the request it gets. `plugins_before_security => [Module]` in the route group runs `pre_request/4` of the listed plugins ahead of the security function; see [Running a plugin ahead of the security callback](plugins.md#running-a-plugin-ahead-of-the-security-callback). A route group whose callback does not read them says so with `security_reads_body => false`, see [Saying whether the security callback reads the body](plugins.md#saying-whether-the-security-callback-reads-the-body).

### The security-function callback

A security function takes the request and returns one of the values below. With `{true, Data}` it hands data to the controller, such as the user and its roles.

#### Return values for the secure function

| **Return value** | **Description** |
| ---------------- | --------------- |
| `true` | The request will continue to the controller. |
| `{true, Data :: term()}` | The request will continue to the controller and the data will be available in the `Req` object under the `auth_data` key. |
| `{redirect, Url :: binary() \| string()}` | The request will be redirected to the specified URL with HTTP status 302. The URL can name another site. |
| `{redirect_local, Path :: binary() \| string()}` | As above, but only to a path on the site the request was made to, such as `/login`. Anything else, `https://example.com/` and `//example.com/` included, is answered with `400` and logged as a warning. Use it when the target comes from the request. |
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

Once the security function has been called, the data will be available in the `Req` object under the `auth_data` key. A websocket controller gets it under `auth_data` in its controller data, and in the upgrade request under `req` there. A [plugin](plugins.md#before-and-after-the-security-callback) cannot put something else in its place. What a plugin writes under `auth_data` stays only when the callback returns a plain `true`, and on a route without a callback.

A route group of an [included application](multi-app.md#security-of-a-mounted-application) can have several callbacks: the `secure` of every `nova_apps` entry its application is mounted through, and its own `security`. They run in that order and all have to allow the request. The controller gets the data of the last callback that returned some.

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
that application. A `plugin_strategy` key on a single route entry is
ignored, and a warning is logged once per application.

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
itself, as `secure` does. One of those can have an `override_secure` of its
own, and then both callbacks have to allow a request. The
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


## The listeners of a route group

A node can have several listeners, see [Servers and listeners](listeners.md).
A route group is served on every listener that serves its application. With
`listener` it is served on the listeners it names and on no other:

```erlang
routes(_Environment) ->
    [#{prefix => "/",
       routes => [{"/", fun my_controller:index/1, #{methods => [get]}}]},
     #{prefix => "/admin",
       listener => admin,
       security => fun my_auth:admin/1,
       routes => [{"/users", fun my_admin_controller:users/1, #{methods => [get]}}]}].
```

```erlang
{listeners, #{
    public => #{port => 8443, tls => Tls, applications => [my_app]},
    admin => #{ip => {127, 0, 0, 1}, port => 9090, applications => [my_app]}
}}
```

`/admin/users` is answered on port 9090 and is a `404` on port 8443. The
applications the group mounts with `apps` are served where the group is.

Every listener the group names has to serve the application. A name that is
not a listener, or a listener without the application in its `applications`,
stops the node from starting with
`{route_group_not_served, App, Prefix, Listener}`.

Routes added at runtime with `nova_router:add_routes/2,3` follow the same
rule: a group that names listeners is added to the table of one of them
alone, and Nova logs a notice for a table the group is left out of. An
application added with `nova_sup:add_application/2` whose group names a
listener that does not serve it gets a warning in the log, not a stopped
node.

`listener` is not a security callback. It keeps a group off the listeners
that do not have it, and whoever reaches the listener that has it still
needs to be let through.

## The HTTP versions of a route

A route is served over every HTTP version its listener speaks. With
`http_versions`, a list out of `http1`, `http2` and `http3`, it is served
over those alone. The key goes in the options of a route, or in a route
group for every route of the group that has none of its own:

```erlang
#{prefix => "/api",
  http_versions => [http2, http3],
  routes => [
      {"/stream", fun my_api:stream/1, #{methods => [get]}},
      {"/hook", fun my_api:hook/1, #{methods => [post], http_versions => [http1, http2]}}
  ]}
```

A request over another version is not routed. Over HTTP/1.1 it is answered
with `426` and an `upgrade` header that names the versions to come back
with. Over HTTP/2 and HTTP/3, which have no such header, it is answered with
`505`. Both go through the [status-code routes](#status-code-routes), and no
plugin or security callback runs.

The `upgrade` header is there because a `426` has to have one, and for
whoever reads the response. A client cannot switch on it: HTTP/2 is agreed
on in the TLS handshake and HTTP/3 is found through `alt-svc` or DNS, so a
client that speaks the version comes back on a new connection. What the
header holds may change between releases.

The version is that of the connection Nova gets. Behind a proxy or a load
balancer that speaks HTTP/1.1 to the node, every request is `http1`
whatever the client spoke, and a route for `[http2, http3]` alone answers
`426` to all of them. Use `http_versions` where clients reach the listener
directly, or where the proxy keeps the version.

A route that asks for versions its listener serves none of stops the node
from starting with
`{route_http_versions_not_served, Path, Versions, Listener}`. So does a
websocket route on a listener without a version the server takes websockets
over, with `{websocket_route_not_served, Path, Listener}`. A route group
that is only meant for some listeners names them with `listener`.

`nova_req:http_version/1` tells a controller which version a request came
over.

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

Once a path matches, the method is resolved. An exact method wins over a route
declared with `'_'`. If the path matches but the method does not, Nova answers
`405` with an `allow` header listing the methods that path does accept. No
plugin runs for such a request, with one exception: a CORS preflight runs
`pre_security/4` of the plugins of the route for the method it asks about
before the `405`, so that `nova_cors_plugin` can answer it.

If two routes declare the same path *and* the same method, the first one
registered wins and the second is logged and ignored. Set
`use_strict_routing` to `true` in the `nova` application environment to make
Nova refuse to start on a conflict instead, which also reports overlapping
literal and binding routes.

## Paths that are not canonical

A request whose path is not written the way it is routed is answered with
`400` before a route is looked up. No plugin and no security callback runs
for it. The answer is the `400` status-code route of the application when it
has one, and a `400` without a body otherwise.

| Path | Answer |
|---|---|
| `//a`, `/a//b`, `/a//` | `400`: an empty segment ahead of the last one |
| `/a/./b`, `/a/../b`, `/a/..` | `400`: a `.` or `..` segment |
| `/a/%2e%2e/b`, `/a/%2E/b` | `400`: the same, percent-encoded |
| `/a/` | Routed as `/a`. One trailing slash is not refused |
| `/a/...`, `/.well-known/a`, `/a%2fb`, `/a%5cb` | Routed as written |
| `a`, a path without a leading slash, which HTTP/2 lets a client send | `400`, whatever the setting is |

A proxy, a cache or a filter in front of Nova may read such a path another
way than Nova does. Refusing it leaves nothing to disagree about.

`non_canonical_paths` in the `nova` environment takes `reject`, the default,
and `resolve`:

```erlang
{nova, [{non_canonical_paths, resolve}]}
```

With `resolve` a path is routed after empty segments are dropped and `.` and
`..` are resolved, as in earlier releases: `/public/../admin//users` is
routed as `/admin/users`. Any other value stops the boot with
`{invalid_setting, non_canonical_paths, Value}`.

The `path` in the request map is what the client sent, in both modes. With
`reject` it still differs from the routed path by a trailing slash. A plugin
or security callback that decides by path must use
`nova_router:canonical_path/1`, and compare whole segments: `/api` is not a
prefix of `/apikeys`.

```erlang
is_admin_path(Req) ->
    case binary:split(nova_router:canonical_path(Req), ~"/", [global, trim_all]) of
        [~"admin" | _] -> true;
        _ -> false
    end.
```

The `excluded_paths` of `nova_csrf_plugin` are matched this way.

## Status-code routes

A route whose path is an integer handles that status code:

```erlang
#{routes => [{404, fun my_error_controller:not_found/1, #{}},
             {500, fun my_error_controller:server_error/1, #{}}]}
```

Nova registers its own 404 and 500 handlers; an application's routes for the same codes replace them. A status-code route is rendered when a path is not canonical (400), when no route matches (404), when the method is not allowed (405), when a controller, a plugin or the security function crashes or a plugin returns `{error, Reason}` (500), when a controller throws `{Status, Reason}`, and when a controller returns something no handler understands. It is *not* rendered for a `{status, Code}` returned by a controller, nor for a request the security function refuses or a plugin stops.

A status-code route that crashes, or throws `{Status, Reason}` itself, is not rendered a second time. The request gets the 500 page; when it was the application's 500 page that failed, Nova's own; and a `500` without a body should that fail as well.

## The matched route

When a request matches a route, Nova stores the route's template in the
request map under `nova_route`. Controllers, plugins and
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

Routes are added to the table as it was when the call began. When another
process stored routes into the table meanwhile, or removed an application
from it, yours are compiled again into the table as it is by then, so
nothing either of you stored is lost. After three tries the call raises the
error `{routing_table_changed, DispatchKey}`, and none of its routes are in
the table. `{routing_table_replaced, DispatchKey}` is raised when the
listener of the table was started again meanwhile, or is being started.

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
