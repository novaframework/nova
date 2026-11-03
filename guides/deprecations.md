# Deprecations in Nova

We try to keep the Nova API as stable as possible, but sometimes we need to depreciate old functionality.
This is a list of the things that are currently deprecated in Nova.


## 0.9.24

- The old format with `{Module, Function}` is deprecated in favor of `fun Module:function/1`. This is a breaking change,
but it is a good time to do it now before we release 1.0.0. The old format will be removed in 1.0.0. This goes for all occurrences
of `{Module, Function}` in Nova.


## Unreleased

- `routing_tree` is no longer a dependency. Nova's dispatch table is now built
  by the in-tree `nova_routing_trie`. If you introspected the routing table by
  including `routing_tree.hrl` and matching on `#host_tree{}`, `#routing_tree{}`,
  `#node{}` or `#node_comp{}`, those records are gone. Use
  `nova_routing_trie:routes/1`, which returns
  `[{Host, Path, Method, HandlerValue}]`; the trie itself is opaque.

  Nothing changes for applications that only declare routes and let Nova serve
  them.

### Behaviour changes to be aware of

- **Route matching backtracks.** A request is now matched against every route
  that could serve it, so `/a/:x/c` matches `/a/b/c` even when a `/a/b/d` route
  exists. Previously matching committed to the first sibling it found and gave
  up, returning a 404. Routes that were unreachable before may start being
  reached.

- **Every binding at the same depth is reachable.** `/p/:id/picture` and
  `/p/:user_id/name` both work. Previously only whichever one happened to be
  visited first was reachable, and which one that was depended on insertion
  order.

- **`use_strict_routing` now takes effect.** It never reached the routing table
  before, so it has been a no-op. An application with genuinely conflicting
  routes and `use_strict_routing` set to `true` will now refuse to start.
  Conflicts are reported with both paths and the method.

- **An application's own status-code routes now win.** Nova's routes are
  compiled last, so a `{404, fun my_controller:not_found/1, #{}}` entry in your
  router replaces Nova's default error page. Previously Nova's was registered
  first and yours was silently ignored.

- **`nova:format_stacktrace/1` always returns an integer `arity`.** A frame
  that carries its argument list (the first frame of a `function_clause`)
  used to be returned with the arguments in `arity`; it now gets their count,
  so request data never reaches a log or an error page through it.

- **Plugins.** A route entry that declares `plugins` uses exactly those, and one
  that does not uses the globally configured ones - unchanged. The new
  `plugin_strategy` option lets you combine both; see the
  [routing guide](routing.md).

- **`{break, ...}` from a plugin is a deprecated alias of `stop`.** It ends the
  request and sends its reply, if it has one, and logs a warning once per
  plugin module. It will be removed in 1.0; return `{stop, ...}` instead. A
  plugin reply Nova does not know (anything but `{reply, ...}`,
  `{redirect, Url}` or `{status, ...}`) now renders the 500 page instead of
  being ignored. Reply headers may be a map or a list of pairs.

- **`{ok, Reply, Req, State}` from a plugin takes only `{reply, ...}`.** A
  redirect or a status there used to be overwritten by the controller's
  answer; it now renders the 500 page and logs that `stop` is needed.

- **The default error page shows crash details only in `dev`.** Details
  (the reason, extra messages and the stacktrace) are shown only when
  `{nova, [{environment, dev}]}` is set. An unset environment shows none.

- **`decode_json_body` and `read_urlencoded_body` match the content type
  case-insensitively,** so `Application/JSON` is decoded.

- **`{json, Status, Headers, Req, JSON}` is deprecated.** Return
  `{json, Status, Headers, JSON, Req}`, the order the docs always gave. The old
  order still works and logs a warning once per controller module. The order
  is read from the fourth element: it is taken as the request only when it
  looks like one (a map with `pid` and `streamid`) and the fifth does not.

- **`nova_cors_plugin`** answers only real preflights (`OPTIONS` with `Origin`
  and `Access-Control-Request-Method`) itself, with a `204`. Other `OPTIONS`
  requests reach the application.

- **`nova_cors_plugin` sends `access-control-allow-origin` only to a matching
  `Origin`.** With one or more origins configured, the header is sent only
  when the request's `Origin` is on the list, and never to a request without
  `Origin`. Responses carry `vary: origin`, appended to any `vary` already
  set. Header names are lowercase.

- **Static files are sent with `x-content-type-options: nosniff`** (200 and
  206). `.json`, `.txt`, `.woff2` and `.webp` files now get their real
  content type instead of `application/octet-stream`. XML, HTML and `+xml`
  types that only the full table knows (such as `.xhtml` and `.svgz`) are
  served as `application/octet-stream`, so a browser does not run them as a
  page; set `mimetype` on the route to serve one with its type.

- **The `plugin_strategy` option in `nova_apps` wins over a route entry's.**
  The route entry's `plugin_strategy` is now read too, and is used when the
  including application sets none; `local_or_global` is the default.

- **`terminate/3` is called with three arguments.** `nova_handler` checked
  for `terminate/3` and called `terminate/2`. Controllers get
  `terminate(Reason, Req, #{})`; plain Cowboy handlers get their own state. A
  controller that only exports `terminate/2` is not called, as before.

- **A handler registered as `{Module, Function}` is called with arity 3.**
  `nova_handlers:register_handler/2` used to build `fun Module:Function/4`,
  but handlers are called with three arguments. Export `Function/3`.

- **`decode_json_body` decodes `application/json` only.** The content type is
  matched exactly (in any case, with any parameters). Types that only began
  with `application/json`, such as `application/json-seq` or
  `application/json-patch+json`, are no longer decoded; neither were `+json`
  types such as `application/problem+json`. Decode those in the controller
  from the request body, or with a plugin of your own.
  `read_urlencoded_body` likewise matches `application/x-www-form-urlencoded`
  exactly.

- **`nova_request_plugin` reads at most 1 MB of body, within 60 seconds.**
  With `decode_json_body` or `read_urlencoded_body` set, a body longer than
  `max_body_length` (default `1000000` bytes) gets a `413`: before it is
  read when `Content-Length` says so, otherwise as soon as it passes the
  limit. A body still arriving after `body_read_timeout` (default `60000`
  ms, and at least `1`) gets a `408`. Both close the connection. Raise
  `max_body_length` on the routes that take larger bodies; set both to
  `infinity` for the old behaviour.

- **`nova_router:lookup_url/1,2,3`** keep their return shapes, including
  `{error, comparator_not_found, AllowedMethods}` for a path that exists but
  does not accept the method.
