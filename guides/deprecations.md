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

- **Plugins.** A route entry that declares `plugins` uses exactly those, and one
  that does not uses the globally configured ones - unchanged. The new
  `plugin_strategy` option lets you combine both; see the
  [routing guide](routing.md).

- **`nova_router:lookup_url/1,2,3`** keep their return shapes, including
  `{error, comparator_not_found, AllowedMethods}` for a path that exists but
  does not accept the method.

- **CSRF `excluded_paths` cover whole segments.** They are matched against the
  path the request is routed on, segment by segment, where earlier releases
  compared the bytes of the raw path. `<<"/webhook">>` excludes `/webhook` and
  `/webhook/stripe` but no longer `/webhooks/stripe`, whose `POST` now gets a
  `403`. List every path that is to be excluded.

- **A static directory serves only what is inside it.** A file reached through
  a symbolic link that points out of the directory is answered with `404`.
  Move the file into the directory, or set `follow_external_symlinks => true`
  in the options of the route.

- **A `Range` that is not valid, or is sent with `If-Range`, gets the whole
  file.** A static file answered a byte range that is not valid, such as
  `bytes=5-2`, with `416`, and took no notice of `If-Range`. Both now get the
  whole file with `200`, as RFC 9110 asks for.

- **An invalid `security` value stops the boot.** `security => true` was logged
  and the group left open, and any other value that is not `false`, a `fun/1`
  or `{Module, Function}` crashed on every request. Both now fail the start
  with `{invalid_security, Value}`, and so do `secure` and `override_secure`
  in `nova_apps`. Give the group a callback, or `false` for none.

- **An invalid route method stops the boot.** Only the lowercase atoms `get`,
  `post`, `put`, `delete`, `options`, `head`, `connect`, `trace` and `patch`
  were recognised, and anything else was treated as `'_'`, so the route
  answered every method. An atom that is none of those in either case, or a
  binary or string that is not a method token, now fails the start with
  `{invalid_method, Method}`. A route declared with `<<"GET">>`, `"GET"` or
  `'GET'` answers only that method from now on, and `405` for the others. If
  it was meant to answer every method, declare `'_'`.

- **Methods are case-sensitive.** A method written as a binary or a string is
  matched as it is written, and so is the method of a request: `get` does not
  match a route declared for `GET` and gets a `405`. Write `<<"PURGE">>`, not
  `<<"purge">>`.

- **A correlation ID from the request is checked.** `nova_correlation_plugin`
  takes it over only when it is 1 to 255 visible ASCII characters. Anything
  else is replaced by a generated ID, so a caller that sends such a value no
  longer finds it in the response or in the logs.

- **`nova_jsonlogger` encodes strings as UTF-8.** A string with characters
  above 255 was printed as a list of integers and is now text. A binary or
  string that is not UTF-8 is printed as an Erlang term, `"<<255,0>>"`, where
  the whole event used to be printed outside JSON. Adjust log queries that
  match on the old output.

- **A second listener renders its own status pages.** The page for a crashed
  controller or a thrown `{Status, Reason}` is looked up in the routing table
  of the listener the request came in on, and is Nova's default page when that
  listener declares none. Earlier releases rendered the default listener's
  page everywhere. Declare the status code routes in the application each
  listener serves.

- **Your own routes win a clash with an included application.** Where both
  declare the same path and method, or a page for the same status code, the
  included application's route is ignored and a warning names it. Earlier
  releases registered included applications first, so theirs was served. Move
  or remove one of the two; with `use_strict_routing` the clash stops the
  boot.

- **A listener that cannot be started stops the boot.** A port in use or an
  unreadable certificate fails the start with `{listener_not_started, Reason}`.
  Earlier releases logged the error and kept running without a listener, so a
  node that used to come up without serving anything now does not come up.

- **`secure` no longer replaces a callback a route group declares.** In a
  `nova_apps` entry it applies to the groups of the included application that
  declare no `security`, and is ignored, with a warning, by a group that
  declares one. Earlier releases let it replace the group's callback,
  `secure => false` included. Use `override_secure` to replace a callback the
  application declares; see the [routing guide](routing.md#overriding-security-for-an-included-application).
