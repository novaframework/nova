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
