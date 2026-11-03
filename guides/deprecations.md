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

- **`nova_router:lookup_url/1,2,3`** keep their return shapes, including
  `{error, comparator_not_found, AllowedMethods}` for a path that exists but
  does not accept the method.

### Sessions

- **The session cookie is issued lazily.** Nova used to set a `session_id`
  cookie on every response to a client without one. It is now only sent with
  the response of a request that writes to the session (or calls `renew/1` or
  `delete/1`), and carries `Path=/; HttpOnly; SameSite=Lax`. As before, `nova_session_id` is only in the request for a client that
  sent no cookie; use `nova_session` rather than reading it, since that id only
  reaches the client once the session is written.

- **Presented ids the backend does not know are replaced**, for backends that
  implement `session_exists/1` (`nova_session_ets` does). Sessions held by
  another node, for example during a rolling deploy, are among them.

- **Sessions expire after 14 days by default.** `session_max_age` used to
  default to no limit; it is now `1209600` seconds, which also sets the
  cookie's `Max-Age`. Set `{session_max_age, infinity}` for the old behaviour.

- **The cookie is `Secure` unless the environment is `dev`.** It used to be
  `Secure` only when the request came in over HTTPS, which a proxy that
  terminates TLS hides. A staging environment served over plain HTTP needs
  `{environment, dev}` or `#{secure => false}` in `session_cookie_opts`.

- **Two different session cookies mean no session.** A request carrying two
  or more different session ids (for example one set for a parent domain) is
  treated as having none instead of using the first. A warning is logged once
  per node. The new `session_cookie_name` option takes a `__Host-` name,
  which prevents this; changing the name signs every client out once.

- **Session settings follow the bootstrap application.** `session_cookie_opts`,
  `session_max_age`, `session_cookie_name` and the new `redact_keys` are read
  like `use_sessions`: the bootstrap application first, then `nova`.

- **Without `nova_stream_h`, `renew/1` and `delete/1` set the cookie on the
  request they return.** Return that request from the controller.

- **A session manager without `session_exists/1` gets a warning at boot.**

- Use `nova_session:renew/1` at sign-in.
