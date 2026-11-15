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
  [routing guide](routing.md). It is set per application in `nova_apps`. A
  `plugin_strategy` key on a route entry, where the routing guide for 0.17.0
  to 0.18.1 put it, is ignored and logs a warning once per application.

- **`{stop, Req}` from a plugin is deprecated.** It still ends the request
  like `{stop, Req, State}` and keeps the plugin's state as it was, and logs
  a warning once per plugin module. It will be removed in 1.0; return
  `{stop, Req, State}` instead.

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

- **`terminate/3` is called with three arguments.** `nova_handler` checked
  for `terminate/3` and called `terminate/2`. Controllers get
  `terminate(Reason, Req, #{})`; plain Cowboy handlers get their own state. A
  controller that only exports `terminate/2` is not called, as before.

- **A handler registered as `{Module, Function}` is called with arity 3.**
  `nova_handlers:register_handler/2` used to build `fun Module:Function/4`,
  but handlers are called with three arguments. The form itself stays
  deprecated (see 0.9.24) and is removed in 1.0: register
  `fun Module:Function/3` instead.

- **A JSON object that has a key twice is refused.** With the default
  `json_lib`, a body such as `{"role":"user","role":"admin"}` is answered
  with a bare `400` by `decode_json_body`. The library that was the default before
  kept the last value of the key and OTP's `json` keeps the first, so a
  callback that checks the body and the code behind it could each have gone
  by another one. Bytes that follow the JSON value, other than whitespace,
  are refused the same way. A `json_lib` of your own decides both itself.
  The warning for a body that does not decode no longer has the reason of
  the library, which could hold bytes of the body: it has `error`, the kind
  of error, and `position` when OTP's `json` found it.

- **`decode_json_body` decodes `application/json` and `application/*+json`.**
  The type is matched in any case, with any parameters, and `+json` types
  such as `application/vnd.api+json` are now decoded too (RFC 6839). Types
  that only began with `application/json`, such as `application/json-seq`,
  are no longer decoded. Decode those in the controller from the request
  body, or with a plugin of your own. An empty or invalid body with a `+json`
  type now gets a `400` before the controller runs; such requests used to
  pass through undecoded.
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

- **CSRF `excluded_paths` cover whole segments.** They are matched against the
  path the request is routed on, segment by segment, where earlier releases
  compared the bytes of the raw path. `<<"/webhook">>` excludes `/webhook` and
  `/webhook/stripe` but no longer `/webhooks/stripe`, whose `POST` now gets a
  `403`. List every path that is to be excluded.

- **A static directory serves only what is inside it.** A file reached through
  a symbolic link that points out of the directory is answered with `404`.
  Move the file into the directory, or set `follow_external_symlinks => true`
  in the options of the route. The directory is the one its path named when
  the routes were compiled: a relative path does not follow a later change of
  the working directory, and a symbolic link on the way to the directory that
  is changed later is not followed until Nova is restarted.
  A route to a single file given as a relative path keeps to the file it named
  when the routes were compiled as well. A directory that is removed while
  Nova runs gets a `404` where its index used to get a `500`.

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
  the whole event used to be printed outside JSON. A value that is printed
  as a term, which a tuple or a list that is not a string also is, is cut
  off after about 4096 characters. Adjust log queries that match on the old
  output.

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

- **`override_secure` covers the applications an included application
  includes.** An application in the `nova_apps` of an included application
  was served without the `override_secure` of the entry that included its
  parent, and an `override_secure` given to an application in the `apps` of a
  route group replaced the one around it. The outer callback now runs for
  those routes too, in front of an `override_secure` of their own, and both
  have to allow the request. A request that reached such a route without
  passing the outer callback is now refused by it. Nothing else is handed
  down: see the [multi-app guide](multi-app.md#applications-included-by-an-included-application).

- **`secure` and the `security` of a route group both have to allow a
  request.** In a `nova_apps` entry, `secure` used to replace the `security`
  callback of every route group of the included application, `secure => false`
  included. A group now keeps its callback next to it: the `secure` callback
  runs first, the callback of the group second, and the first that does not
  allow the request answers it. What to look for after the upgrade:

  - A request that `secure` lets through to a group with a stricter callback
    of its own is now refused by that callback. It used to reach the
    controller.
  - A request that the callback of a group allows is still subject to
    `secure`, as it was.
  - With `secure => false`, a group that declares `security` is protected by
    it, where it used to be left open.
  - The controller gets the `auth_data` of the group's callback when that
    returns some, and otherwise that of `secure`.

  Use `override_secure` to replace a callback the application declares; see
  the [multi-app guide](multi-app.md#security-of-a-mounted-application).

### Sessions

- **The session cookie is issued lazily.** Nova used to set a `session_id`
  cookie on every response to a client without one. It is now only sent with
  the response of a request that writes to the session (or calls `renew/1` or
  `delete/1`), and carries `Path=/; HttpOnly; SameSite=Lax`. As before, `nova_session_id` is only in the request for a client that
  sent no cookie; use `nova_session` rather than reading it, since that id only
  reaches the client once the session is written.

- **Presented ids the backend does not know are replaced**, for backends that
  implement `session_exists/1` (`nova_session_ets` does). Sessions held by
  another node, for example during a rolling deploy, are among them. With
  `nova_session_ets` on several nodes without sticky sessions, or across a
  restart, users are signed out; use a shared session store there.

- **Sessions expire after 14 days by default.** `session_max_age` used to
  default to no limit; it is now `1209600` seconds, which also sets the
  cookie's `Max-Age`. Set `{session_max_age, infinity}` for the old behaviour.

- **The cookie is `Secure` unless `{nova, [{environment, dev}]}` is set.**
  It used to be `Secure` only when the request came in over HTTPS, which a
  proxy that terminates TLS hides. An unset `environment` is `prod`. Local development over plain HTTP without that setting loses the
  session cookie, as does a staging environment served over plain HTTP; set
  `{nova, [{environment, dev}]}` or `#{secure => false}` in
  `session_cookie_opts`.

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
