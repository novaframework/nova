# Deprecations in Nova

We try to keep the Nova API as stable as possible, but sometimes we need to deprecate old functionality or change how something behaves.
This page lists what is deprecated and what changed, by the release that made the change. The [Upgrading](upgrading.md) guide says what to do about the changes of the latest releases.


## 0.9.24

- The old format with `{Module, Function}` is deprecated in favor of `fun Module:function/1`. This is a breaking change,
but it is a good time to do it now before we release 1.0.0. The old format will be removed in 1.0.0. This goes for all occurrences
of `{Module, Function}` in Nova.


## 0.10.3

- The `ca_cert` and `cert` keys of `cowboy_configuration` are deprecated in
  favor of `ssl_options`, which takes the transport options of the TLS
  listener as a map. The old keys still work and log a deprecation warning.


## 0.17.0

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
  [routing guide](routing.md). It is set per application in `nova_apps`. A
  `plugin_strategy` key on a route entry, where the routing guide for 0.17.0
  to 0.18.1 put it, is ignored and logs a warning once per application.

- **`nova_router:lookup_url/1,2,3`** keep their return shapes, including
  `{error, comparator_not_found, AllowedMethods}` for a path that exists but
  does not accept the method.


## Next release

These changes are part of the release that follows 0.18.3.

### Defaults that changed

The [Upgrading](upgrading.md#defaults-that-changed) guide says who each of
these affects and how to get the earlier behaviour back.

- **An unset `environment` is read as `prod`.** It was read as `dev`.
  `nova:get_environment/0` returns `prod` for it, routers are called with
  `routes(prod)`, and Nova logs one notice at boot.

- **A path that is not canonical is answered with `400`.** A path with an
  empty segment ahead of its last one, or with a `.` or `..` segment, was
  resolved and routed. `{non_canonical_paths, resolve}` routes it as before.

- **`pre_request/4` of a plugin runs after the security callback.** The new
  optional callback `pre_security/4` runs ahead of it. `nova_cors_plugin`,
  `nova_correlation_plugin` and `nova_ws_origin_plugin` work there and have
  no `pre_request/4`. `nova_request_plugin` parses the query string there
  and reads the body in `pre_request/4`. `nova_csrf_plugin` checks the
  origin there and the token in `pre_request/4`. `plugins_before_security`
  names the plugins whose `pre_request/4` runs ahead of the callback as it
  did. `auth_data` of the request is what the security callback returned as
  `{true, AuthData}`, also when a plugin writes the key in `pre_request/4`.

- **What the new order changes is logged when routes are compiled.** A route
  group whose security callback runs before `nova_request_plugin` reads the
  body, and one with a plugin from outside Nova that has no `pre_security/4`,
  is named once, in a report that has `notice` set to `body_behind_security`,
  `form_behind_security`, `pre_request_behind_security` or
  `pre_request_not_for_preflight`. The new route group key
  `security_reads_body` answers the first two: `false` says the callback of
  the group does not read the body and ends them, `true` says that it does
  and stops the boot until the plugin is listed under
  `plugins_before_security`. `false` answers for the callback the group
  declares as its `security`, not for one its application is mounted behind.
  See [Plugins](plugins.md#warnings-about-the-order).

- **A CORS preflight is answered for a route that does not take `OPTIONS`.**
  `pre_security/4` of the plugins of the route for the method the preflight
  asks about runs before the `405`.

- **Websocket routes check where a request comes from.** Every request to a
  websocket route is checked against `trusted_origins`, which is `same_host`
  when it is not set. `same_host` takes an origin with the host and port the
  request was sent to, and on a request that came in over TLS only an
  `https` one. An upgrade whose `Sec-Fetch-Site` header is there and is
  neither `same-origin` nor `none` is refused unless its `Origin` is in a
  list of `trusted_origins`, or the origins are `any`. A client that sends
  neither header is let through. Behind a proxy that terminates TLS, list
  the `https` origins of the site in `trusted_origins`: not every browser
  sends `Sec-Fetch-Site` with a websocket upgrade.

- **`nova_csrf_plugin` checks where a request comes from.** A request that
  changes state is refused when `Sec-Fetch-Site` and `Origin` say that a
  page of another site sent it. It goes by `trusted_origins` too, where `any`
  of the `nova` environment is read as `same_host`. The plugin option
  `trusted_origins => any` turns the check off for a route group.

- **`secret_key_base` is the secret of the node.** Nova makes a key for each
  purpose from it and never signs with the secret itself. With it set,
  `nova_csrf_plugin` signs its tokens and stores nothing for them, and
  session ids are signed. Without it the token is kept in the session,
  handed out under a mask that is new for every response, and a notice is
  logged at boot unless `csrf_token` is `session`.

- **`nova_csrf_plugin` on a node without sessions stops the boot.** A route
  group that lists the plugin while `use_sessions` is `false` is refused with
  `{invalid_setting, use_sessions, {required_by, nova_csrf_plugin}}`, and
  `{csrf_token, stateless}` there with
  `{invalid_setting, csrf_token, {requires, use_sessions}}`. Every request
  to the routes of the plugin would fail on such a node.

- **Six settings are checked at boot.** `non_canonical_paths`,
  `trusted_origins`, `csrf_token`, `secret_key_base`, `max_sessions` and
  `plugins_before_security` are read from the `nova` environment when Nova
  starts. A value that cannot be used stops the start with
  `{invalid_setting, Key, Reason}`.

- **A `secret_key_base` that plainly is not random stops the boot.** One that
  is shorter than 32 bytes, has fewer than 8 different bytes (a letter or a
  word repeated), is an example out of the documentation, or is a
  `${VARIABLE}` that was not filled in is refused with
  `{invalid_setting, secret_key_base, redacted}`. Make one with
  `base64:encode(crypto:strong_rand_bytes(48)).` in an Erlang shell, or with
  `openssl rand -base64 48`.

- **Three keys that were in no release are refused.** `allowed_origins`,
  `csrf_secret` and `plugins_after_security` were read by builds between
  0.18.3 and this release, and never by a release. A node or a route group
  that still has one does not start, with
  `{invalid_setting, Key, {replaced_by, Setting}}`: `allowed_origins` is
  `trusted_origins`, with the values it had, `csrf_secret` is
  `secret_key_base`, and `plugins_after_security` is gone, with
  `plugins_before_security` as the one setting that changes the order.

- **`json_lib` defaults to `json` of Erlang/OTP.** It was `thoas`, which
  Nova no longer depends on.

- **`nova_session:client_id/1` is new.** It returns the session id the
  client holds, and hands one out in the session cookie to a client without
  one, without starting a session.

- **Telemetry.** Plugin spans have `callback => pre_security` for
  `pre_security/4`. The controller span of a status page that the router or
  a failing plugin or security callback asked for has `status_page => true`.

### Behaviour changes to be aware of

- **`nova:format_stacktrace/1` always returns an integer `arity`.** A frame
  that carries its argument list (the first frame of a `function_clause`)
  used to be returned with the arguments in `arity`; it now gets their count,
  so request data never reaches a log or an error page through it.

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

- **`handle_websocket/3` of `nova_basic_handler` is deprecated.** Nothing in Nova
  calls it: a websocket route is one with `protocol => ws`, upgraded by
  `nova_ws_handler`. It is removed in 1.0.

- **Cowboy 2.21.** The requirement moved from `2.18.0` to `~> 2.21.0`, with
  Cowlib 2.22. Over HTTP/2, a request whose headers decode to more than
  32 768 bytes is reset with `enhance_your_calm`; earlier releases had no
  limit on the decoded list. Set `max_header_list_size` in the `options` of
  `cowboy_configuration` for clients that send more.

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
  The warning for a body that does not decode is `Request refused`, where it
  was `Failed to decode json.`. It no longer has the reason of the library,
  which could hold bytes of the body: it has `error`, the kind of error, and
  `position` when OTP's `json` says where it is.

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
  Nova runs gets a `404` where its index used to get a `500`, and so does a
  route to a single file whose file is gone. Both are logged once.

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

- **A response that has begun is not answered again.** A handler or a
  controller that streamed with `cowboy_req:stream_reply/3` and returned the
  request it streamed with was answered a second time, which was logged as a
  crash. Nothing more is sent now. A failure after the response has begun is
  logged, with a warning that no error page is sent, and the client keeps
  what it was sent. See [Handlers](handlers.md#long-lived-responses).

- **A request is answered once.** A controller, a plugin or a security
  callback that answered with `cowboy_req:reply/4` and did not hand back the
  request that call returned was answered a second time. The second response
  is now dropped: the client keeps the first one, the connection stays open,
  and the warning `The request was answered already, so this response is not
  sent ...` is logged once per request, with the status of the dropped
  response and the method. It does not name the controller. A reply a plugin
  returns with `stop` for a request that was answered already is dropped as
  well, with one warning per plugin.

- **The application that starts a listener reads its own `nova_apps`
  entry.** An entry an application wrote for itself in its own `nova_apps`
  was skipped. The `prefix`, `secure`, `override_secure` and
  `plugin_strategy` of that entry now apply to the routes of the application
  named in `bootstrap_application`, and of one started with
  `nova_sup:add_application/2`. Its `secure` and `override_secure` also
  cover the applications it mounts. A notice names the application and the
  keys of the entry when the routes are compiled. Remove the entry if the
  options are not meant to apply.

- **A security callback under a key that is not read stops the boot.** A
  `secure` key in a route group, a `security` or `secure` key in the options
  of a single route, and a `security` key in a `nova_apps` entry are not
  read and never were. With any value but `false`, each is now logged as an
  error and the start fails with `{misplaced_security, App, Key, Path}`;
  `nova_router:add_routes/2,3` throw
  `{error, {misplaced_security, App, Key, Path}}`. With `false` the key logs
  a warning. Declare the callback as `security` on the route group, or as
  `secure` on the `nova_apps` entry.

- **`secure` and the `security` of a route group cover nested
  applications.** The `secure` of a `nova_apps` entry did not reach the
  applications the included application includes, and the `security` of a
  route group did not reach the applications under its `apps` key. Both now
  run for those routes, outermost first, in front of the callbacks those
  applications declare, and every one has to allow the request. A request
  that reached such a route without passing them is now refused.
  `override_secure` still replaces all of them.

- **An application mounted twice with different security stops the boot.**
  An application that two `nova_apps` entries mount is compiled once, for
  the entry that comes first. When the entries ask for different `secure` or
  `override_secure` callbacks, counting the inherited ones, the start now
  fails with `{nova_app_mounted_twice, App}`.

- **Security callbacks and plugins take `{redirect_local, Path}`.** It
  answers `302` to a path on the site the request was made to and `400` for
  anything else, as it does for a controller.

- **A plugin is initialised when its first route is compiled.** A plugin
  that first appeared in routes added with `nova_router:add_routes/2` or
  `nova_sup:add_application/2` was called with `undefined` as its state.
  `init/0` now runs when the router compiles the route, and the caller waits
  for it. It must not call into `nova_sup`.

- **`post_ws_connection` plugins always run.** They ran only for a
  controller that exports `terminate/3`, and not when that callback
  crashed.

- **A websocket callback that returns something unknown closes the
  connection.** It crashed the connection process. It is now logged as
  `Websocket handler failed on what the controller returned`.

- **`nova:get_apps/0` lists the applications of every listener**, those of
  the default listener first. `nova_router:dispatch_keys/0` lists the
  routing tables there are.

- **Watcher arguments that are not strings skip the watcher.** They stopped
  `nova_watcher`. `Could not start watcher` is logged and the other watchers
  run.

- **A status code route finds its data in the request.** What
  `nova_router:render_status_page/3,5` is given is in the request of the
  page, as `#{status => StatusCode, data => Data}` under `controller_data`.

- **Router error reports have their text under `msg`.** Thirteen reports
  had it under `reason`.

- **The default logger key of `nova_correlation_plugin` is an atom.** The id
  was put in the logger metadata under the binary `<<"correlation-id">>`,
  and is now under the atom `'correlation-id'`. A binary given as
  `logger_metadata_key` is used as the atom of that name when the node knows
  one; otherwise an error is logged and the default is used.

- **`nova_jsonlogger` redacts the arguments of a format string.** With
  `redact => true`, an event logged as a format string with arguments has
  its arguments redacted before they are put into the text. Arguments that
  no longer fit the format string are printed as a term next to it. A
  message logged as a plain string is not changed.

- **A route takes the `extra_state` of its route group.** A route without
  `extra_state` had `undefined` in the request, whatever its group declared,
  while a websocket route got the one of the group. Both now do. A route in a
  group that declares none still has `undefined`.

- **The security data of a websocket route is in the upgrade request.** What
  the callback returned in `{true, AuthData}` was only in the controller
  data. It is now under `auth_data` in the upgrade request as well, where the
  callback of the route group and the plugins that run after it find it. The
  controller data has the `auth_data` of the upgrade request, so it also has
  what a plugin put there on a route whose callback returned a plain `true`,
  or that has no callback.

- **Nova's `404` and `500` pages negotiate on `accept`.** They compared the
  header with `application/json` and `text/html` as text, and the `500` page
  had a body only when the header was exactly one of the two. Both pages now
  read media ranges and quality values: JSON for a request without the
  header or with `*/*`, the form the header prefers otherwise, and no body
  only when it admits neither. See
  [Error handling](error-handling.md#the-default-pages).

- **Nova's `500` page has a body in every environment.** Outside `dev`, a
  crash in a plugin was answered with a bare `500`.
  It now gets the generic page, without the class, the reason or the
  stacktrace.

- **`render_error_pages` only turns Nova's own pages off.** With `false`, a
  controller that crashed or threw a status was answered with the bare
  status code and no page was looked up, also not a status code route of
  the application, while Nova's `404` page was still sent. The status code
  routes of the application now run whatever the setting is, and with
  `false` Nova's `404` and `500` pages send the bare status code to every
  client. See [Error handling](error-handling.md#the-default-pages).

- **A view without a template is a `500`.** It was a `404`. The `500` page
  names the template in `dev`.

- **The router of an Elixir application is found by either name.** An
  application named `:my_app` had its router looked up as `my_app.Router`.
  `MyApp.Router` is now found as well.

- **The `headers` of a view are added to the default.** The `headers` option
  of `{ok, Variables, Options}` and `{view, Variables, Options}` replaced
  `content-type: text/html`, so a view that set another header was sent
  without a content type. The default now stays unless the option names a
  `content-type`.

- **A `url` tag without `application` renders `#`.** It crashed the view. It
  now logs the warning an unknown application gets.

- **A missing router module is reported.** An application without a
  `router_module` and without a module named `<app>_router` crashed the start
  with `undef`. The start now fails with `{router_module_not_found, App}`.

- **`nova:use_stacktrace(false)` returns `ok`.** It returned `true` or
  `false`.

### Sessions

- **The session cookie is issued lazily.** Nova used to set a `session_id`
  cookie on every response to a client without one. It is now only sent with
  the response of a request that writes to the session (or calls `sign_in/3`,
  `renew/1`, `delete/1` or `client_id/1`), and carries `Path=/; HttpOnly; SameSite=Lax`. As before, `nova_session_id` is only in the request for a client that
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
  proxy that terminates TLS hides. An unset `environment` is read as `prod`.
  Local development over plain HTTP without that setting loses the
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

- **Without `nova_stream_h`, `sign_in/3`, `renew/1` and `delete/1` set the
  cookie on the request they return.** Return that request from the
  controller.

- **A session manager without `session_exists/1` gets a warning at boot.**

- **`nova_session_ets` holds at most `max_sessions` sessions**, 1 000 000 by
  default, on each node. The setting is an integer above 0, or `infinity`. In a full table a write that starts a session drops
  another one: an expired one, then a provisional one, then, for
  `nova_session:set/3` only, the session nearest its expiry.

- **`nova_session:set_provisional/3` is new.** It writes a value for a
  visitor who is not signed in. With `nova_session_ets`, a session that only
  holds such values lives one day, is dropped first from a full table, and
  is refused with `{error, session_store_full}` when the table holds nothing
  but sessions written with `set/3`. `nova_csrf_plugin` stores its token
  this way when it keeps tokens in the session. A backend takes these writes in the new optional callback
  `set_provisional_value/3`, and in `set_value/3` without it.

- **`session_exists/1` is asked once for the reads of a request.** For reads
  the answer is kept for one second by the process that calls
  `nova_session`. Every write asks again.

- **Session values are typed as `term()`.** The typespecs said `binary()`,
  while any term was passed through to the backend.

### Uploads

- **Spooled uploads belong to the request.** `nova_multipart_file_handler`
  left every file in its directory. They are now deleted when the request is
  over, unless the controller calls `nova_multipart_spool:keep/1` or moves
  the file.

- **The spool is bounded.** `multipart_spool` limits the bytes spooled at
  one time to `max_bytes`, 8 000 000 000 by default, and a part past it gets
  a `503`. `max_files` is `infinity` by default.

- **A stalled upload gets a `408`.** `nova_multipart_plugin` has the new
  option `chunk_timeout`, 15 000 ms by default: the next 64 000 bytes of the
  body, or its end, have to arrive within it. `read_timeout` still bounds
  the whole body. A `408` over HTTP/1 carries `connection: close`.

### Signing in and out

- **Sign a user in with `nova_session:sign_in/3`.** It is new. It moves the
  client to a new session id, stores who signed in under it and returns the
  request that carries the new id. `nova_session:renew/1` is the lower-level
  piece, for another change of what a client may do, and
  `nova_session:delete/1` signs out. With `secret_key_base` set, a session id
  is kept by the first write, so a sign-in that only calls `set/3` signs in
  whoever holds the id, also somebody who planted it.

- **With `secret_key_base` set, session ids are signed.** An id is 16 random
  bytes and 16 bytes of their HMAC-SHA-256, in base64, and
  `nova_session:generate_session_id/0` returns that form. A visitor who was
  handed an id with a page, by `nova_csrf_plugin` for instance, keeps it when
  the session is first written. Without a secret that write moves the client
  to a new id.

- **A signed session id is exactly what Nova handed out.** The same bytes
  written another way, with whitespace in the cookie value or other spare
  bits in the last character, are not the id and are replaced. A token of
  `nova_csrf_plugin` is held to the same rule.

- **A session that was deleted is not brought back.** `nova_session_ets`
  remembers the id of a deleted session for five minutes and stores nothing
  under it in that time: `nova_session_ets:set_value/3` returns
  `{error, session_ended}`, and a request that was on its way when its client
  signed out writes to a session of its own. A session backend of your own
  can do the same with the new optional callback `session_ended/1`. Code that
  calls `nova_session_ets:delete_value/1` and then writes to the same id
  directly gets the error.
