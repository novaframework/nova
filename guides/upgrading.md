# Upgrading

This page says what to check when moving to a newer release of Nova. It
covers the releases that changed how existing applications behave. The full
list of changes per release is in [Deprecations](deprecations.md), and the
notes of each release are on the
[releases page](https://github.com/novaframework/nova/releases).

## To the next release

The release that follows 0.18.3 is 1.0. It takes Cowboy out of Nova and
tightens several defaults. Go through the
tables with your application in mind. The entries under
[Next release](deprecations.md#next-release) have the details.

### The server and its listeners

Nova 1.0 has no HTTP server of its own. Cowboy moved to the `nova_cowboy`
package, and `nova_roadrunner` runs Nova on Roadrunner. See
[Servers and listeners](listeners.md).

| Before | Now | What to change |
|---|---|---|
| Nova depended on Cowboy and started its listener | A server package starts the listeners. Without one Nova starts, logs a warning and serves nothing | Add `nova_cowboy` to the dependencies and to the `applications` of your application, next to `nova` |
| The listener was configured under `cowboy_configuration` | Listeners are configured by name under `listeners`. `cowboy_configuration` is still read when `listeners` is not set, as one listener named `default`, and a notice says so at boot | Write the listener out, see [the table of keys](listeners.md#cowboy_configuration) |
| A node had one listener, and more only through `nova_sup:add_application/2` | A node has as many as `listeners` names, each with its `applications` | Nothing |
| A listener without TLS served HTTP/1.1 and HTTP/2 | A listener serves its `protocols`: `[http1]` without `tls` and `[http1, http2]` with it, when they are not set. One read from `cowboy_configuration` serves both, as before. A node with neither key served both on port 8080, and now has the defaults of a listener: `[http1]` | List `http2` in the `protocols` of a listener without TLS that clients speak HTTP/2 to |
| A listener that could not start failed the `nova` application with `{listener_not_started, Reason}` | The application of the server package fails, with the reason in `{shutdown, {failed_to_start_child, nova_listener, Reason}}`. A listener that asks for what the server cannot do, and a route that no listener could serve, fail it as well, see [what is checked at boot](listeners.md#what-is-checked-at-boot) | Code that matched the error of `application:ensure_all_started/1` |
| The request was Cowboy's request map, read with `cowboy_req` | It is a map with the keys `nova_req` documents, read and answered with `nova_req`. On `nova_cowboy` it is still Cowboy's map with Nova's keys added, so `cowboy_req` calls keep working there | Call `nova_req` in code that is to run on any server. `cowboy_req:match_qs/2` and `cowboy_req:cast/2` have no counterpart |
| A response sent with `cowboy_req:reply/4` carried the session cookie | The session cookie goes out with the response Nova sends and with one sent through `nova_req` | Answer with `nova_req:reply/4` where a session may have started |
| Typespecs said `cowboy_req:req()` | They say `nova_req:req()` | Specs of your own that dialyzer now reports |
| `nova_sup:add_application/2` compiled the routes of the application in the calling process | They are compiled in a process of Nova's that does not outlive the call. The process dictionary, the logger metadata and the `self()` of the caller are not there, and what `routes/1` links to or creates, an ETS table for one, goes with that process. The applications of the configuration are compiled in the process that has the listeners, as before | Create what has to last in a process of your own, and read what the router needs from the environment of the application |
| A router could call `nova_sup:add_application/2` or `remove_application/1` as its routes were compiled | It is answered `{error, adding_an_application}` | Add the other application from `start/2`, or list it in `nova_apps` |
| `nova_sup:add_application/2` before the listener was up had nothing to add to | The application is added once the listeners of the configuration are up, and the answer is where it will be served. The server package does not start when it cannot be: `{application_not_added, App, Reason}` | Nothing. A library can call it from its `start/2` |
| An application that was added was gone when the listeners were | Nova adds it again when the server package is started again, to the listener it was on. `nova_sup:remove_application/1` is what forgets it. A listener that was started for an application and whose address another has by then stops the start with `{address_in_use, Other}` as the reason | Remove an application that is not to come back |
| An application added to a port Nova already listened on was added whatever else its settings asked for, `use_ssl` or `stream_handlers` say, and those were dropped | It is refused with `{error, {listener_settings_ignored, Name, Keys}}`, and so is `port => 0`, with `{invalid_listener_setting, App, port, 0}` | Add it with the address alone or with `#{listener => Name}`, or give it a port of its own |
| Adding an application to the listener the configuration lists it on answered `{error, {already_started, App}}` | The answer is `{ok, App, Host, Port}`, where it is served. `already_started` is the answer to adding an application a second time | Code that matched the error for an application of the configuration |
| `nova_sup:listeners/0` returned `ranch` references, and the listener of the bootstrap application was `nova_listener` | It returns the names of the listeners. On Cowboy the `ranch` reference of a listener is `nova_cowboy:ranch_ref(Name)` | Code that calls `ranch` with the reference |
| The routing table of a listener started at runtime was under `{nova_dispatch, App, Port}` | It is under `{nova_dispatch, Name}`. `nova_router:dispatch_keys/0` lists them | Code that builds the key itself |
| `nova_stream_h` was a module of `nova` | It is a module of `nova_cowboy`, and `stream_handlers` goes in the `server_opts` of a listener | Nothing, with `nova_cowboy` in the release |
| `middleware_handlers` listed Cowboy middlewares | `steps` of a listener lists them. They implement `nova_pipeline` and are called as before, with `execute/2` | Rename the key when you write the listener out |
| When `nova_sup` went down without `nova` being stopped, the listener stayed bound and a new `nova_sup` took it over | The application of the server package stops with `nova_sup`, and its listeners with it | Start that application again to serve again |
| The value of a route answered by a module, such as a WebSocket route, was the record `#cowboy_handler_value{}` of `nova_router.hrl`, and the module was under `cowboy_handler` in the environment of a request | The record is `#nova_module_value{}`, and the key is `handler_module` | Code that builds the record or reads the key, such as a plugin that tells a WebSocket route from another |
| `#nova_handler_value{}` had seven fields and the record of a module route five | Both have `http_versions` as their last field, the HTTP versions the route is served over | Compile against the `nova_router.hrl` of this release. A tool with a copy of the records of its own, as `rebar3_nova` before its release for Nova 1.0, does not recognise a route |
| Every route was served on every listener of its application, over every HTTP version | The same, unless a route group names its `listener` or a route its `http_versions`, see [Routing](routing.md#the-listeners-of-a-route-group) | Nothing |

### Defaults that changed

Each of these turns something on, or refuses something that was let through.
The last column says how to get the earlier behaviour back.

| What changes | Who is affected | To keep the earlier behaviour |
|---|---|---|
| An unset `environment` is read as `prod`. It was read as `dev`. Nova logs one notice at boot | Applications that do not set it. Routers are called with `routes(prod)`, `nova:get_environment/0` returns `prod` and the output of watchers is not printed | Set `{nova, [{environment, dev}]}` in the development configuration |
| A path with an empty segment ahead of its last one, or with a `.` or `..` segment, is answered with `400` before routing. It was resolved and routed. See [Routing](routing.md#paths-that-are-not-canonical) | Clients and proxies that send `//` or dot segments, such as a base URL that ends in `/` joined with a path that starts with one | `{nova, [{non_canonical_paths, resolve}]}` |
| `pre_request/4` of a plugin runs once the security callback has let the request through. It ran ahead of it. What has to run first goes in the new callback `pre_security/4`. See [Plugins](plugins.md#before-and-after-the-security-callback) | Security callbacks that read `json`, `params`, `body` or `files`. Plugins of your own that have to see every request, such as a rate limit, an access log or one that answers CORS preflights: they no longer run for a request the callback refuses. Nova logs a warning for both when routes are compiled, see [Routing and security](#routing-and-security) | List the plugin under `plugins_before_security` in the route group, or set `{nova, [{plugins_before_security, all}]}`. For a plugin of your own, export `pre_security/4` |
| `nova_request_plugin` parses the query string ahead of the security callback and reads the body behind it | The same callbacks. `parsed_qs` is in the request they get, `json`, `params` and `body` are not | As above |
| `nova_cors_plugin` and `nova_correlation_plugin` work in `pre_security/4` and have no `pre_request/4` | Code and tests that call `pre_request/4` of these modules | Call `pre_security/4`. In routes nothing changes: they are still listed as `pre_request` and still run first |
| Nova checks the `Origin` of every request to a websocket route, against `trusted_origins`, `same_host` by default. Nothing was checked. See [Websockets](websockets.md#the-origin-check) | Pages on another host than the websocket they open. Sites behind a proxy that rewrites the `Host` header. Clients that are not browsers and send an `Origin` | List the origins in `trusted_origins`, set it to `any`, which opens websockets alone, or list `nova_ws_origin_plugin` with `trusted_origins => any` in the route group |
| With `same_host`, a request that came in over TLS needs an `https` origin. On a listener without TLS the scheme is not compared, and no forwarded header is read | Clients that send an `Origin` of another scheme to a listener with TLS, such as `http://` or `wss://`, for a websocket route or a request `nova_csrf_plugin` checks | List the origin in `trusted_origins` |
| A websocket upgrade whose `Sec-Fetch-Site` header is neither `same-origin` nor `none` is answered with `403`, unless its `Origin` is in a list of `trusted_origins` or the origins are `any`. Not every browser sends the header with an upgrade | Pages the browser counts as another site than the websocket they open, such as a page loaded over `http` for a site behind a proxy that terminates TLS | List the origin of the page in `trusted_origins` |
| `nova_csrf_plugin` refuses a request that changes state when the browser says a page of another site sent it, whatever token it has. See [Plugins](plugins.md#csrf-trusted-origins) | Pages on another origin, a sibling subdomain included, that post with a valid token | Give the plugin of the route group `trusted_origins` with those origins and the one of the site, or `trusted_origins => any`, which turns the check off for the group. `{trusted_origins, any}` in the `nova` environment does not turn it off |
| `csrf_token` in the request is another value for every response. With `secret_key_base` set it is signed and nothing is stored. See [Plugins](plugins.md#csrf-token-modes) | Code and tests that compare it with the value in the session, or expect the same token twice | Send the token back as it was handed out. A token that an earlier release handed out is still accepted |
| `nova_csrf_plugin` logs a notice at boot when neither `secret_key_base` nor `csrf_token` is set | Every application that uses the plugin | Set `secret_key_base`, or `{csrf_token, session}` |
| The session cookie is named `__Host-session_id` when `session_cookie_name` is not set. It was `session_id`, and still is when `environment` is `dev`. See [Sessions](sessions.md#the-__host-cookie-name) | Every application that does not name its cookie. The `session_id` cookie of before is not read, so every client is signed out once. A browser keeps the new cookie only over HTTPS | Set `{session_cookie_name, <<"session_id">>}` |
| `non_canonical_paths`, `trusted_origins`, `csrf_token`, `secret_key_base`, `max_sessions` and `plugins_before_security` are checked when Nova starts, and read from the `nova` environment only. See [Configuration](configuration.md#settings-checked-at-boot) | Configurations with a value that cannot be used, such as `{max_sessions, 0}`: the start fails with `{invalid_setting, Key, Reason}`. Configurations that set one of them under the bootstrap application: it is not read | Fix the value, and set the key under `nova` |
| `shutdown_delay` and `shutdown_drain_timeout` are checked when Nova starts: an integer of milliseconds from `0` to `4294967295` | Configurations with another value, such as `infinity` or a string: the start fails with `{invalid_setting, Key, Value}` | Set a number of milliseconds |
| `json_lib` defaults to `json` of Erlang/OTP. It was `thoas`, which Nova no longer depends on | Applications that call `thoas` without listing it in their own `deps`. Code that relies on how `thoas` encodes or decodes | Add `thoas` to the `deps` of the application and set `{json_lib, thoas}`, see [Configuration](configuration.md#configure-json_lib) |
| Plugin telemetry has `callback => pre_security` for `pre_security/4`. The controller span of a `400`, `404` or `405` from the router, and of the `500` after a crash in a plugin or a security callback, has `status_page => true`. See [Telemetry](telemetry.md) | Handlers that match `callback => pre_request` for `nova_cors_plugin` or `nova_correlation_plugin`. Metrics that count requests by `status_page => false` | Nothing restores it. Adjust the handlers |
| With the default `json_lib`, a JSON body with an object that has a key twice, or with anything but whitespace after its value, is answered with `400`. `thoas` kept the last value of the key. See [Plugins](plugins.md#nova-request) | Clients that send such a body. Code that relied on the last value of a repeated key | Fix the client, or set a `json_lib` of your own, which decides both itself |

### Start-up

| Before | Now | What to change |
|---|---|---|
| `security => true` in a route group was logged and the group was left open | Any `security`, `secure` or `override_secure` that is not `false` or a `fun/1` stops the start with `{invalid_security, Value}` | Give the group a callback, or `false` |
| A method that was not one of the lowercase atoms was treated as `'_'` | An atom that is not a method, or a binary or string that is not a method token, stops the start with `{invalid_method, Method}` | Fix the method. Declare `'_'` for a route that is meant to answer every method |
| A route declared with `<<"GET">>`, `"GET"` or `'GET'` answered every method | It answers that method only | Nothing, unless clients relied on the other methods |
| A listener that could not be started was logged and Nova kept running | The start fails with `{listener_not_started, Reason}` | Free the port, or fix the certificate |
| An application without a router module crashed the start with `undef` | The start fails with `{router_module_not_found, App}` and an error in the log | Add the `<app>_router` module, or set `router_module` |
| The router of an Elixir application named `:my_app` was looked up as `my_app.Router` | `MyApp.Router` is found as well | Nothing. A `router_module` set for that reason can go |
| A `secure` key in a route group, `security` or `secure` in the options of a route, and `security` in a `nova_apps` entry were skipped without notice | They are still not read. With any value but `false` the start fails with `{misplaced_security, App, Key, Path}`, and an error names each key. With `false` each logs a warning | Declare the callback as `security` on the route group, or as `secure` on the `nova_apps` entry |
| `nova_router:add_routes/2,3` compiled routes with such a key | They throw `{error, {misplaced_security, App, Key, Path}}` | Fix the routes, or catch the throw where routes come from elsewhere |
| The entry an application wrote for itself in its own `nova_apps` was skipped | The application that starts a listener is compiled with the `prefix`, `secure`, `override_secure` and `plugin_strategy` of that entry, and a notice names the keys. Its `secure` and `override_secure` also cover the applications it mounts | Remove the entry, or the options that are not meant to apply |
| An application that two `nova_apps` entries mount was compiled for the first one | The same when both ask for the same `secure` and `override_secure`. Otherwise the start fails with `{nova_app_mounted_twice, App}` | Mount the application once, or give both entries the same callbacks |
| A plugin first used by routes added at runtime was never initialised: it was called with `undefined` as its state | `init/0` runs when the router compiles the first route that uses the plugin, also at runtime, and the caller waits for it | Do not call into `nova_sup` from `init/0` |
| Watcher arguments that are not strings stopped `nova_watcher` | The watcher is skipped and `Could not start watcher` is logged | Write the arguments as strings |
| `nova_csrf_plugin` could be listed with `{use_sessions, false}`, and every request to its routes failed | A route group that lists it there stops the start with `{invalid_setting, use_sessions, {required_by, nova_csrf_plugin}}`, and `{csrf_token, stateless}` there with `{invalid_setting, csrf_token, {requires, use_sessions}}` | Applications that turned sessions off: take the plugin off the routes, or turn sessions on |
| There was no secret to set | `secret_key_base` is checked when Nova starts. One that is shorter than 32 bytes, has fewer than 8 different bytes, is an example out of the documentation or is a `${VARIABLE}` that was not filled in stops the start with `{invalid_setting, secret_key_base, redacted}`. See [Configuration](configuration.md#the-secret-of-the-node) | Applications that set it: make the secret with `base64:encode(crypto:strong_rand_bytes(48)).` or `openssl rand -base64 48`, and check that the release fills the variable in |
| Builds between 0.18.3 and this release read `allowed_origins`, `csrf_secret` and `plugins_after_security`. No release did | A node or a route group that still has one of them does not start, with `{invalid_setting, Key, {replaced_by, Setting}}` | Applications configured for such a build: rename `allowed_origins` to `trusted_origins` and `csrf_secret` to `secret_key_base`, and take `plugins_after_security` out |
| `session_cookie_opts` with a `domain`, a `path` other than `/` or `secure => false` was used as it was | Outside `dev`, with no `session_cookie_name`, it stops the start with `{invalid_setting, session_cookie_opts, {requires, session_cookie_name}}`: the default name has the `__Host-` prefix, which rules those out | Set `session_cookie_name`, to `<<"session_id">>` for the cookie as it was, or take the option out |
| Nothing was logged about a `Secure` session cookie on a listener without TLS | Outside `dev` such a node logs one notice at boot, with `notice => session_cookie_needs_https` | Nothing behind a proxy that terminates TLS. Where browsers reach the node over plain HTTP, see [Sessions](sessions.md#the-session-cookie) |

### Routing and security

| Before | Now | What to change |
|---|---|---|
| In `nova_apps`, `secure` replaced the `security` of every route group of the included application | Both callbacks run, `secure` first, and both have to allow a request | Use `override_secure` where a callback has to be replaced |
| `secure` did not reach the applications an included application includes, and the `security` of a route group did not reach the applications under its `apps` key | Both cover them, outermost first, in front of the callbacks those applications declare | Check that those routes are still reachable for the clients that need them |
| `secure => false` in `nova_apps` removed the callbacks of the included application | The groups keep their callbacks | Use `override_secure` with a callback that returns `true` to open a group |
| `override_secure` did not reach the applications an included application includes | It covers them | Check that those routes are still reachable for the clients that need them |
| An included application won a clash over a route or a status page | Your own route wins, and a warning names the route that is ignored | Move or remove one of the two |
| CSRF `excluded_paths` matched a byte prefix of the path | They match whole segments | List every path that is to be excluded: `<<"/webhook">>` no longer covers `/webhooks/stripe` |
| A route without `extra_state` had `undefined` in the request, whatever its route group declared | It has the `extra_state` of its route group, and `undefined` when the group declares none | Check controllers that match on `extra_state` in a group that declares one |
| The `AuthData` of a websocket route was only in the controller data | It is also in the upgrade request, under `auth_data` | Nothing |
| `auth_data` in the controller data of a websocket controller was what the security callback returned in `{true, AuthData}` | It is the `auth_data` of the upgrade request. On a route whose callback returns `true`, or that has none, that is what a plugin wrote there | Websocket controllers on routes with a plugin that writes `auth_data`: check what they match on |
| A security callback ran after `nova_request_plugin` had decoded the body, and nothing was logged about it | A route group with a security callback and a body that is decoded behind it logs a warning when its routes are compiled, with `notice => body_behind_security`, until 1.0. See [Plugins](plugins.md#saying-whether-the-security-callback-reads-the-body) | Route groups with `security` and `decode_json_body` or `read_urlencoded_body`: set `security_reads_body => false` when the callback does not read `json` or `params`, and list `nova_request_plugin` under `plugins_before_security` when it does. `false` answers for the `security` of the group only, not for a `secure` or a route group the application is mounted behind |
| The same, for a route group that reads a form for `nova_csrf_plugin` | A route group with a security callback that lists `nova_csrf_plugin` and `nova_request_plugin` with `read_urlencoded_body` and no `decode_json_body` logs a notice in place of the warning, with `notice => form_behind_security` | Route groups set up as the generators and these guides set them up: set `security_reads_body => false`. When the callback reads `params`, list `nova_request_plugin` under `plugins_before_security` |
| `pre_request/4` of a plugin of your own ran for every request to its routes | A plugin from outside Nova that has `pre_request/4` and no `pre_security/4` is named in a warning when routes are compiled: with `notice => pre_request_behind_security` in a route group with a security callback, and with `pre_request_not_for_preflight` in a group with a route that does not take `OPTIONS`. See [Plugins](plugins.md#warnings-about-the-order) | Authors of plugins: export `pre_security/4` for what has to run for every request. Listing the plugin under `plugins_before_security` ends the first warning and not the second |
| Security callbacks and plugins could only redirect with `{redirect, Url}`, to any site | They also take `{redirect_local, Path}`, which answers `400` for anything but a path on the same site | Return `{redirect_local, Path}` where the target comes from the request |
| `nova:get_apps/0` listed the applications of the default listener | It lists those of every listener, the default listener's first | Nothing. `nova_router:compiled_apps/1` and the new `nova_router:dispatch_keys/0` read one listener |
| Nova wrote a key `apps` to the environment of the bootstrap application | It no longer does | Code that read the key calls `nova:get_apps/0` |

### Sessions

| Before | Now | What to change |
|---|---|---|
| Every response to a client without a session cookie set one | The cookie is issued with the first response of a request that writes to the session | Nothing. Do not read `nova_session_id` from the request |
| Sessions lived until they were deleted | They end after `session_max_age`, 14 days by default | Set `session_max_age` if 14 days does not fit |
| The cookie was `Secure` only for requests over HTTPS | It is `Secure` unless `environment` is set to `dev` | Set `{environment, dev}` for local development over HTTP. For a plain HTTP staging environment set `#{secure => false}` in `session_cookie_opts` and a `session_cookie_name` without the `__Host-` prefix |
| A session id the backend did not know was used | It is replaced with a new one | Use a shared session store when running several nodes without sticky sessions |
| The number of sessions was not limited | `nova_session_ets` holds at most `max_sessions` on a node, 1 000 000 by default, an integer above 0 or `infinity`. In a full table a new session takes the place of an expired one, then of a provisional one, then of the session nearest its expiry | Raise the limit if it does not fit |
| There was one way to write to a session | `nova_session:set_provisional/3` writes for a visitor who is not signed in. Such a session lives one day and is dropped first. It returns `{error, session_store_full}` when the table holds nothing but sessions in use | Use it for carts, flash messages and the like, and `set/3` once a client has signed in. See [Sessions](sessions.md#data-for-visitors-who-are-not-signed-in) |
| `nova_csrf_plugin` wrote its token with `set/3` | Without a `secret_key_base` it writes it with `set_provisional/3`. With `nova_session_ets`, a visitor who only has the token has a session that ends after one day | Nothing, unless a form is left open for more than a day before it is posted: that post gets a `403` |
| A session backend had four callbacks | `set_provisional_value/3` is a new optional one. Without it the backend gets those writes through `set_value/3` | Implement it in a backend that limits its size |
| `session_exists/1` of the backend was called on every access by a request with a cookie | For reads the answer is kept for one second by the calling process. Every write asks again | Nothing |
| The typespecs of `nova_session` and its backend callbacks said a value is a `binary()` | They say `term()`, which is what was always passed through | A backend that stores binaries encodes the value, for instance with `term_to_binary/1` |
| Who signed in was written to the session with `set/3` | `nova_session:sign_in/3` moves the client to a new session id, stores the value under it and returns the request to go on with. `nova_session:renew/1` changes the id alone. See [Sessions](sessions.md#nova_session-sign_in-3) | Every application that signs users in: call `sign_in/3` where a user signs in and `nova_session:delete/1` where one signs out. Answer a sign-in with a redirect: the `csrf_token` of that request was made for the old id |
| A session id was 32 random bytes | With `secret_key_base` set it is 16 random bytes and 16 bytes of their HMAC-SHA-256, 44 characters as before, and `nova_session:generate_session_id/0` returns that form. A visitor who was handed an id with a page keeps it when the session is first written | Applications that set `secret_key_base` and sign in with `set/3` alone: the id stays the one the visitor had, which somebody else may know. Call `sign_in/3` |
| A session id was used as the client sent it | A signed id is exactly the 44 characters Nova handed out. The same bytes written another way, with whitespace in the cookie value or other spare bits in the last character, are replaced with a new id | Clients and proxies that rewrite cookie values: send the cookie back as it was set |
| A request that was on its way when its client signed out could write the session back | A session that was deleted stays deleted. `nova_session_ets` remembers a deleted id for five minutes and stores nothing under it: `nova_session_ets:set_value/3` returns `{error, session_ended}`, and `nova_session` stores what such a request writes in a session of its own | Code that calls `nova_session_ets:delete_value/1` and then writes to the same id directly: write under a new id. Implement the new optional callback `session_ended/1` in a backend of your own |

### Requests and responses

| Before | Now | What to change |
|---|---|---|
| `nova_request_plugin` read a body of any size | It reads at most `max_body_length`, 1 MB by default, within `body_read_timeout`, 60 seconds | Raise the limits on the route groups that take larger bodies |
| `decode_json_body` decoded types that start with `application/json` | It decodes `application/json` and `application/*+json` | Decode other types, such as `application/json-seq`, in the controller |
| The warning for a JSON body that did not decode was `Failed to decode json.`, with the reason of the library | It is `Request refused`, with `reason`, `status_code`, the kind of error under `error` and, from OTP's `json`, where it is under `position`. Nothing of the body is logged | Adjust log queries and alerts that match `Failed to decode json.` |
| The `500` page showed the reason and the stacktrace | They are shown only when `environment` is set to `dev` | Read the log for the details |
| A view whose template does not exist was answered with `404` | It is answered with the `500` page, and in `dev` the page names the template | Add the template. Clients that took the `404` for a missing resource now see a `500` |
| With `{render_error_pages, false}`, a controller that crashed or threw a status got the bare status code without a page being looked up, and Nova's `404` page was still sent | The status code routes of the application run either way. Nova's own `404` and `500` pages send the bare status code to every client | Remove status code routes that are not meant to answer. Add them for the pages that are to have a body |
| Nova's `404` and `500` pages compared the `accept` header with `application/json` and `text/html` as text, and the `500` page had no body unless the header was exactly one of the two | Both pages negotiate on `accept`, quality values included: JSON without the header and for `*/*`, HTML for a browser, no body only when the header admits neither | Nothing, unless a client relied on the empty body |
| Outside `dev`, a crash in a plugin was answered with a bare `500` | Nova's `500` page has its generic body in every environment, without the class, the reason or the stacktrace | Nothing |
| A handler or a controller that streamed its response and returned the request was answered a second time, which was logged as a crash | Nothing more is sent. A failure after the response has begun is logged and gets no error page | Return the request `cowboy_req:stream_reply/3` gave back |
| A controller, plugin or security callback that answered with `cowboy_req:reply/4` and did not hand back the request that call returned was answered a second time | The client gets the first response and the connection stays open. The second response is dropped with one warning per request, which has the status and the method | Hand back the request `cowboy_req:reply/4` returned |
| The `headers` option of a view replaced the default `content-type: text/html` | The headers are added to it, and one that names a `content-type` replaces it | Nothing |
| A `url` tag without `application` crashed the view | It renders `#` and logs a warning | Give the tag an `application` |
| `nova_cors_plugin` sent `access-control-allow-origin` to every request | It is sent only to a request whose `Origin` is allowed | List every origin that needs access in `allow_origins` |
| A CORS preflight to a route that does not take `OPTIONS` got a `405`, and no plugin ran | `pre_security/4` of the plugins of the route for the method it asks about runs before the `405`, so `nova_cors_plugin` answers it | Nothing |
| `nova_cors_plugin` answered every `OPTIONS` request | It answers preflights only. Other `OPTIONS` requests reach the application | Add a route for `OPTIONS` where the application answers it |
| A static directory served files through symbolic links that leave it | Such a file is a `404` | Move the file, or set `follow_external_symlinks => true` on the route |
| A static route to a single file given as a relative path followed the working directory of the node | It serves the file the path named when the routes were compiled | Nothing, unless the application changes the working directory |
| The index of a static directory that was removed while Nova runs got a `500` | Every request under the directory gets a `404`, and the first is logged | Restore the directory |
| A static route to a single file whose file was gone got a `500` | It gets a `404`, logged once per file | Restore the file |
| Uploaded files stayed in the directory of `nova_multipart_file_handler` | They are deleted when the request is over | Call `nova_multipart_spool:keep/1` for the files to keep |
| The bytes spooled by uploads in progress were not limited | At most `max_bytes` of `multipart_spool`, 8 GB by default, are spooled at one time. A part past it gets a `503`. `max_files` is `infinity` by default | Raise `max_bytes` for larger uploads, and treat `503` as try again later in clients |
| A multipart upload that stalled held its connection until `read_timeout` or longer | An upload that sends less than 64 000 bytes within `chunk_timeout`, 15 seconds by default, gets a `408`, and so does one that is not done within `read_timeout`. Over HTTP/1 the `408` closes the connection | Raise `chunk_timeout` on routes with clients on slow links |
| The decoded header list of an HTTP/2 request was not limited | Cowboy 2.21 resets a stream whose headers decode to more than 32 768 bytes, with `enhance_your_calm` | Set `max_header_list_size` in the `options` of `cowboy_configuration` for clients that send more |
| `nova_correlation_plugin` logged the id under the binary `<<"correlation-id">>` by default | The default key is the atom `'correlation-id'`. A binary key is used as the atom of that name when the node knows one, and otherwise replaced by the default with an error in the log | Adjust log queries and formatters that look for the binary key. Write `logger_metadata_key` as an atom |
| `nova_jsonlogger` with `redact => true` redacted reports and their metadata | It also redacts the arguments of a format string before they are put into the text. Arguments that no longer fit the format are printed as a term | Adjust log queries that match on the old text |

### Plugins and controllers

| Before | Now | What to change |
|---|---|---|
| `{stop, Req}` from a plugin | Deprecated, logs a warning | Return `{stop, Req, State}` |
| `{break, ...}` from a plugin | Deprecated alias of `stop`, logs a warning | Return `{stop, ...}` |
| `{ok, {redirect, Url}, Req, State}` and `{ok, {status, Code}, Req, State}` from a plugin were overwritten by the controller | They are answered with the `500` page | Return `{stop, Reply, Req, State}` |
| `{json, StatusCode, Headers, Req, JSON}` from a controller | Deprecated, logs a warning | Return `{json, StatusCode, Headers, JSON, Req}` |
| `plugin_strategy` on a route group | Ignored, logs a warning | Set it in the `nova_apps` entry of the application |
| `nova:use_stacktrace(false)` returned `true` or `false` | It returns `ok` | Do not match on the old value |
| `post_ws_connection` plugins ran only for a controller that exports `terminate/3`, and not when it crashed | They run whenever a connection goes away | Nothing |
| A websocket callback that returned something Nova does not know crashed the connection process | It is logged as `Websocket handler failed on what the controller returned` and the connection is closed | Return one of the documented values |
| `handle_websocket/3` of `nova_basic_handler` | Deprecated. Nova does not call it | Remove calls to it |
| A status code route found what `nova_router:render_status_page/3,5` was given only in the Cowboy environment | It is in the request, as `#{status => StatusCode, data => Data}` under `controller_data`. Every status page has it and `resp_status_code`, also one rendered for a controller that failed | Nothing |
| Thirteen error reports of the router had their text under `reason` | The text is under `msg`, as in every other report | Adjust log queries that read `reason` |
| The request key of the stream handler was `nova_stream_h` | It is `nova_stream`. The key is internal | Do not read it |

### Dependencies

| Before | Now | What to change |
|---|---|---|
| Cowboy `2.18.0` | Cowboy `~> 2.21.0`, with Cowlib 2.22 | Check the Cowboy [migration guides](https://ninenines.eu/docs/en/cowboy/2.21/guide/) for code that calls Cowboy directly |
| `thoas` was a dependency of Nova | It is not. See [Defaults that changed](#defaults-that-changed) | List `thoas` in your own `deps` if you call it |

## To 0.17.0

| Before | Now | What to change |
|---|---|---|
| The routing table was a `routing_tree` structure | It is a `nova_routing_trie`, which is opaque | Read routes with `nova_routing_trie:routes/1` instead of matching on `routing_tree` records |
| Matching stopped at the first sibling that fitted | Matching backtracks, so `/a/:x/c` matches `/a/b/c` when `/a/b/d` exists | Check for routes that were unreachable and now answer |
| `use_strict_routing` had no effect | Conflicting routes stop the start when it is `true` | Resolve the conflicts, or leave it `false` |
| Nova's `404` and `500` pages won over the ones of the application | The status code routes of the application win | Nothing |
| A route group used its own `plugins` or the global ones | The same, and `plugin_strategy` can combine them for an included application | Nothing |

## Deprecated and removed in 1.0

| Deprecated | Since | Use |
|---|---|---|
| `{Module, Function}` in routes, security and handlers | 0.9.24 | `fun Module:Function/1` |
| `ca_cert` and `cert` in `cowboy_configuration` | 0.10.3 | `ssl_options` |
| `{stop, Req}` and `{break, ...}` from a plugin | Next release | `{stop, Req, State}` and `{stop, Reply, Req, State}` |
| `{json, StatusCode, Headers, Req, JSON}` | Next release | `{json, StatusCode, Headers, JSON, Req}` |
| `handle_websocket/3` of `nova_basic_handler` | Next release | Nothing. Nova does not call it |
