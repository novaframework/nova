# Upgrading

This page says what to check when moving to a newer release of Nova. It
covers the releases that changed how existing applications behave. The full
list of changes per release is in [Deprecations](deprecations.md), and the
notes of each release are on the
[releases page](https://github.com/novaframework/nova/releases).

## To the next release

The release that follows 0.18.3 tightens several defaults. Go through the
tables with your application in mind. The entries under
[Next release](deprecations.md#next-release) have the details.

### Start-up

| Before | Now | What to change |
|---|---|---|
| `security => true` in a route group was logged and the group was left open | Any `security`, `secure` or `override_secure` that is not `false` or a `fun/1` stops the start with `{invalid_security, Value}` | Give the group a callback, or `false` |
| A method that was not one of the lowercase atoms was treated as `'_'` | An atom that is not a method, or a binary or string that is not a method token, stops the start with `{invalid_method, Method}` | Fix the method. Declare `'_'` for a route that is meant to answer every method |
| A route declared with `<<"GET">>`, `"GET"` or `'GET'` answered every method | It answers that method only | Nothing, unless clients relied on the other methods |
| A listener that could not be started was logged and Nova kept running | The start fails with `{listener_not_started, Reason}` | Free the port, or fix the certificate |
| An unset `environment` was read as `dev` without notice | It is still read as `dev` by routers, and Nova logs a warning at boot | Set `{environment, prod}` in the release configuration |
| An application without a router module crashed the start with `undef` | The start fails with `{router_module_not_found, App}` and an error in the log | Add the `<app>_router` module, or set `router_module` |
| The router of an Elixir application named `:my_app` was looked up as `my_app.Router` | `MyApp.Router` is found as well | Nothing. A `router_module` set for that reason can go |
| A `secure` key in a route group, and `security` or `secure` in the options of a route, were skipped without notice | They are still not read, and each logs a warning at boot | Declare the callback as `security` on the route group |
| The entry an application wrote for itself in its own `nova_apps` was skipped | The application that starts a listener is compiled with the `prefix`, `secure`, `override_secure` and `plugin_strategy` of that entry | Remove the entry, or the options that are not meant to apply |

### Routing and security

| Before | Now | What to change |
|---|---|---|
| In `nova_apps`, `secure` replaced the `security` of every route group of the included application | Both callbacks run, `secure` first, and both have to allow a request | Use `override_secure` where a callback has to be replaced |
| `secure => false` in `nova_apps` removed the callbacks of the included application | The groups keep their callbacks | Use `override_secure` with a callback that returns `true` to open a group |
| `override_secure` did not reach the applications an included application includes | It covers them | Check that those routes are still reachable for the clients that need them |
| An included application won a clash over a route or a status page | Your own route wins, and a warning names the route that is ignored | Move or remove one of the two |
| A request path was matched as sent | Empty segments are dropped and `.` and `..` are resolved before matching | Use `nova_router:canonical_path/1` in callbacks and plugins that decide by path |
| CSRF `excluded_paths` matched a byte prefix of the path | They match whole segments | List every path that is to be excluded: `<<"/webhook">>` no longer covers `/webhooks/stripe` |
| A route without `extra_state` had `undefined` in the request, whatever its route group declared | It has the `extra_state` of its route group, and `undefined` when the group declares none | Check controllers that match on `extra_state` in a group that declares one |
| The `AuthData` of a websocket route was only in the controller data | It is also in the upgrade request, under `auth_data` | Nothing |

### Sessions

| Before | Now | What to change |
|---|---|---|
| Every response to a client without a session cookie set one | The cookie is issued with the first response of a request that writes to the session | Nothing. Do not read `nova_session_id` from the request |
| Sessions lived until they were deleted | They end after `session_max_age`, 14 days by default | Set `session_max_age` if 14 days does not fit |
| The cookie was `Secure` only for requests over HTTPS | It is `Secure` unless `environment` is set to `dev` | Set `{environment, dev}` for local development over HTTP. For a plain HTTP staging environment set `#{secure => false}` in `session_cookie_opts` |
| A session id the backend did not know was used | It is replaced with a new one | Use a shared session store when running several nodes without sticky sessions |
| The number of sessions was not limited | `nova_session_ets` holds at most `max_sessions`, 1 000 000 by default | Handle `{error, session_store_full}` from `nova_session:set/3`, or raise the limit |

### Requests and responses

| Before | Now | What to change |
|---|---|---|
| `nova_request_plugin` read a body of any size | It reads at most `max_body_length`, 1 MB by default, within `body_read_timeout`, 60 seconds | Raise the limits on the route groups that take larger bodies |
| `decode_json_body` decoded types that start with `application/json` | It decodes `application/json` and `application/*+json` | Decode other types, such as `application/json-seq`, in the controller |
| The `500` page showed the reason and the stacktrace | They are shown only when `environment` is set to `dev` | Read the log for the details |
| Nova's `404` and `500` pages compared the `accept` header with `application/json` and `text/html` as text, and the `500` page had no body unless the header was exactly one of the two | Both pages negotiate on `accept`, quality values included: JSON without the header and for `*/*`, HTML for a browser, no body only when the header admits neither | Nothing, unless a client relied on the empty body |
| Outside `dev`, a crash in a plugin was answered with a bare `500` | Nova's `500` page has its generic body in every environment, without the class, the reason or the stacktrace | Nothing |
| A handler or a controller that streamed its response and returned the request was answered a second time, which was logged as a crash | Nothing more is sent. A failure after the response has begun is logged and gets no error page | Return the request `cowboy_req:stream_reply/3` gave back |
| A controller that answered with `cowboy_req:reply/4` and returned without the request that call gave back was answered a second time | The client gets the one response. What the controller returned is not sent, and a warning names the controller | Return the request `cowboy_req:reply/4` gave back |
| The `headers` option of a view replaced the default `content-type: text/html` | The headers are added to it, and one that names a `content-type` replaces it | Nothing |
| A `url` tag without `application` crashed the view | It renders `#` and logs a warning | Give the tag an `application` |
| `nova_cors_plugin` sent `access-control-allow-origin` to every request | It is sent only to a request whose `Origin` is allowed | List every origin that needs access in `allow_origins` |
| `nova_cors_plugin` answered every `OPTIONS` request | It answers preflights only. Other `OPTIONS` requests reach the application | Add a route for `OPTIONS` where the application answers it |
| A static directory served files through symbolic links that leave it | Such a file is a `404` | Move the file, or set `follow_external_symlinks => true` on the route |
| A static route to a single file given as a relative path followed the working directory of the node | It serves the file the path named when the routes were compiled | Nothing, unless the application changes the working directory |
| The index of a static directory that was removed while Nova runs got a `500` | Every request under the directory gets a `404`, and the first is logged | Restore the directory |
| A static route to a single file whose file was gone got a `500` | It gets a `404`, logged once per file | Restore the file |
| Uploaded files stayed in the directory of `nova_multipart_file_handler` | They are deleted when the request is over | Call `nova_multipart_spool:keep/1` for the files to keep |

### Plugins and controllers

| Before | Now | What to change |
|---|---|---|
| `{stop, Req}` from a plugin | Deprecated, logs a warning | Return `{stop, Req, State}` |
| `{break, ...}` from a plugin | Deprecated alias of `stop`, logs a warning | Return `{stop, ...}` |
| `{ok, {redirect, Url}, Req, State}` and `{ok, {status, Code}, Req, State}` from a plugin were overwritten by the controller | They are answered with the `500` page | Return `{stop, Reply, Req, State}` |
| `{json, StatusCode, Headers, Req, JSON}` from a controller | Deprecated, logs a warning | Return `{json, StatusCode, Headers, JSON, Req}` |
| `plugin_strategy` on a route group | Ignored, logs a warning | Set it in the `nova_apps` entry of the application |
| `nova:use_stacktrace(false)` returned `true` or `false` | It returns `ok` | Do not match on the old value |

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
