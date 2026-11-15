# Security

This guide collects what a Nova application has to get right before it is
reachable by others: who may call a route, how sessions and forms are
protected, what Nova limits and escapes for you and what it leaves to you.

To report a vulnerability in Nova itself, see
[Reporting a vulnerability](#reporting-a-vulnerability).

## Defaults at a glance

What Nova does when a setting is left out:

| Setting | When it is not set | Details |
|---|---|---|
| `environment` | Read as `prod`. No crash details on error pages, the session cookie is `Secure`, routers are asked for `routes(prod)` | [Configuration](configuration.md#nova-specific-configurations) |
| `non_canonical_paths` | `reject`. A path with an empty, `.` or `..` segment is answered with `400` before routing | [Routing](routing.md#paths-that-are-not-canonical) |
| `allowed_origins` | `same_host`. A websocket route refuses a request from another host, and so does `nova_csrf_plugin` for a request that changes state | [Configuration](configuration.md#allowed-origins) |
| `plugins_before_security` | `[]`. No plugin of Nova reads the body before the security callback has let the request through | [Plugins](plugins.md#before-and-after-the-security-callback) |
| `csrf_secret`, `csrf_token` | `nova_csrf_plugin` keeps its tokens in the session | [Plugins](plugins.md#csrf-token-modes) |
| `max_sessions` | `1000000` sessions on a node | [Sessions](sessions.md#when-the-session-store-is-full) |
| `use_stacktrace` | `false`. No stacktraces in crash logs | [Configuration](configuration.md#nova-specific-configurations) |
| `security` of a route group | The routes of the group are public | [Protecting routes](#protecting-routes) |

`non_canonical_paths`, `allowed_origins`, `csrf_secret`, `csrf_token`,
`max_sessions` and `plugins_before_security` are checked when Nova starts. A
value that cannot be used stops the boot, see
[Configuration](configuration.md#settings-checked-at-boot).

## Protecting routes

A route is public unless its route group declares a security callback. The
callback is a function of the request, set with the `security` key of the
group:

```erlang
routes(_Environment) ->
    [#{prefix => "/",
       security => false,
       routes => [
           {"/login", fun my_app_session_controller:login/1, #{methods => [post]}}
       ]},
     #{prefix => "/admin",
       security => fun my_app_auth:session/1,
       routes => [
           {"/", fun my_app_admin_controller:index/1, #{methods => [get]}}
       ]}].
```

```erlang
-module(my_app_auth).
-export([session/1]).

session(Req) ->
    case nova_session:get(Req, ~"user_id") of
        {ok, UserId} -> {true, #{user_id => UserId}};
        {error, _} -> {redirect, "/login"}
    end.
```

The callback runs before the controller of every route in the group. With
`{true, AuthData}` the controller finds the data under `auth_data`:

```erlang
index(#{auth_data := #{user_id := UserId}}) ->
    {json, #{user_id => UserId}}.
```

For an API that authenticates with a token:

```erlang
bearer(Req) ->
    case cowboy_req:header(~"authorization", Req) of
        <<"Bearer ", Token/binary>> ->
            case my_app_tokens:verify(Token) of
                {ok, UserId} -> {true, #{user_id => UserId}};
                error -> {false, 403, #{}}
            end;
        _ ->
            {false, 401, #{~"www-authenticate" => ~"Bearer"}}
    end.
```

The [routing guide](routing.md#the-security-function-callback) lists every
return value. A callback that crashes answers the request with `500`.

A callback that sends the client somewhere the request names, such as a
`next` parameter, returns `{redirect_local, Path}` and not
`{redirect, Url}`. It answers `302` only to a path on the site the request
was made to, and `400` for anything else, so the sign-in page cannot be used
to send a visitor to another site:

```erlang
session(Req) ->
    case nova_session:get(Req, ~"user_id") of
        {ok, UserId} ->
            {true, #{user_id => UserId}};
        {error, _} ->
            #{next := Next} = cowboy_req:match_qs([{next, [], ~"/login"}], Req),
            {redirect_local, Next}
    end.
```

Things to know:

* **A callback in the wrong place stops the boot.** The callback of a route
  group is its `security` key, and the callback of an included application
  is the `secure` key of its `nova_apps` entry. A `secure` key in a route
  group, a `security` or `secure` key in the options of a single route, and
  a `security` key in a `nova_apps` entry are not read. With any value but
  `false` Nova logs an error for each and does not start, with
  `{misplaced_security, App, Key, Path}`. With `false` it logs a warning and
  starts. A group without `security` is public.
* **Invalid values stop the boot.** `security` takes `false` or a `fun/1`.
  Anything else, `true` included, stops the application from starting with
  `{invalid_security, Value}`.
* **Status pages are routes.** Declare the routes for status codes such as
  `404` in a group without a callback, or the page is only rendered for
  requests the callback lets through.
* **Paths with `..` and empty segments are refused.** A request for
  `/public/../admin` or `/admin//users` is answered with `400` before any
  plugin or callback runs, unless `non_canonical_paths` is `resolve`. See
  [Routing](routing.md#paths-that-are-not-canonical).
* **Compare whole path segments.** A callback or plugin that decides by path
  uses `nova_router:canonical_path/1`, not the raw `path` of the request,
  which can end in a slash the routed path does not have, and with `resolve`
  can contain `..` and empty segments.
* **Check your routes.** `rebar3 nova audit --env prod --strict` reports
  routes that change state without a callback and routes that accept every
  method. `rebar3 nova routes` prints all of them.

### What runs before the callback

Only `pre_security/4` of the `pre_request` plugins of a route runs before its
security callback. With the plugins of Nova that is the correlation id, the
CORS headers and the answer to a preflight, the origin checks of
`nova_csrf_plugin` and `nova_ws_origin_plugin`, and the parsing of the query
string. None of them reads the body. `nova_request_plugin` and
`nova_multipart_plugin` read it in `pre_request/4`, once the callback has let
the request through, so a client that is refused has nothing read, decoded
or spooled.

It follows that the callback does not find `json`, `params` or `files` in
the request. A callback that needs the decoded body lists the plugin under
`plugins_before_security`. The body of every request to that route group is
then read from clients that are not authenticated, so keep
`max_body_length` low there. See
[Plugins](plugins.md#before-and-after-the-security-callback).

### Included applications

An application that is included with `nova_apps` keeps the callbacks its
route groups declare. The application that includes it can put a callback of
its own in front with `secure`, in which case both have to allow a request,
or replace them with `override_secure`. Both also cover the applications the
included application includes in turn, and the `security` of a route group
covers the applications listed under its `apps` key. An application that two
entries mount with different callbacks stops the boot. See
[Multiple applications](multi-app.md#security-of-a-mounted-application).

## Sessions

Sessions are described in the [Sessions](sessions.md) guide. For security:

* Call `nova_session:renew/1` when a client signs in, signs out or changes
  its password. It moves the client to a new, empty session, so a session id
  that was planted or observed earlier stops working.
* The session cookie is sent with `HttpOnly`, `SameSite=Lax`, `Path=/` and,
  unless `environment` is set to `dev`, `Secure`.
* Name the cookie with the `__Host-` prefix in new applications
  (`session_cookie_name`), so another host of the same domain cannot set it.
* The session id is 256 random bits. An id the backend does not know is
  replaced, so a client cannot choose its own. A custom backend has to
  implement `session_exists/1` for that.
* Session data is kept on the server. With the default backend it is kept in
  memory, sessions end after `session_max_age` (14 days) and the number of
  sessions on a node is limited by `max_sessions`.
* Store what a visitor who is not signed in gets, such as a flash message or
  a cart, with `nova_session:set_provisional/3` and not with
  `nova_session:set/3`. The next section says why.

### When the session store is full

Every request without a session cookie that writes to the session starts a
new one, so anyone can fill the store. The default backend,
`nova_session_ets`, holds `max_sessions` sessions (1 000 000) and then drops
a session for every new one. Which one depends on how the sessions were
written:

* A session that only holds what was written with
  `nova_session:set_provisional/3` is provisional. `nova_csrf_plugin` stores
  its token that way when it keeps tokens in the session. A provisional session lives one day, is dropped first,
  and can only take the place of an expired or another provisional session.
  The first `nova_session:set/3` on it makes it a session in use.
* A session written with `nova_session:set/3` is in use. It takes the place
  of an expired session, then of a provisional one, and when there is
  neither, of the session in use that is nearest its expiry.

What requests without a cookie can still do:

* **Fail the forms of other visitors.** A flood of such requests to routes
  with `nova_csrf_plugin` drops the provisional sessions of other visitors,
  oldest first. A visitor whose session was dropped between loading a form
  and posting it gets a `403` from the plugin, on the sign-in form as on any
  other, for as long as the flood outpaces the visitors. When the store
  holds nothing but sessions in use, a new visitor gets no session and no
  token, and the post is refused the same way. Sessions in use are not
  dropped by this. With signed tokens the plugin stores nothing, and this
  does not happen.
* **Sign users out, when the application calls `nova_session:set/3` for
  clients that are not signed in.** Each of those writes starts a session in
  use, and in a full store it takes the place of the oldest session in use,
  which can be the one of a signed-in user. Use
  `nova_session:set_provisional/3` for such data, see
  [Sessions](sessions.md#data-for-visitors-who-are-not-signed-in).
* **Use memory.** Up to `max_sessions` sessions on every node. Provisional
  sessions are gone within a day.

The limit is kept by each node on its own, and every write that is accepted
is sent to every connected node. Nova has no rate limiting: limit requests
per client in the proxy in front of the node. A session backend of your own
sets its own limits.

## Cross-site request forgery

A browser sends the session cookie with requests that another site makes it
send. `SameSite=Lax` on the cookie stops most of them. Add
`nova_csrf_plugin` to the route groups that take forms from signed-in users.
It refuses a `POST`, `PUT`, `PATCH` or `DELETE`:

* that the browser says a page of another site sent, by `Sec-Fetch-Site` and
  `Origin`, before the security callback runs and before the body is read,
* that does not carry the token a page of the site was given.

Set `csrf_secret` so that the tokens are signed and nothing is stored for a
visitor:

```erlang
{nova, [
    {csrf_secret, ~"${CSRF_SECRET}"}
]}
```

See [Nova CSRF](plugins.md#nova-csrf) for the two checks, the token modes and
how to change the secret.

Do not change state in a `GET` route. The plugin does not check them.

## Cross-origin requests

`nova_cors_plugin` decides which other origins may read the responses of
your API from a browser. List the origins you trust in `allow_origins`. The
plugin never sends `access-control-allow-credentials`, so responses to
requests made with cookies are not exposed to other origins. See
[Nova cors](plugins.md#nova-cors).

## Websockets

A websocket upgrade passes the plugins and the security callback of its
route group. A browser sends cookies with an upgrade started by a page of any
site, and CORS does not apply to websockets. Nova therefore checks the
`Origin` header of every request to a websocket route against the allowed
origins of the node, before anything else of the route runs, and answers a
request from another origin with `403`. The default, `same_host`, accepts
pages served from the host the request was sent to. See
[The origin check](websockets.md#the-origin-check).

## Request data

* **Body size.** `nova_request_plugin` reads at most `max_body_length` bytes
  of a body (1 MB by default) within `body_read_timeout` (60 seconds), and
  answers `413` and `408` past them. `nova_multipart_plugin` has limits per
  part, per field and per request. Raise them only on the routes that need
  it. A controller that reads the body itself sets its own limits with the
  options of `cowboy_req:read_body/2`.
* **Uploads.** `nova_multipart_plugin` ends an upload that sends less than
  64 000 bytes in `chunk_timeout` (15 seconds) with `408`, and
  `nova_multipart_file_handler` bounds the bytes spooled on the node at one
  time (`multipart_spool`). That room is not shared out between clients:
  with the defaults, 125 uploads of 64 MB that are kept open for up to the
  60 seconds of `read_timeout` fill it, and other uploads get `503`
  meanwhile. The upload is read once the security callback has let the
  request through, so put upload routes in a group with a callback, and
  limit concurrent uploads per client in the proxy.
* **Binaries, not atoms.** Path parameters, query string keys and the keys of
  a decoded JSON body are binaries. Do not turn data from a request into
  atoms with `binary_to_atom/2` or `list_to_atom/1`: atoms are never garbage
  collected. Never pass request data to `binary_to_term/1`.
* **Redirect targets.** `{redirect, Location}` goes wherever it is told. For
  a target that comes from the request, such as a `next` parameter, return
  `{redirect_local, Path}`, which only redirects to a path on the same site
  and answers `400` for anything else. Controllers, security callbacks and
  plugins all take it.
* **Uploaded files.** The filename, the content type and the field name of
  an upload are chosen by the client. `nova_multipart_file_handler` stores a
  file under a random name and keeps an extension only when it is on the
  `extensions` list. See [Nova multipart](plugins.md#nova-multipart).

Nova has no rate limiting. Limit sign-in attempts and other expensive routes
in your application or in the proxy in front of it.

## Output

* **Views.** ErlyDTL escapes variables for HTML by default. The `safe` filter
  turns that off for a value, so use it only on markup your application
  produced. See [Views](views.md#escaping).
* **JSON.** `{json, Term}` encodes the term with the configured JSON library
  and sets `content-type: application/json`.
* **Static files.** A static directory route only serves files inside the
  directory, also through symbolic links, and sends
  `x-content-type-options: nosniff`. File types a browser would run as a
  page from your origin, such as XHTML and compressed SVG, are served as
  `application/octet-stream`. See [Routing](routing.md#static-files).
* **Response headers.** Nova does not add `strict-transport-security`,
  `content-security-policy`, `x-frame-options` or `referrer-policy`. Set
  them in a plugin of your own or in the proxy in front of the node:

```erlang
-module(my_app_security_headers_plugin).
-behaviour(nova_plugin).
-export([pre_security/4, plugin_info/0]).

pre_security(Req, _Env, _Options, State) ->
    Headers = #{~"x-frame-options" => ~"DENY",
                ~"referrer-policy" => ~"same-origin",
                ~"content-security-policy" => ~"default-src 'self'"},
    {ok, cowboy_req:set_resp_headers(Headers, Req), State}.

plugin_info() ->
    #{title => ~"my_app_security_headers_plugin",
      version => ~"0.1.0",
      url => ~"https://example.com/my_app",
      authors => [~"My App team"],
      description => ~"Adds security headers to every response"}.
```

  The headers are set in `pre_security/4`, so that a response to a request
  the security callback refuses has them too. Responses to requests that
  match no route (`404`, `405`) and to paths that are not canonical (`400`)
  run no plugins and do not get these headers.

## Errors and logs

* The pages for `404` and `500` show a title and a generic message. The
  reason and the stacktrace of a crash are only on the page when
  `environment` is set to `dev`. See [Error handling](error-handling.md).
* What is kept out of the logs, see [Logging](logging.md#redaction):

  | Logged | Redacted |
  |---|---|
  | Crash reasons and stacktraces that Nova logs or shows on an error page | A request map is reduced to its method and path, the arguments of a stacktrace frame to their number, and values under keys that look sensitive are replaced with `redacted` |
  | A `csrf_secret` that is refused at boot | Replaced with `redacted`, in the log and in the error the boot fails with |
  | Any other setting that is refused at boot | No. The value is logged as it was set |
  | Reports, metadata and format arguments that your own code logs | Only with `nova_jsonlogger` and `redact => true` |
  | A secret that is text already: a message logged as a plain string, a format string, anything inside a binary such as the unparsed bytes of a request in a crash report | No |
* Telemetry events carry the route template and not the path. The raw path
  and the address of the client are only added when
  `telemetry_request_fields` asks for them.

## Transport

Serve production traffic over TLS, either from the node (`use_ssl` and
`ssl_options` in `cowboy_configuration`) or from a proxy in front of it. See
[Building releases](building-releases.md#tls).

### Behind a proxy that terminates TLS

Nova then receives plain HTTP. What to check:

| Check | Why |
|---|---|
| The proxy passes the `Host` header on as the browser sent it | `same_host` compares the `Origin` of a request with the host and port in `Host`. With a rewritten `Host`, websocket upgrades from the pages of the site are refused with `403`, and so are the requests `nova_csrf_plugin` goes by the `Origin` of |
| `allowed_origins` is `same_host` or a list of the public origins, not `same_origin` | `same_origin` compares the scheme too, and the scheme Nova sees is `http`. Every `https` origin is refused |
| `allowed_origins` is a list when the proxy cannot keep `Host` | A list is compared with the `Origin` alone. Put every public origin of the site in it, such as `[~"https://app.example.com"]` |
| `allowed_origins` is not `any` | `any` also turns off the origin check of `nova_csrf_plugin` |
| `environment` is not `dev` | The session cookie is `Secure` whatever the scheme, unless `environment` is `dev` |
| The node is only reachable through the proxy | Nova does not read `x-forwarded-for`, `x-forwarded-proto` or `forwarded`. `cowboy_req:peer/1` is the address of the proxy, and a header your own code reads can be set by anyone who reaches the node directly |
| `strict-transport-security` is set, by the proxy or by a plugin | Nova does not add it |

```erlang
{nova, [
    {environment, prod},
    {allowed_origins, [~"https://app.example.com"]}
]}
```

## Before going to production

| Check | Where |
|---|---|
| `environment` is not `dev` | `{nova, [{environment, prod}]}`. When it is not set it is read as `prod`, and Nova logs a notice at boot |
| `use_stacktrace` is `false` | `nova` environment |
| Every route group has the `security` it needs | `rebar3 nova audit --env prod --strict` |
| The session cookie has a `__Host-` name | `session_cookie_name` |
| `nova_session:renew/1` is called at sign-in | Your sign-in controller |
| Forms of signed-in users are covered by `nova_csrf_plugin` | Route groups |
| CSRF tokens are signed, with a secret that is the same on every node and not in the repository | See [Cross-site request forgery](#cross-site-request-forgery) |
| The allowed origins fit how the site is reached | See [Behind a proxy that terminates TLS](#behind-a-proxy-that-terminates-tls) |
| `non_canonical_paths` is not `resolve` | `nova` environment |
| Data for visitors who are not signed in is stored with `nova_session:set_provisional/3` | Controllers and plugins that write to the session |
| Upload routes have a security callback | Route groups |
| `plugins_before_security` only lists plugins whose callback needs the body | Route groups and the `nova` environment |
| The boot logs no warning about the order of plugins | See [Warnings about the order](plugins.md#warnings-about-the-order) |
| CORS lists the origins you trust, not `*` | `nova_cors_plugin` options |
| Traffic is served over TLS | `cowboy_configuration` or the proxy |
| The release has a distribution cookie that is long and random | `RELEASE_COOKIE`, see [Building releases](building-releases.md) |
| No watchers are configured | `watchers` in the environment of your application |
| Logs redact what your own code logs | `nova_jsonlogger` with `redact => true` |

## Reporting a vulnerability

Do not open a public issue for a vulnerability in Nova. Report it privately
as described in the
[security policy](https://github.com/novaframework/nova/security/policy).
