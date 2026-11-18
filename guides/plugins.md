# Plugins

A plugin is a module that runs before or after the controller of a request.
Plugins read or change the request, add response headers, or answer the
request themselves. The plugins that come with Nova parse request bodies, add
CORS headers, check CSRF tokens and more, see [Nova plugins](#nova-plugins).

## Configuring plugins

A plugin is configured as `{Phase, Module, Options}`. The phases of a request
are `pre_request` and `post_request`. Plugins of one phase run in the order
they are listed. A `pre_request` plugin runs ahead of the security callback of
the route, behind it, or both, see
[Before and after the security callback](#before-and-after-the-security-callback).

Global plugins are set under the `nova` application in *sys.config*:

```erlang
{nova, [
    {plugins, [
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]}
]}
```

The plugins of a route group are set in the router:

```erlang
#{prefix => "/api",
  security => false,
  plugins => [
      {pre_request, nova_request_plugin, #{decode_json_body => true}}
  ],
  routes => [
      {"/items", fun my_app_items_controller:create/1, #{methods => [post]}}
  ]}
```

A route group that declares `plugins` uses those and not the global ones. A
group without the key uses the global ones. See
[Routing](routing.md#using-plugins-local-to-a-set-of-endpoints) for the ways to combine them in an included
application.

A controller reads what a plugin added to the request. With the configuration
above the decoded body is under the atom key `json`:

```erlang
create(#{json := #{~"id" := Id, ~"value" := Value}}) ->
    {json, 200, #{}, #{id => Id, received => Value, increment => Value + 1}}.
```

## Writing a plugin

A plugin implements the `nova_plugin` behaviour. `pre_security/4`,
`pre_request/4` and `post_request/4` take the request, the environment of
the request steps, the options from the configuration and the state of the plugin:

```erlang
-module(my_app_header_plugin).
-behaviour(nova_plugin).
-export([pre_request/4, post_request/4, plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    {ok, nova_req:set_resp_header(~"x-served-by", ~"my_app", Req), State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"my_app_header_plugin",
      version => ~"0.1.0",
      url => ~"https://example.com/my_app",
      authors => [~"My App team"],
      description => ~"Adds an x-served-by header to every response",
      options => []}.
```

A plugin exports the callbacks of the phases it is configured for. A
`pre_request` plugin exports `pre_security/4`, `pre_request/4` or both.
`plugin_info/0` is required and its map needs at least `title` and
`version`. `rebar3 new nova_plugin <name>` creates a skeleton.

Keep data that belongs to one request in `Req`. The state is one value per
plugin module, shared by every request, see below.

## Return values

`pre_security/4`, `pre_request/4` and `post_request/4` return one of:

| Return | Effect |
|--------|--------|
| `{ok, Req, State}` | Carry on with the next plugin. |
| `{ok, Reply, Req, State}` | Set the reply's status, headers and body on `Req` and carry on. Only `{reply, ...}` is accepted here; see below. |
| `{stop, Req, State}` | End the request here. The plugin has replied itself, for example with `nova_req:reply/4`. |
| `{stop, Req}` | Deprecated form of `{stop, Req, State}` that keeps the plugin's state, removed in 1.0. |
| `{stop, Reply, Req, State}` | End the request and send `Reply`. |
| `{break, Req, State}`, `{break, Reply, Req, State}` | Deprecated aliases of the matching `stop` returns, removed in 1.0. They log a warning, once per plugin module. |
| `{error, Reason}` | End the request with the 500 status page. |

`Reply` is one of `{reply, Body}`, `{reply, Status, Body}`, `{reply, Status, Headers, Body}`, `{redirect, Url}`, `{redirect_local, Path}`, `{status, Status}` or `{status, Status, Headers}`. `Headers` is a map or a list of `{Name, Value}` pairs. `{status, ...}` sends the bare status without rendering a status-code route. Any other reply renders the 500 status page.

`{redirect, Url}` answers `302` to any location, another site included. `{redirect_local, Path}` answers `302` only to a path on the site the request was made to. Anything else, such as `https://example.com/` or `//example.com/`, is answered with a bare `400` and logged as the warning `Plugin refused to redirect to a location that is not a local path`. Use it for a target that comes from the request:

```erlang
pre_request(Req, _Env, _Options, State) ->
    case nova_session:get(Req, ~"user_id") of
        {ok, _} ->
            {ok, Req, State};
        {error, _} ->
            Next = proplists:get_value(~"next", nova_req:parse_qs(Req), ~"/"),
            {stop, {redirect_local, Next}, Req, State}
    end.
```

A `post_request` plugin runs after the controller has answered, so a `Reply` it returns with `stop` is dropped, with one warning per plugin in the log.

A request is answered once. A plugin that answers with `nova_req:reply/4` returns `{stop, Req1, State}` with the request that call gave back. When it hands back the request it was called with, Nova answers a second time: that response is dropped, the client keeps the first one, and the warning `The request was answered already, so this response is not sent ...` is logged, once per request.

With `ok` the controller still runs and answers, so a redirect or a bare status set there would be overwritten. `{ok, {redirect, ...}, Req, State}` and `{ok, {status, ...}, Req, State}` therefore render the 500 status page and log that `stop` is needed. To send them, return `{stop, Reply, Req, State}`:

```erlang
pre_request(Req, _Env, _Options, State) ->
    case nova_session:get(Req, ~"user_id") of
        {ok, _} -> {ok, Req, State};
        {error, _} -> {stop, {redirect, "/login"}, Req, State}
    end.
```

Once a `pre_request` plugin stops the request or the security function refuses it, the controller and the `post_request` plugins do not run. Keep access logging and similar work that has to see every request in a stream handler or in `pre_security/4` of a plugin.

The state a plugin returns is stored per plugin module, not per request, and is shared by every request. Nova only writes it when it changed, and concurrent requests overwrite each other's writes: the last writer wins. Keep per-request data in `Req`, and keep shared data that has to add up (counters, rate-limit buckets) in `ets` or `counters`.

## Before and after the security callback

A plugin listed as `{pre_request, Module, Options}` takes part twice before the controller, with the same options:

| Callback | Runs | For |
|---|---|---|
| `pre_security/4` | Ahead of the security callback of the route, for every request to the route | What has to happen whether or not the request is let in: answering a CORS preflight, setting a correlation id, turning a client away by address or rate. It must not read the request body |
| `pre_request/4` | Once the security callback has let the request through. At once on a route without one | Reading and decoding the body, and what only a request that is let in needs |

Both are optional. Within each of the two the plugins run in the order they are listed.

A plugin that turns clients away before they are authenticated does it in `pre_security/4`:

```erlang
-module(my_app_blocklist_plugin).
-behaviour(nova_plugin).
-export([pre_security/4, plugin_info/0]).

pre_security(Req, _Env, #{blocked := Blocked}, State) ->
    {Address, _Port} = nova_req:peer(Req),
    case lists:member(Address, Blocked) of
        true -> {stop, {status, 403}, Req, State};
        false -> {ok, Req, State}
    end.

plugin_info() ->
    #{title => ~"my_app_blocklist_plugin",
      version => ~"0.1.0",
      url => ~"https://example.com/my_app",
      authors => [~"My App team"],
      description => ~"Refuses requests from listed addresses",
      options => [{blocked, ~"The addresses to refuse, as inet:ip_address()"}]}.
```

```erlang
{pre_request, my_app_blocklist_plugin, #{blocked => [{203, 0, 113, 7}]}}
```

The plugins of Nova are split like this:

| Plugin | `pre_security/4` | `pre_request/4` |
|---|---|---|
| `nova_correlation_plugin` | Sets the correlation id | |
| `nova_cors_plugin` | Sets the CORS headers and answers a preflight | |
| `nova_ws_origin_plugin` | Checks where a request to a websocket route comes from | |
| `nova_csrf_plugin` | Checks where a request that changes state comes from | Hands out and checks the token |
| `nova_request_plugin` | Parses the query string into `parsed_qs` | Reads and decodes the body into `json`, `params` and `body` |
| `nova_multipart_plugin` | | Reads the parts into `params` and `files` |

The security callback finds in the request what `pre_security/4` put there: the correlation id, the CORS response headers and `parsed_qs`. `json`, `params`, `body` and `files` are not there yet. Nothing of the body is read from a client the callback refuses.

`auth_data` of the request is what the security callback returned as `{true, AuthData}`. A plugin that writes the key, in `pre_security/4` or in `pre_request/4`, does not change that. What a plugin writes there stays when the callback returns a plain `true`, and on a route without a callback, so a plugin can be what signs a request in. A websocket controller gets the `auth_data` of the upgrade request in its controller data, whoever wrote it.

A websocket route runs its `pre_request` plugins on the upgrade request, in the same two steps. For the time after the upgrade it has the phases `pre_ws_request` and `post_ws_request`, which run around every callback of the controller, and `post_ws_connection`, which runs once when the connection goes away: after `terminate/3` if the controller exports it, and also when it does not or when it crashes. `post_ws_connection` does not run for an upgrade that was refused. See the [Websockets](websockets.md#plugins) guide.

Releases up to 0.18 ran `pre_request/4` of every plugin ahead of the security callback. `plugins_before_security` puts it back there for the modules it names. It is a key of a route group or of the `nova` environment, and a route group that has the key does not read the one of the environment. Nothing moves `pre_security/4` behind the security callback.

### Running a plugin ahead of the security callback

`plugins_before_security` names the modules whose `pre_request/4` runs ahead of the security callback, right after their `pre_security/4`. Use it for a security callback that reads what a plugin decodes, such as one that checks a signature over the body:

```erlang
#{prefix => "/hooks",
  security => fun my_app_hooks:is_signed/1,
  plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],
  plugins_before_security => [nova_request_plugin],
  routes => [{"/payment", fun my_app_hooks:payment/1, #{methods => [post]}}]}
```

```erlang
is_signed(Req = #{body := Body}) ->
    Sent = nova_req:header(~"x-signature", Req, ~""),
    Expected = binary:encode_hex(crypto:mac(hmac, sha256, signing_key(), Body), lowercase),
    byte_size(Sent) =:= byte_size(Expected) andalso crypto:hash_equals(Sent, Expected).
```

The body of every request to such a route is read and decoded, also of one the callback goes on to refuse. Keep `max_body_length` low on it.

The value is `all`, for every plugin of the route group, or a list of modules:

```erlang
{nova, [{plugins_before_security, [nova_request_plugin]}]}
```

| Where | What the list may name | Otherwise |
|---|---|---|
| `nova` environment | Modules that export `pre_request/4` | `nova_sup` fails to start with `{invalid_setting, plugins_before_security, Value}` |
| Route group | Modules that export `pre_request/4` and that the group uses as `pre_request` plugins | The routes are refused with `{invalid_setting, plugins_before_security, Value}` and the application does not start |

`[nova_cors_plugin]` is refused in both places, since that plugin has no `pre_request/4`, and so are `[all]` and a misspelt name. The default is `[]`.

A CORS preflight to a route that does not take `OPTIONS` runs `pre_security/4` only, also of a module that is listed here, see [Nova cors](#nova-cors).

### Saying whether the security callback reads the body

A route group that has a security callback and decodes a body behind it is warned about once, when its routes are compiled: a callback written for 0.18 that reads `json` or `params` finds none. `security_reads_body` in the route group says which it is:

```erlang
#{prefix => "/api",
  security => fun my_app_auth:bearer/1,
  security_reads_body => false,
  plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],
  routes => [{"/items", fun my_app_items_controller:create/1, #{methods => [post]}}]}
```

| Value | Says | Effect |
|---|---|---|
| `false` | The callback does not read what `nova_request_plugin` decodes | Ends the warning |
| `true` | The callback reads it | The routes only compile when `nova_request_plugin` is listed under `plugins_before_security`. When its body is decoded behind the callback, the application does not start, with `{invalid_setting, security_reads_body, {requires, plugins_before_security}}` |
| Anything else | | The application does not start, with `{invalid_setting, security_reads_body, Value}` |

A route group that lists `nova_csrf_plugin` and reads a form with `read_urlencoded_body`, and no JSON, is not warned about: the form is read for the token. Its security callback finds no `params` either, and nothing shows whether it reads them, so such a group is told once at the `notice` level instead, with `notice => form_behind_security` in the report. `security_reads_body => false` ends that too, and `security_reads_body => true` is what to set when the callback reads `params`:

```erlang
#{prefix => "/account",
  security => fun my_app_auth:session/1,
  security_reads_body => false,
  plugins => [{pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
              {pre_request, nova_csrf_plugin, #{}}],
  routes => [{"/profile", fun my_app_profile_controller:update/1, #{methods => [post]}}]}
```

`security_reads_body => false` is the word of the author of a route group about the callback the group declares as its `security`. It does not answer for a callback the routes are behind because of where their application is mounted: the `secure` or `override_secure` of the `nova_apps` entry, or the `security` of a route group that lists the application under `apps`. Somebody else wrote that callback. Such a route group is warned about as if it had said nothing. When the callback in front does read the body, list `nova_request_plugin` under `plugins_before_security`, in the route group or in the `nova` environment.

### Warnings about the order

What the order changes without anything failing is logged when routes are compiled, once for a route group and its plugins. Each report names the application, the prefix and the plugins, and says which of the four it is under `notice`. They are logged until 1.0.

| `notice` | Level | Starts with | When | Ends when |
|---|---|---|---|---|
| `body_behind_security` | warning | `The security callback of this route group runs before nova_request_plugin reads the body` | A route group with a security callback has `nova_request_plugin` with `decode_json_body` or `read_urlencoded_body`, and the body is read behind the callback | The group sets `security_reads_body => false`, or lists the plugin under `plugins_before_security` |
| `form_behind_security` | notice | `The security callback of this route group runs before nova_request_plugin reads the form` | The same, in a group that also lists `nova_csrf_plugin` and whose `nova_request_plugin` does not have `decode_json_body`. It is logged in place of the warning above | The group sets `security_reads_body => false`, or lists the plugin under `plugins_before_security` |
| `pre_request_behind_security` | warning | `pre_request/4 of these plugins runs once the security callback of this route group has let a request through` | A route group with a security callback has a plugin from outside Nova that exports `pre_request/4` and not `pre_security/4` | The plugin exports `pre_security/4`, or is listed under `plugins_before_security` |
| `pre_request_not_for_preflight` | warning | `pre_request/4 of these plugins does not run for a CORS preflight to a route of this group that does not take OPTIONS` | A route group has such a plugin and a route whose `methods` has neither `options` nor `'_'`, and the warning above does not name the plugin already: the group has no security callback, or the plugin is listed under `plugins_before_security` | The plugin exports `pre_security/4`. Nothing else ends it: `pre_request/4` does not run for such a preflight wherever the plugin is listed |


## Request errors

`nova_req` exits with `{request_error, Reason, Message}` when the client sent something it cannot parse, or did not send the body in time: `parse_qs/1`, `parse_header/2`, `parse_cookies/1`, `read_urlencoded_body/2` and the multipart readers do. Nova does not treat that as a crash. In a `pre_request` plugin the request is answered with a bare `400`, `408` when the body timed out or `413` when it was too large. In a controller the same status goes through the status-code route for it, if the application has one. Both are logged as a warning.

## Reading the matched route

A plugin can read the template of the route that matched from `Req` under
`nova_route`, for example `<<"/users/:id">>`. It is absent when no route
matched, so match on it rather than assume it:

```erlang
pre_request(Req = #{nova_route := Route}, _Env, _Opts, State) ->
    {ok, Req#{metric_label => Route}, State};
pre_request(Req, _Env, _Opts, State) ->
    {ok, Req, State}.
```

The template does not include the host, and two applications can share one,
so pair it with the application (`app` in `Env`) when you aggregate on it. See
[The matched route](routing.md#the-matched-route) for the full rules.

## Optional callbacks

`init/0` and `stop/1` are optional. `init/0` is called when the plugin is loaded and returns the initial state of the plugin. `stop/1` is called with the state when the plugin is unloaded, usually when Nova stops. Older releases called `stop/0`; it is still called when `stop/1` is not exported.

```erlang
init() ->
    #{started_at => erlang:system_time(second)}.

stop(_State) ->
    ok.
```

`init/0` runs while `nova_sup` is starting, when the routes that use the plugin are compiled, and `nova_sup` waits for it to return. It must not call into `nova_sup`, as in `supervisor:which_children(nova_sup)` or `nova_sup:add_application/2`: `nova_sup` cannot answer before `init/0` has returned, so Nova fails to start, after five seconds of waiting.

## Request pipeline

A request passes these steps in this order. A step that answers the request
ends it, and the steps after it do not run.

1. **Routing.** The path and the method are looked up in the routing table.
   A path that is not canonical has been answered with `400` before that, see
   [Routing](routing.md#paths-that-are-not-canonical).
2. **`pre_security/4`** of the `pre_request` plugins, in the order they are
   listed. On a websocket route the origin check comes first. `pre_request/4`
   of a plugin listed under `plugins_before_security` runs here too.
3. **The security callback** of the route. A route of an included
   application can have several, see the
   [multi-app guide](multi-app.md#security-of-a-mounted-application).
4. **`pre_request/4`** of the `pre_request` plugins, in the order they are
   listed.
5. **The controller**, or the upgrade of a `protocol => ws` route.
6. **`post_request` plugins.** The response has been sent by then.

What follows from that order:

- `pre_security/4` sees every request to its routes, also the ones the
  security callback is about to refuse, and `auth_data` is not set yet.
- `pre_request/4` sees only requests the security callback let through. It
  finds `auth_data` in the request when the callback returned
  `{true, AuthData}`.
- The security callback sees what `pre_security/4` added to the request, such
  as `parsed_qs` from `nova_request_plugin`. It does not see `json`, `params`
  or `files`, unless the plugin is listed under `plugins_before_security`.
- `nova_csrf_plugin` and `nova_ws_origin_plugin` refuse a request from
  another origin in `pre_security/4`, before the security callback is asked.
  The token of `nova_csrf_plugin` is checked in `pre_request/4`, after it.
- A plugin that returns `{stop, ...}` answers the request itself. Nothing
  after it runs: from `pre_security/4` that leaves out the security callback,
  `pre_request/4`, the controller and the `post_request` plugins.
- A plugin that crashes in either callback is logged and answered with the
  `500` page. The controller is not called and a websocket is not upgraded.
- A request the security callback refuses is answered with `401`, or with what
  the callback returned, and one whose callback crashes with `500`. Neither
  reaches `pre_request/4`, the controller or the `post_request` plugins.
- `auth_data` in the request is what the security callback returned as
  `{true, AuthData}`, whatever a plugin wrote there, in `pre_security/4` or
  in `pre_request/4`. What a plugin wrote stays when the callback returns
  `true` and on a route without a callback. A websocket controller gets the
  `auth_data` of the upgrade request under `auth_data` in its controller
  data.
- A request that matches no route gets the `404` page, or `405` when the path
  exists for another method, and runs no plugins at all. Headers that a plugin
  adds to every response, CORS headers or a correlation ID, are missing from
  those two. The one exception is a CORS preflight, see
  [Nova cors](#nova-cors).
- A page declared for a status code is a route like any other. Declared in a
  group with `security`, the `404` and `405` pages are only rendered for a
  request that callback lets through, and every other request gets a `401`.
  Declare status pages in a group without one.

A websocket upgrade is a request and passes the same steps, so the origin
check and the security callback have run before the connection is accepted.
Over HTTP/1.1 the `post_request` plugins run when the upgrade has been
answered, not when the connection ends. Over HTTP/2 they do not run. After
that every frame and message runs the `pre_ws_request` plugins, the controller
and the `post_ws_request` plugins. `post_ws_connection` plugins run when the
connection ends, whether or not the controller exports `terminate/3`.

## Nova plugins

These plugins come with Nova.

|Plugin|Description|Code|
|------|-----------|----|
|nova_correlation_plugin|Gives every request a correlation id: in the `x-correlation-id` response header, in the logger metadata and under `correlation_id` in the request.|[nova_correlation_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_correlation_plugin.erl)|
|nova_cors_plugin|Answers CORS preflights and adds the CORS headers to responses.|[nova_cors_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_cors_plugin.erl)|
|nova_csrf_plugin|Refuses a state-changing request that another site sent, or that does not carry the CSRF token of the client.|[nova_csrf_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_csrf_plugin.erl)|
|nova_multipart_plugin|Reads `multipart/form-data` bodies and streams uploaded files to a handler.|[nova_multipart_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_multipart_plugin.erl)|
|nova_request_plugin|Parses the query string and decodes urlencoded and JSON bodies. Multipart bodies are left to `nova_multipart_plugin`.|[nova_request_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_request_plugin.erl)|
|nova_ws_origin_plugin|Refuses a websocket upgrade that a page on another origin started. Nova runs it on every websocket route, see [Websockets](websockets.md#the-origin-check).|[nova_ws_origin_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_ws_origin_plugin.erl)|


### Nova correlation

The plugin gives every request a correlation id. It sets the id in the
`x-correlation-id` response header, adds it to the logger metadata of the
request process, and puts it in the request under `correlation_id`, for a
controller that passes it on to other services.

```erlang
{pre_request, nova_correlation_plugin, #{request_correlation_header => ~"x-correlation-id",
                                         logger_metadata_key => correlation_id}}
```

|Option|Description|
|------|-----------|
|request_correlation_header|The request header to take the id from. Without the option the request is not looked at and an id is always generated.|
|logger_metadata_key|The key of the id in the logger metadata, an atom. Defaults to the atom `'correlation-id'`. A binary is taken as the atom of that name when the node knows such an atom. When it does not, the default is used and an error is logged once.|

A generated id is a version 4 UUID. The response header is always
`x-correlation-id`, whatever `request_correlation_header` is.

A correlation ID from the request is used when it is 1 to 255 visible ASCII characters. A value with spaces, control characters or other bytes, or a longer one, is ignored and a new ID is generated, because the ID is copied to the logger metadata and to the response.

### Nova cors

This plugin answers CORS preflights (an `OPTIONS` request with both `Origin` and `Access-Control-Request-Method`) itself with a `204` and the CORS headers, so the controller never sees them. Any other request, including a plain `OPTIONS`, goes on to the application with the CORS headers added to the response.

The plugin works in `pre_security/4`, so a preflight is answered ahead of the security callback of the route. A preflight carries no credentials, and a browser shows a `401` to the page only when the response has the CORS headers.

The route does not have to accept `OPTIONS` for that. A preflight names the method of the request it is about, in `Access-Control-Request-Method`, and Nova runs `pre_security/4` of the `pre_request` plugins of the route for that method on it, and then its `post_request` plugins. `{"/orders", fun orders:create/1, #{methods => [post]}}` in a route group with this plugin therefore answers the preflight for a `POST`. Without a plugin that answers, the preflight gets the `405` it always got, as does a preflight for a method the path has no route for, and a plain `OPTIONS`.

For such a preflight the security callback of the route does not run. Neither does `pre_request/4` of any plugin, one listed under `plugins_before_security` included. A plugin of your own that answers preflights has to do it in `pre_security/4`.

The plugin never sends `access-control-allow-credentials`, so a browser does not expose a response to a request made with cookies or HTTP authentication.

```erlang
{pre_request, nova_cors_plugin, #{allow_origins => [~"https://app.example.com",
                                                    ~"https://admin.example.com"]}}
```

With a list (or a single origin), the request's `Origin` is echoed back in `access-control-allow-origin` only when it is on the list, and every response carries `vary: origin` (added to any `vary` value already set) so caches keep the answers apart. `<<"*">>` allows any origin.

By default a preflight is told that every header it asks for is allowed. That is only safe because `access-control-allow-origin` is sent to allowed origins alone: list the origins you trust, and set `allow_headers` explicitly when you use `<<"*">>`.

|Option|Description|
|------|-----------|
|allow_origins|Required. `<<"*">>`, an origin, or a list of origins.|
|allow_headers|Request headers a preflight allows, as a binary or a list. Defaults to the headers the preflight asks for in `access-control-request-headers`; a literal `*` would not cover `authorization`.|
|allow_methods|Methods a preflight allows, as a binary or a list. Defaults to `<<"*">>`.|
|max_age|Seconds a browser may cache the preflight answer.|

### Nova CSRF

The plugin protects forms against cross-site request forgery. It makes two
checks on a request that changes state, which is every method but `GET`,
`HEAD` and `OPTIONS`:

| Check | Runs in | A refused request gets |
|---|---|---|
| The browser says another site sent the request | `pre_security/4`, before the security callback and before the body is read | `403` and `Forbidden - request from another origin` |
| The request carries the token a page of the site was given | `pre_request/4`, after the security callback | `403` and `Forbidden - CSRF token invalid` |

The controller is not called for a request that is refused. For `GET`, `HEAD`
and `OPTIONS` the plugin puts a token in the request under `csrf_token`. The
other methods send it back in the `x-csrf-token` header or in the form field
`_csrf_token`.

```erlang
#{prefix => "/account",
  security => fun my_app_auth:session/1,
  security_reads_body => false,
  plugins => [
      {pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
      {pre_request, nova_csrf_plugin, #{}}
  ],
  routes => [
      {"/profile", fun my_app_profile_controller:show/1, #{methods => [get]}},
      {"/profile", fun my_app_profile_controller:update/1, #{methods => [post]}}
  ]}
```

The plugin needs sessions: the token is kept in the session of a client or
signed for its session cookie. A route group that lists the plugin while
`use_sessions` is `false` is refused when the routes are compiled, and the
application does not start, with
`{invalid_setting, use_sessions, {required_by, nova_csrf_plugin}}`. The plugin
that parses the form, `nova_request_plugin` or `nova_multipart_plugin`, has to
be listed before it so the field is in `params`.

The form is read once the security callback has let the request through, so
the callback finds no `params` in the request. `security_reads_body => false`
in the example says that the callback does not read them. Without it such a
route group logs one notice when its routes are compiled, see
[Saying whether the security callback reads the body](#saying-whether-the-security-callback-reads-the-body).

A view gets the token as the variable `csrf_token`:

```html+django
<form method="post" action="/account/profile">
  <input type="hidden" name="_csrf_token" value="{{ csrf_token }}">
  <button>Save</button>
</form>
```

A page that submits with JavaScript sends the token in the `x-csrf-token`
header. Every response carries another token, and the earlier ones are still
accepted.

|Option|Description|
|------|-----------|
|field_name|The form field that carries the token. Defaults to `~"_csrf_token"`.|
|header_name|The request header that carries the token. Defaults to `~"x-csrf-token"`.|
|session_key|The session key a token kept in the session is stored under. Defaults to `~"_csrf_token"`.|
|trusted_origins|The origins a request that changes state may come from, for this route group: `same_host`, `same_origin`, a list of origins, or `any`, which turns the check off. Defaults to `trusted_origins` of the `nova` environment, where `any` is read as `same_host`. See [Trusted origins](#csrf-trusted-origins).|
|excluded_paths|Path prefixes that are not checked, by either check. A prefix covers whole path segments: `~"/api"` covers `/api` and `/api/hooks`, not `/apikeys`. Defaults to `[]`.|

Routes for API clients that authenticate with a token in a header and not
with the session cookie do not need the plugin.

### CSRF token modes

The plugin makes its tokens in one of two ways. `secret_key_base` and
`csrf_token` of the `nova` environment decide which:

| `secret_key_base` | `csrf_token` | Tokens |
|---|---|---|
| Set | Not set, or `stateless` | Signed. Nothing is stored |
| Set | `session` | Kept in the session |
| Not set | Not set | Kept in the session. A notice is logged once, when the plugin is first loaded |
| Not set | `session` | Kept in the session, without the notice |
| Not set | `stateless` | Nova does not start, with `{invalid_setting, secret_key_base, not_set}` |

`{csrf_token, stateless}` with `use_sessions` set to `false` stops the boot
too, with `{invalid_setting, csrf_token, {requires, use_sessions}}`.

```erlang
{nova, [
    {use_sessions, true},
    {secret_key_base, ~"${SECRET_KEY_BASE}"}
]}
```

`secret_key_base` is the one secret of the node. Nova never signs with it as
it is: the plugin has a key of its own made from it, and the session ids
have another, see [Configuration](configuration.md#the-secret-of-the-node).
The example is for a `sys.config.src` of a release, which takes the value
from the environment variable `SECRET_KEY_BASE` when the node starts. Give
every node the same secret, and keep it across restarts: a token signed with
another secret is refused.

The secret has to come out of a random source. Whoever sees a signed token
can try secrets against it at home, so a word or a sentence does not do,
however long. Make one in an Erlang shell or with `openssl`. Both give 64
characters:

```erlang
base64:encode(crypto:strong_rand_bytes(48)).
```

```sh
openssl rand -base64 48
```

It is a binary of at least 32 bytes. Nova does not start, with
`{invalid_setting, secret_key_base, redacted}`, with a secret that is shorter
or that plainly is not random: fewer than 8 different bytes, such as a letter
or a word repeated, an example out of this documentation, or a `${VARIABLE}`
that the release did not fill in. Nova cannot tell a sentence from a random
value, so a secret that passes this check is not shown to be a good one.

A signed token is tied to the session cookie of the client:

- It is an HMAC-SHA256, under the key the plugin has from the secret, over
  the id in the session cookie and 16 random bytes. Each token is good for as
  long as the client holds that cookie and the secret is in use.
- A visitor without a session cookie is handed one by the response. No
  session is started and nothing is stored for it. What the visitor first
  writes to the session is stored under the id of that cookie, so the tokens
  of the pages that are open stay good.
- A request that changes state is refused when it does not send the cookie
  its token was signed for.
- A token is the characters a page was given. The same bytes written another
  way, with whitespace in them or with other bits to spare in the last
  character, are refused. That goes for a token kept in the session too.
- `nova_session:sign_in/3`, which is how a user is signed in, moves the
  client to another session id, and so do `nova_session:renew/1` and
  `nova_session:delete/1`. A token handed out before that is refused
  afterwards. That goes for `csrf_token` in the request that signs in too, so
  answer a sign-in with a redirect: the next page is then rendered for the
  new cookie.
- A session cookie that was handed out before the secret was set, or whose id
  was signed with a secret that is no longer listed, is replaced by the first
  write to the session when no session is stored under it. The tokens made
  for it are refused from then on.

To change the secret, set a list. The first secret signs and all of them are
accepted:

```erlang
{nova, [
    {secret_key_base, [~"${SECRET_KEY_BASE}", ~"${SECRET_KEY_BASE_OLD}"]}
]}
```

Take the old secret out once the pages that were open have been sent.

A token kept in the session is a random value, stored with
`nova_session:set_provisional/3`, so every visitor to a page with the plugin
gets a session. A page gets the token under a mask that is new for every
response.

After a secret is set, a form that was rendered before still sends the token
of its session. It is accepted for as long as that session is there.

### CSRF trusted origins

The first check goes by two request headers that a page cannot set,
`Sec-Fetch-Site` and `Origin`, and by `trusted_origins`:

| The request has | `same_host` | `same_origin` | A list | `any`, as the option of the plugin |
|---|---|---|---|---|
| `Sec-Fetch-Site: same-origin` or `none` | Passes | Passes | Passes | Passes |
| Any other `Sec-Fetch-Site`, such as `same-site` or `cross-site` | Refused | Refused | Passes when the `Origin` is in the list | Passes |
| No `Sec-Fetch-Site` and no `Origin` | Passes | Passes | Passes | Passes |
| No `Sec-Fetch-Site`, and an `Origin` | Passes when host and port are those of the request. A request that came in over TLS also needs an `https` origin | Passes when scheme, host and port are those of the request | Passes when the `Origin` is in the list | Passes |

A request with neither header is not from a browser, or from one too old to
send them. Without `Sec-Fetch-Site`, `Origin: null` is refused by everything
but `any`. The
[Configuration](configuration.md#trusted-origins) guide says how an origin is
compared with each value.

`trusted_origins` of the `nova` environment is the default, and websocket
upgrades go by the same setting. `any` there opens websocket upgrades to
every origin and nothing else: the plugin then checks by `same_host`. To let
pages of other origins post to one route group, give the plugin of that group
a list. A list takes the place of `same_host`, so the origin of the site
itself goes in it too:

```erlang
#{prefix => "/embed",
  security => fun my_app_auth:session/1,
  security_reads_body => false,
  plugins => [
      {pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
      {pre_request, nova_csrf_plugin, #{trusted_origins => [~"https://app.example.com",
                                                            ~"https://partner.example.org"]}}
  ],
  routes => [
      {"/comment", fun my_app_embed_controller:comment/1, #{methods => [post]}}
  ]}
```

The option takes the values of the setting. Any other value is refused when
the routes are compiled, and the application does not start, with
`{invalid_setting, trusted_origins, Value}`. The option `any` turns this
check off for the route group and leaves the token check: it is the one way
to turn it off. `nova_ws_origin_plugin` takes the same option, see
[Websockets](websockets.md#the-origin-check).

### Nova request

The plugin parses the query string and decodes the request body, and adds the result to the request. The query string is parsed in `pre_security/4`, ahead of the security callback. The body is read and decoded in `pre_request/4`, once the callback has let the request through, so `json`, `params` and `body` are not in the request the callback gets. See [Running a plugin ahead of the security callback](#running-a-plugin-ahead-of-the-security-callback) for a callback that needs them.

```erlang
{pre_request, nova_request_plugin, #{decode_json_body => true,
                                     read_urlencoded_body => true,
                                     parse_qs => true|list}}
```

An empty or malformed JSON body is answered with a bare `400` (no body, no status page). With the default `json_lib`, so is a body with an object that has a key twice, such as `{"role":"user","role":"admin"}`, and one with anything but whitespace after the JSON value. A `json_lib` of your own decides both itself. A query string or a urlencoded body that cannot be parsed gets a bare `400` as well, and so does a query string with more than 100 keys. A urlencoded body with more than 100 fields gets a bare `413`. `parse_qs => false` leaves the query string alone.

Each is logged as the warning `Request refused`, with `reason` and `status_code` and nothing of what the client sent. For a JSON body that does not decode the report is:

```erlang
#{msg => ~"Request refused", reason => ~"Malformed JSON body", status_code => 400,
  error => invalid_byte, position => 22}
```

`error` is the kind of error, an atom such as `invalid_byte`, `unexpected_sequence`, `unexpected_end` or `duplicate_key`. `position` is the byte offset where OTP's `json` found an `invalid_byte` or an `unexpected_sequence`, and is left out for the other errors and with a `json_lib` of your own. An empty body is logged with `reason => ~"Empty JSON body"` and neither key.

The body is read only when `decode_json_body` or `read_urlencoded_body` is set,
and at most `max_body_length` bytes of it (default `1000000`, 1 MB). A body
whose `Content-Length` is over the limit gets a `413` before any of it is
read; a chunked body gets one as soon as it passes the limit. Decoded JSON
takes several times the size of its text in memory, and this happens before
your controller runs, so raise the limit only on the routes that need it, for
example
`{pre_request, nova_request_plugin, #{decode_json_body => true, max_body_length => 10000000}}`
in that route group's `plugins`. `max_body_length => infinity` reads bodies of
any size.

Reading the body also has a deadline, `body_read_timeout` (default `60000`
milliseconds). A body that has not fully arrived by then, such as one sent a
byte at a time, gets a `408`. `infinity` turns the deadline off. Both the
`413` and the `408` have no body and close the connection, since the rest of
the request body is still on it.

`max_body_length` takes a non-negative integer and `body_read_timeout` a
positive one, or `infinity`. Any other value, including a `body_read_timeout`
of `0`, is replaced by the default, with one warning per value.

`decode_json_body` decodes `application/json` and any `application` type with
the `+json` suffix (RFC 6839), such as `application/vnd.api+json` or
`application/merge-patch+json`. The match ignores case and parameters such as
`charset`. Types that only start with `application/json`, such as
`application/json-seq`, are not decoded: decode them in the controller from the
request body, or with a plugin of your own. `read_urlencoded_body` needs
`application/x-www-form-urlencoded`.

|Option|Description|Req|
|------|-----------|----|
|decode_json_body|Decode a JSON body. The body of a `GET` or `DELETE` request is not decoded.| `Req#{json => Term}`|
|read_urlencoded_body|Decode an `application/x-www-form-urlencoded` body.| `Req#{params => Map}`|
|parse_qs|Parse the query string. `true` gives a map, `list` a list of `{Key, Value}`.|`Req#{parsed_qs => Map or List}`|
|max_body_length|Bytes of body read for `decode_json_body` and `read_urlencoded_body`, or `infinity`. Default `1000000`. A longer body gets a `413`.| |
|body_read_timeout|Milliseconds allowed for reading that body (at least `1`), or `infinity`. Default `60000`. A slower body gets a `408`.| |

### Nova multipart

`nova_request_plugin` never buffers a `multipart/form-data` body. Add
`nova_multipart_plugin` to the routes that take uploads and give it a handler
that receives every file part chunk by chunk:

```erlang
#{prefix => "/files",
  plugins => [
    {pre_request, nova_multipart_plugin, #{handler => {nova_multipart_file_handler, #{dir => ~"/var/uploads"}}}}
  ],
  routes => [
    {"/upload", fun upload_controller:upload/1, #{methods => [post]}}
  ]
}
```

The same tuple works in the global `plugins` list in `sys.config` when every
route may take an upload.

```erlang
-module(upload_controller).
-export([upload/1]).

upload(#{params := #{~"title" := Title}, files := Files}) ->
    Paths = [Path || #{result := #{path := Path}} <- Files],
    [ok = nova_multipart_spool:keep(Path) || Path <- Paths],
    {json, 200, #{}, #{title => Title, uploaded => Paths}}.
```

A file spooled by `nova_multipart_file_handler` belongs to the request and is
deleted when the request is over: after the response, when a plugin or the
security callback refused the request, when the controller crashed, and when
the client went away in the middle of a part. A controller that wants a file
to stay calls `nova_multipart_spool:keep/1` with its path, as above, or moves
it elsewhere with `file:rename/2`. Releases up to 0.18.3 left every file in
`dir`, also those of refused requests.

Regular form fields end up in `params`. Every part with a filename ends up in
`files` as a map with `name`, `filename`, `content_type` and `result`, where
`result` is what the handler returned. On a non-multipart request `files` is
`[]` and `params` is `#{}` unless another plugin set it, so the controller
above answers a JSON request with an empty upload list rather than a 500.

Two handlers ship with Nova:

|Handler|Result|
|-------|------|
|`nova_multipart_file_handler`|Streams the part to a random name under `dir` and returns `#{path => Path}`. Pass `extensions => [<<"png">>, <<"jpg">>]` to keep a listed extension from the client filename, lowercased, on the stored name; any other extension is dropped, never rejected. No list means no extension.|
|`nova_multipart_memory_handler`|Keeps the part in memory and returns `#{body => Binary}`. Takes no init args: `{nova_multipart_memory_handler, #{}}`. For small attachments only.|

Write your own by implementing the `nova_multipart_handler` behaviour
(`init/2`, `handle_data/2`, `handle_end/1`, `handle_abort/2`). A handler that
writes files should register them with `nova_multipart_spool`, so they are
deleted with the request and count towards the spool bounds. `filename`,
`content_type` and `name` are all client controlled: never build a filesystem
path from `filename`, and never echo `content_type` back as a response header.
The `extensions` allowlist exists because `dir` may be served by a web server,
where a stored `.html` or `.svg` is stored XSS. An extension says nothing about
the bytes; check those in the controller if the type matters.

A part without a filename is a regular field no matter how large it is, and is
bounded by `max_field_size`. When a form is protected by `nova_csrf_plugin`,
`nova_multipart_plugin` must run before it so the token is in `params`.

|Option|Description|
|------|-----------|
|handler|Required. `{Mod, InitArgs}` implementing `nova_multipart_handler`.|
|max_parts|Reply 413 when the body has more than this many parts (default 32).|
|max_part_size|Reply 413 past this many bytes in a single file part (default 8 000 000).|
|max_field_size|Reply 413 past this many bytes in a single non-file field (default 65 536).|
|max_total_size|Reply 413 past this many bytes across all parts (default 64 000 000).|
|read_timeout|Reply 408 if the body is not fully read within this many ms (default 60 000).|
|chunk_timeout|Reply 408 if the next 64 000 bytes of the body, or its end, do not arrive within this many ms (default 15 000).|

A malformed part gets a 400 and a handler error a 500. In every case the
controller is never called and the handler's `handle_abort/2` has run.

The two timeouts work together. `read_timeout` bounds the whole upload,
however fast it arrives. `chunk_timeout` ends an upload that stalls or
trickles before that: with the defaults a client has to send at least 64 000
bytes every 15 seconds, about 4.3 kB a second, until the body ends. The
handler gets the data 64 000 bytes at a time, and what is left when the body
ends. A `408` sent over HTTP/1.0 or HTTP/1.1 carries `connection: close`,
since the rest of the body is not read.

These limits hold for one request. Nova does not share them out between
clients: several clients, or one client with several connections, can each
send a body up to `max_total_size` for up to `read_timeout`, and together
fill the spool described below. Limit the number of concurrent uploads per
client in the proxy in front of the node if that matters.

The files and bytes spooled at one time, over all requests being served, are
bounded as well. Set the bounds under `multipart_spool` in the `nova`
environment:

```erlang
{nova, [{multipart_spool, #{max_files => infinity, max_bytes => 8000000000}}]}
```

|Key|Description|
|---|-----------|
|max_files|Files spooled at one time (default `infinity`), or an integer.|
|max_bytes|Bytes spooled at one time (default 8 000 000 000), or `infinity`.|

A part that would pass a bound gets a 503. Files a controller kept do not
count. With the default part limits a request spools at most 32 files and
64 MB, so `max_bytes` leaves room for 125 uploads of the largest size at
once; raise it together with `max_part_size` and `max_total_size` when
single uploads are larger. The number of files is not bounded by default: a
request spools at most `max_parts` of them, and a count across the node is
one that a few slow uploads can use up.

The upload is read once the security callback of the route has let the
request through. A request the callback refuses is answered at once and
nothing of its body is read or spooled. On a route without a security
callback, and on one that lists `nova_multipart_plugin` under
`plugins_before_security`, any client can spool up to the limits above. See
[Before and after the security callback](#before-and-after-the-security-callback).

To read the parts yourself, leave `nova_multipart_plugin` off the route and use
`nova_req:read_part/1`
from the controller, handing the updated request back through a five element
return tuple:

```erlang
upload(Req) ->
    {Files, Req1} = read_parts(Req, []),
    {json, 200, #{}, #{uploaded => length(Files)}, Req1}.

read_parts(Req0, Acc) ->
    case nova_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {ok, Body, Req2} = nova_req:read_part_body(Req1),
            read_parts(Req2, [{cow_multipart:form_data(Headers), Body}|Acc]);
        {done, Req1} ->
            {lists:reverse(Acc), Req1}
    end.
```
