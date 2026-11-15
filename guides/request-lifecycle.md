# Request lifecycle

Nova is a set of Cowboy stream handlers and middlewares. This guide follows a
request through them and lists what Nova adds to the request on the way.

## The steps

```
Client
  │
  ▼
Cowboy listener (nova_listener)
  │
  ▼
Stream handlers: nova_stream_h → cowboy_compress_h → cowboy_stream_h
  │
  ▼
Middlewares, in the request process:
  1. nova_router            refuse a path that is not canonical, find the route
  2. nova_plugin_handler    pre_security/4 of the pre_request plugins
  3. nova_security_handler  security callback, then pre_request/4 of the plugins
  4. nova_handler           controller, then the handler for what it returned
  5. nova_plugin_handler    post_request plugins
```

A step that answers the request ends it, and the steps after it do not run.

1. **Stream handlers.** `nova_stream_h` sets the `server` response header to
   `Cowboy/Nova` and issues the session cookie when the request wrote to the
   session. `cowboy_compress_h` compresses responses.
2. **Routing.** `nova_router` answers a path that is not canonical, such as
   `/a//b` or `/a/../b`, with `400`, see
   [Routing](routing.md#paths-that-are-not-canonical). It then looks up the
   host, the path and the method in the routing table. Without a match the
   request gets the `404` page, or `405` when the path exists for another
   method, and no plugin runs.
3. **`pre_security/4` of the `pre_request` plugins**, in the order they are
   listed. A plugin can change the request or answer it. Nothing of the body
   is read here. On a websocket route the origin check runs first.
4. **The security callback** of the route group, if it has one. A request it
   refuses is answered with `401` or with what the callback returned.
   **`pre_request/4` of the `pre_request` plugins** runs after it, in the
   order they are listed. This is where the body is read and decoded.
5. **The controller.** Its return value is handed to the handler registered
   for it, which sets the response on the request. Nova then sends the
   response. For a route with `protocol => ws` the connection is upgraded
   here instead.
6. **`post_request` plugins.** The response has been sent by then.

The [Plugins](plugins.md#request-pipeline) guide has the consequences of
this order, and the two route group keys that move a plugin to the other
side of the security callback.

## Terms

| Term | Meaning |
|---|---|
| Bootstrap application | Your application, named by `bootstrap_application`. Nova reads its router and starts the listener for it |
| Router | The module with the `routes/1` callback of an application |
| Route group | A map in the list `routes/1` returns: a prefix, a security callback, plugins and routes |
| Route | A path with the controller function, static file or websocket module that serves it |
| Controller | A function of the request that returns what to answer |
| Handler | The function that turns the value a controller returned into a response |
| Plugin | A module that runs before or after controllers |
| Security callback | A function of the request that decides if a route group may be called |
| Status code route | A route whose path is a status code. It renders the page for that code |
| Included application | Another Nova application that is served from the same listener, listed in `nova_apps` |

## The request map

Controllers, plugins and security callbacks get the Cowboy request, a map.
The `cowboy_req` functions read the method, the headers, the query string,
the cookies and the body from it. Nova adds these keys:

| Key | Set by | Value |
|---|---|---|
| `bindings` | Router | The path parameters, a map with binary keys: `#{~"id" => ~"42"}` |
| `extra_state` | Router | The `extra_state` of the route, or of its route group when the route has none, otherwise `undefined`. For a route ending in `[...]` it has the trailing segments under `pathinfo` |
| `nova_route` | Router | The template of the matched route, such as `<<"/users/:id">>`. Not set when no route matched |
| `plugins` | Router | The plugins of the route |
| `correlation_id` | `nova_correlation_plugin`, before the security callback | The correlation id of the request |
| `parsed_qs` | `nova_request_plugin`, before the security callback | The query string, a map or a list, with `parse_qs` |
| `auth_data` | Security callback | The data of `{true, AuthData}` |
| `json` | `nova_request_plugin`, after the security callback | The decoded JSON body, with `decode_json_body` |
| `body` | `nova_request_plugin`, after the security callback | The body as it was read, with `decode_json_body` or `read_urlencoded_body`. `<<>>` otherwise |
| `params` | `nova_request_plugin`, `nova_multipart_plugin`, after the security callback | The fields of a urlencoded or multipart body, a map with binary keys |
| `files` | `nova_multipart_plugin`, after the security callback | The uploaded files |
| `csrf_token` | `nova_csrf_plugin`, after the security callback | A CSRF token for the client, another one for every response |
| `crash_info` | Nova, for a status code route | What is known about the failure the page is rendered for |
| `controller_data` | Nova, for a status code route | `#{status => StatusCode, data => Data}`. See [Error handling](error-handling.md#your-own-pages) |
| `resp_status_code` | Handlers, and Nova for a status code route | The status code of the response. Read by status code routes, by `post_request` plugins and by Nova when it sends the response |

The other keys Nova adds are internal and can change with any release:
`nova_dispatch_key`, `nova_stream`, `nova_session_id`,
`nova_session_presented`, `nova_session_pending`, `nova_session_direct` and
`nova_session_owner`. Use `nova_session` and `nova_router:dispatch_key/1` to
get at what they hold.

A plugin can add keys of its own. Use a name that will not clash, such as the
name of your application.

## Websockets

An upgrade passes steps 1 to 4 as any request. In step 5 `nova_ws_handler`
calls `init/1` of the websocket module and upgrades the connection. After
that, every frame and every message runs the `pre_ws_request` plugins, the
callback of the module and the `post_ws_request` plugins. See
[Websockets](websockets.md).
