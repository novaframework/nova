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
  1. nova_router            find the route
  2. nova_plugin_handler    pre_request plugins
  3. nova_security_handler  security callback, then the plugins that wait for it
  4. nova_handler           controller, then the handler for what it returned
  5. nova_plugin_handler    post_request plugins
```

A step that answers the request ends it, and the steps after it do not run.

1. **Stream handlers.** `nova_stream_h` sets the `server` response header to
   `Cowboy/Nova` and issues the session cookie when the request wrote to the
   session. `cowboy_compress_h` compresses responses.
2. **Routing.** `nova_router` looks up the host, the path and the method in
   the routing table. The path is matched after empty segments are dropped
   and `.` and `..` are resolved. Without a match the request gets the `404`
   page, or `405` when the path exists for another method, and no plugin
   runs.
3. **`pre_request` plugins**, in the order they are listed. A plugin can
   change the request or answer it.
4. **The security callback** of the route group, if it has one. A request it
   refuses is answered with `401` or with what the callback returned. The
   plugins listed in `plugins_after_security` run after it.
5. **The controller.** Its return value is handed to the handler registered
   for it, which sets the response on the request. Nova then sends the
   response. For a route with `protocol => ws` the connection is upgraded
   here instead.
6. **`post_request` plugins.** The response has been sent by then.

The [Plugins](plugins.md#request-pipeline) guide has the consequences of
this order.

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
| `auth_data` | Security callback | The data of `{true, AuthData}` |
| `json` | `nova_request_plugin` | The decoded JSON body, with `decode_json_body` |
| `params` | `nova_request_plugin`, `nova_multipart_plugin` | The fields of a urlencoded or multipart body, a map with binary keys |
| `parsed_qs` | `nova_request_plugin` | The query string, a map or a list, with `parse_qs` |
| `files` | `nova_multipart_plugin` | The uploaded files |
| `csrf_token` | `nova_csrf_plugin` | The CSRF token of the session |
| `correlation_id` | `nova_correlation_plugin` | The correlation id of the request |
| `crash_info` | Nova, for a status code route | What is known about the failure the page is rendered for |
| `resp_status_code` | Handlers | The status code of the response. Read by `post_request` plugins and by Nova when it sends the response |

Keys that start with `nova_session` and `nova_dispatch_key` are internal.
Use `nova_session` and `nova_router:dispatch_key/1` to get at what they
hold.

A plugin can add keys of its own. Use a name that will not clash, such as the
name of your application.

## Websockets

An upgrade passes steps 1 to 4 as any request. In step 5 `nova_ws_handler`
calls `init/1` of the websocket module and upgrades the connection. After
that, every frame and every message runs the `pre_ws_request` plugins, the
callback of the module and the `post_ws_request` plugins. See
[Websockets](websockets.md).
