# Error handling

This guide describes what a client gets when a request fails, and how to
change it.

## What is answered with what

| Situation | Response | Logged as |
|---|---|---|
| The path is not canonical, as `/a//b` and `/a/../b` are not | `400`, through the `400` page if the application has one. See [Routing](routing.md#paths-that-are-not-canonical) | `Request path is not canonical`, at debug level |
| No route matches the path | The `404` page | |
| The path has routes, but none for the method | `405` with an `allow` header, through the `405` page if the application has one | |
| A controller throws `{StatusCode, Reason}` | The page for that status code | `Controller threw an exception` |
| A controller crashes | The `500` page | `Controller crashed` |
| A controller returns something no handler knows | The fallback controller of the module if it has one, otherwise the `500` page | |
| A view names a template that does not exist | The `500` page. In `dev` the page names the template | `Nova could not render template` |
| A controller or a handler fails after the response has begun | What was sent already. No page | The failure, and a warning that no page is sent |
| A request that was answered already is answered again, for instance by a controller that replied with `cowboy_req:reply/4` and returned without the request that call gave back | The response that was sent first. The second one is dropped, and the connection stays open | `The request was answered already, so this response is not sent ...`, at most once per request and once a second on the node |
| A `pre_request` plugin crashes or returns `{error, Reason}`, in `pre_security/4` or in `pre_request/4` | The `500` page | `Plugin crashed`, or `Plugin returned an error` for `{error, Reason}` |
| A request to a websocket route comes from an origin that is not allowed | `403` with the text `Origin not allowed`. No page is rendered | |
| `nova_csrf_plugin` refuses a request | `403` with the text `Forbidden - request from another origin` or `Forbidden - CSRF token invalid`. No page is rendered | |
| The security callback crashes | The `500` page | `Security handler crashed` |
| The security callback refuses the request | `401`, or what the callback returned. No page is rendered | |
| The client sent something Cowboy cannot parse, or sent the body too slowly | `400`, `408` or `413` | A warning |

A page is the response of the status code route for that code. Nova has
routes for `404` and `500`. For other codes without a route the response is
the bare status code.

`{status, StatusCode}` returned by a controller is sent as it is. It does not
go through a page.

## Throwing a status

A controller can end a request anywhere below it in the call stack by
throwing `{StatusCode, Reason}`:

```erlang
show(#{bindings := #{~"id" := Id}}) ->
    case my_app_items:find(Id) of
        {ok, Item} -> {json, Item};
        not_found -> throw({404, no_such_item})
    end.
```

`Reason` is logged and is available to the page under `crash_info` in the
request.

## The default pages

The pages that come with Nova answer in the form the `accept` header of the
request asks for:

| `accept` header | Response body |
|---|---|
| Not sent, or cannot be parsed | A JSON object |
| Prefers JSON | A JSON object |
| Prefers HTML | An HTML page |
| Admits neither | Empty, and no `content-type` header |

`application/json`, `application/*` and `*/*` stand for JSON, and
`text/html`, `text/*` and `*/*` for HTML. The form with the higher quality
value wins, and `q=0` rules a form out. Of two forms with the same quality
the one the header names first wins, and JSON when one range covers both, as
`*/*` does. A client that sends `*/*` gets JSON, and a browser, which names
`text/html` first, gets the HTML page.

The `500` page has a title and a generic message, in every environment. The
class and the reason of the crash and the stacktrace are only added when
`environment` is set to `dev` under `nova`. An environment that is not set
is read as `prod`. The stacktrace also needs
`{use_stacktrace, true}`. Without it, Nova logs a hint about the setting
with the first such page in `dev`, once per node.

`{render_error_pages, false}` under `nova` turns the pages that come with
Nova off. It is read when one of those pages is rendered and nowhere else, so
the status code routes of your application run either way:

| Situation | `true`, the default | `false` |
|---|---|---|
| The application has a route for the status code | The route runs, for every client | The route runs, for every client |
| A `404` or a `500` without such a route: no route matches, a controller throws `{404, Reason}`, a controller crashes or returns something no handler knows, a plugin or a security callback crashes | Nova's page, in the form the `accept` header asks for | The bare status code for every client, without a body and without a `content-type` |
| Any other status code without a route, such as a `405`, a thrown `{403, Reason}`, a request the security callback refuses, or a `400`, `408` or `413` | The bare status code | The bare status code |
| A page of the application crashes | The `500` page, Nova's own when the application's `500` page is the one that crashed, and a bare `500` if that fails too | A bare `500` |

## Your own pages

A route whose path is an integer is the page for that status code. Declare
them in a route group without a security callback:

```erlang
#{routes => [
    {404, fun my_app_error_controller:not_found/1, #{}},
    {500, fun my_app_error_controller:server_error/1, #{}}
]}
```

```erlang
-module(my_app_error_controller).
-export([not_found/1, server_error/1]).

not_found(_Req) ->
    {status, 404, #{~"content-type" => ~"text/plain"}, ~"Nothing here"}.

server_error(_Req) ->
    {status, 500, #{~"content-type" => ~"text/plain"}, ~"Something went wrong"}.
```

A page is a controller function and returns what a controller returns. Your
routes replace the pages that come with Nova. For a `500`, the request has
what is known about the failure under `crash_info`, after
[redaction](logging.md#redaction). Do not send it to the client.

The request of a page always has `resp_status_code`, and `controller_data`
with `#{status => StatusCode, data => Data}`. `Data` and `crash_info` depend
on how the page was reached:

| Reached from | `Data` | `crash_info` |
|---|---|---|
| A path that is not canonical (`400`) | `#{error => "Path is not canonical"}` | No |
| A request no route matches (`404`) | `#{error => "Not found in path"}` | No |
| A request no method matches (`405`) | `#{error => "Method not allowed"}` | No |
| A crash in a plugin or in a security callback | `#{}` | Yes |
| A controller that crashed, threw `{StatusCode, Reason}` or returned something no handler knows | `#{}` | Yes |
| A request `cowboy_req` could not parse in a controller (`400`, `408`, `413`) | `#{}` | No |
| Your own call of `nova_router:render_status_page/2,3,5` | The map it was given, `#{}` with `render_status_page/2` | No |

```erlang
not_found(#{resp_status_code := Status, controller_data := #{data := Data}}) ->
    Reason = maps:get(error, Data, "Unknown"),
    {status, Status, #{~"content-type" => ~"text/plain"}, unicode:characters_to_binary(Reason)}.
```

A page that crashes is not rendered again: the request gets the `500` page,
Nova's own if it was your `500` page that failed, and a `500` without a body
if that fails too.

See [Routing](routing.md#status-code-routes) for when a page is rendered.

## Fallback controllers

A controller that returns something no handler knows, such as
`{error, not_found}` from a function it called, can leave the answer to a
fallback controller. See
[Controllers](controllers.md#fallback-controllers).

## Errors in plugins and security callbacks

A plugin answers a request itself with `{stop, Reply, Req, State}`, for
instance `{stop, {reply, 503, #{~"retry-after" => ~"60"}, ~"Down for maintenance"}, Req, State}`.
A security callback answers with `{false, StatusCode, Headers, Body}`. Neither
goes through a page. See [Plugins](plugins.md#return-values) and
[Routing](routing.md#the-security-function-callback).

## Finding the cause

Every failure in the table above is logged with the class and the reason,
after redaction. Set `{use_stacktrace, true}` under `nova` to add the
stacktrace to the log. The [Logging](logging.md) guide lists the log messages
and what they mean.
