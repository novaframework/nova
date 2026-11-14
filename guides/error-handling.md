# Error handling

This guide describes what a client gets when a request fails, and how to
change it.

## What is answered with what

| Situation | Response | Logged as |
|---|---|---|
| No route matches the path | The `404` page | |
| The path has routes, but none for the method | `405` with an `allow` header, through the `405` page if the application has one | |
| A controller throws `{StatusCode, Reason}` | The page for that status code | `Controller threw an exception` |
| A controller crashes | The `500` page | `Controller crashed` |
| A controller returns something no handler knows | The fallback controller of the module if it has one, otherwise the `500` page | |
| A controller or a handler fails after the response has begun | What was sent already. No page | The failure, and a warning that no page is sent |
| A controller answers the request itself and returns without the request it answered with | The response the controller sent. What it returned is not sent | A warning that names the controller |
| A `pre_request` plugin crashes or returns `{error, Reason}` | The `500` page | `Plugin crashed` |
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
| Admits neither | Empty |

`application/json`, `application/*` and `*/*` stand for JSON, and
`text/html`, `text/*` and `*/*` for HTML. The form with the higher quality
value wins, and `q=0` rules a form out. Of two forms with the same quality
the one the header names first wins, and JSON when one range covers both, as
`*/*` does. A client that sends `*/*` gets JSON, and a browser, which names
`text/html` first, gets the HTML page.

The `500` page has a title and a generic message, in every environment. The
class and the reason of the crash and the stacktrace are only added when
`environment` is set to `dev` under `nova`. An environment that is not set
does not count as `dev` here. The stacktrace also needs
`{use_stacktrace, true}`. Without it, Nova logs a hint about the setting
with the first such page in `dev`, once per node.

`{render_error_pages, false}` under `nova` has two effects:

* A controller that crashes or throws a status is answered with the bare
  status code. No page is looked up, neither Nova's nor a status code route
  of the application.
* Nova's `500` page, which a crash in a plugin or in a security callback is
  answered with, sends the bare status code where it would send the HTML
  page. Its JSON form is sent as before.

The `404` page for a request no route matches is not affected.

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
