# Handlers

A handler is the function that turns the value a controller returned into a
response. The first element of the returned tuple names the handler:
`{json, #{hello => world}}` is handled by the handler registered as `json`.

## Built-in handlers

| Name | Returned by a controller as | Response |
|---|---|---|
| `json` | `{json, JSON}`, `{json, StatusCode, Headers, JSON}` | The term encoded as JSON |
| `ok`, `view` | `{ok, Variables}`, `{ok, Variables, Options}` | A rendered ErlyDTL view |
| `status` | `{status, StatusCode}`, `{status, StatusCode, Headers}`, `{status, StatusCode, Headers, Body}` | A status code, with optional headers and body |
| `redirect` | `{redirect, Location}` | A `302` to any location |
| `redirect_local` | `{redirect_local, Path}` | A `302` to a path on the same site |
| `sendfile` | `{sendfile, StatusCode, Headers, {Offset, Length, Path}, Mime}` | A file |

The [Controllers](controllers.md#return-values) guide describes each form.
They are implemented in the module `nova_basic_handler`.

## Writing a handler

A handler is a function of three arguments: the value the controller
returned, the controller function, and the request. It sets the response on
the request and returns `{ok, Req}`:

```erlang
-module(my_app_text_handler).
-export([handle_text/3]).

handle_text({text, Body}, _Callback, Req) ->
    Req1 = cowboy_req:set_resp_headers(#{~"content-type" => ~"text/plain"}, Req),
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {ok, Req2#{resp_status_code => 200}}.
```

Nova sends the response after the handler returns. It uses the headers and the
body set on the request and the status code under `resp_status_code`, or
`200` when the key is not there. Do not call `cowboy_req:reply/2,3,4` in a
handler: the response would be sent twice.

`{ok, Req}` is the only return value. Anything else, and an exception, is
handled as a crash of the controller and answered with a `500`.

The `cowboy_req` functions are described in the
[Cowboy manual](https://ninenines.eu/docs/en/cowboy/2.21/manual/cowboy_req/).

## Registering a handler

Register the handler when your application starts:

```erlang
start(_StartType, _StartArgs) ->
    ok = nova_handlers:register_handler(text, fun my_app_text_handler:handle_text/3),
    my_app_sup:start_link().
```

`{my_app_text_handler, handle_text}` is accepted in place of the fun. A
controller can now return `{text, ~"plain words"}`.

A name can be registered once. Registering a name that is taken, a built-in
one included, logs `Another handler is already registered on that name` and
keeps the handler that was there. To replace a handler, remove it first with
`nova_handlers:unregister_handler/1`.

## Long-lived responses

A handler can keep the connection and stream the response, for
server-sent events for instance, with `cowboy_req:stream_reply/3` and
`cowboy_req:stream_body/3`. Cowboy closes a connection that has been idle for
`idle_timeout` (60 seconds by default), and only data it *receives* resets
that timer. A response that just keeps sending is cut when the timeout
passes.

Give such a response a timeout of its own before streaming, rather than
raising the limit for every request on the listener:

```erlang
handle_events({events, Source}, _Callback, Req0) ->
    cowboy_req:cast({set_options, #{idle_timeout => infinity}}, Req0),
    Req = cowboy_req:stream_reply(200, #{~"content-type" => ~"text/event-stream"}, Req0),
    stream(Source, Req).
```

The option lasts for that response only. It works for HTTP/1.1. HTTP/2 has
one idle timer per connection and ignores it; set
`reset_idle_timeout_on_send => true` in the `options` of
`cowboy_configuration` there. A stream with no idle timeout is only ended by
the client leaving, which stops the request process, so bound its lifetime
yourself.

Nova sends the response when a handler returns. A handler that streamed the
response itself therefore has to stay in the function for as long as the
stream lasts. Returning `{ok, Req}` after the response was sent makes Nova
attempt a second response, which is logged as `Controller crashed` with the
reason `response_already_sent`.
