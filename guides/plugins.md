# Plugins

Plugins are a bit like the handlers except they are run on request. There's currently two different type of plugins; `pre_request` and `post_request`.
These can be used to create access logs, insert CORS headers or similar.

Plugins are used to handle things before and/or after a request. They are applied on all requests of a specified protocol.

A rule of thumb is that if you want to store a short-lived state (For the local request) then you could store that under the Env-object. If you want to store a long-lived state (For the global request) then you could store that under the State-object which is initialized in the optional `init/0` callback and (potentially) updated in each call to `pre_request/4` and `post_request/4`. The long-lived state is only applicable to the plugin-module while the local state belongs to the request and is distributed to each invoked plugin.

This is an example:

```erlang
-module(correlation_id).
-behaviour(nova_plugin).
-export([
    pre_request/4,
    post_request/4,
    plugin_info/0
  ]).

pre_request(Req, _Env, _Opts, State) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    {ok, cowboy_req:set_resp_header(<<"x-correlation-id">>, UUID, Req), State}.

post_request(Req, _Env, _Opts, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{
      title => <<"nova_cors_plugin">>,
      version => <<"0.2.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Add CORS headers to request">>,
      options => [
                 ]
     }.
```

This plugin injects a UUID into the headers.

## Return values

`pre_request/4` and `post_request/4` return one of:

| Return | Effect |
|--------|--------|
| `{ok, Req, State}` | Carry on with the next plugin. |
| `{ok, Reply, Req, State}` | Set the reply's status, headers and body on `Req` and carry on. Only `{reply, ...}` is accepted here; see below. |
| `{stop, Req, State}` | End the request here. The plugin has replied itself, for example with `cowboy_req:reply/4`. |
| `{stop, Req}` | Deprecated form of `{stop, Req, State}` that keeps the plugin's state, removed in 1.0. |
| `{stop, Reply, Req, State}` | End the request and send `Reply`. |
| `{break, Req, State}`, `{break, Reply, Req, State}` | Deprecated aliases of the matching `stop` returns, removed in 1.0. They log a warning, once per plugin module. |
| `{error, Reason}` | End the request with the 500 status page. |

`Reply` is one of `{reply, Body}`, `{reply, Status, Body}`, `{reply, Status, Headers, Body}`, `{redirect, Url}`, `{status, Status}` or `{status, Status, Headers}`. `Headers` is a map or a list of `{Name, Value}` pairs. `{status, ...}` sends the bare status without rendering a status-code route. Any other reply renders the 500 status page.

With `ok` the controller still runs and answers, so a redirect or a bare status set there would be overwritten. `{ok, {redirect, ...}, Req, State}` and `{ok, {status, ...}, Req, State}` therefore render the 500 status page and log that `stop` is needed. To send them, return `{stop, Reply, Req, State}`:

```erlang
pre_request(Req, _Env, _Options, State) ->
    case nova_session:get(Req, <<"user_id">>) of
        {ok, _} -> {ok, Req, State};
        {error, _} -> {stop, {redirect, "/login"}, Req, State}
    end.
```

Once a `pre_request` plugin stops the request or the security function refuses it, the controller and the `post_request` plugins do not run. Keep access logging and similar work that has to see every request in a stream handler or in a `pre_request` plugin.

The state a plugin returns is stored per plugin module, not per request, and is shared by every request. Nova only writes it when it changed, and concurrent requests overwrite each other's writes: the last writer wins. Keep per-request data in `Req`, and keep shared data that has to add up (counters, rate-limit buckets) in `ets` or `counters`.

## Running a plugin after the security callback

Every `pre_request` plugin runs before the route's security callback. A plugin that reads the request body, such as `nova_request_plugin` or `nova_multipart_plugin`, therefore reads, decodes and (for uploads) spools what an unauthenticated client sent. List the plugin modules that should wait for the security callback under `plugins_after_security`, in the route group:

```erlang
#{prefix => "/files",
  security => fun my_auth:check/1,
  plugins => [{pre_request, nova_cors_plugin, #{allow_origins => [<<"https://app.example.com">>]}},
              {pre_request, nova_multipart_plugin, #{handler => {nova_multipart_file_handler, #{dir => <<"/var/uploads">>}}}}],
  plugins_after_security => [nova_multipart_plugin],
  routes => [{"/upload", fun upload_controller:upload/1, #{methods => [post]}}]}
```

or for every route group that does not set the key, in `sys.config`:

```erlang
{nova, [{plugins_after_security, [nova_request_plugin, nova_multipart_plugin]}]}
```

The default is `[]`, which is the order Nova has always had. A listed plugin keeps its options and its place among the other listed plugins, and runs once the security callback has let the request through. It does not run for a request the callback refuses, nor after an earlier plugin failed. Plugins that are not listed still run first, so CORS preflights are still answered before the security callback.

The security callback of such a route cannot read what a listed plugin produces: `json`, `params`, `parsed_qs` and `files` are not in `Req` yet. A callback that needs the body, for instance to check a signature over it, has to keep the body-reading plugin out of the list. A plugin that depends on a listed one, such as `nova_csrf_plugin` reading the token from a multipart form, has to be listed too, after it.

A websocket route runs its `pre_request` plugins on the upgrade request. The plugin phases `nova_ws_handler` has for the time after the upgrade cannot be used yet. See the `nova_websocket` documentation.


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

There's two optional callbacks for a plugin that can be used for storing a global long-lived state. This can be useful if you want to keep track of something like a `pid` or similar.
The first callback is `init/0` and the second is `stop/1`. `init/0` is called when the plugin is loaded and `stop/1` is called when the plugin is unloaded (Usually when the nova-application is started/stopped).
Following example shows how we spawn a process and returns the pid in a map - this map will become our new state. When the plugin is stopped, `stop/1` will get invoked with the plugin's state and we will send a stop message to the pid. (Older releases called a `stop/0` instead; it is still called when `stop/1` is not exported.)

```erlang
init() ->
    %% Setup some pids
    Pid = spawn(fun() -> ok end),
    #{my_pid => Pid}.

stop(State) ->
    %% Stop the pids
    Pid = maps:get(my_pid, State),
    Pid ! stop,
    ok.
```

Adding a plugin

Example:
A good example of a very useful plugin is the `nova_request_plugin`. When we are developing a HTTP web api using json as the data format, we need the framework to
decode our message so that we can process it. To do that we need to add `decode_json_body => true` into the options field in our `sys.config`.


**sys.config**


 ```erlang
     {nova, [
         {environment, dev},
         {cowboy_configuration, #{
                                  port => 8080
                                 }},
         {bootstrap_application, chatapp},
         {plugins, [
                    {pre_request, nova_request_plugin, #{parse_bindings => true,
                                                         decode_json_body => true}}
                   ]}
        ]}
 ```
 We have added our plugin in the `plugins` section. As we can see this is a `pre_request` plugin since it processes and decodes the message to json format
 before we can actually use it in our nova application endpoints.

Usage:

**controller**

```erlang
-module(test_controller).
-export([increment/1]).

increment(#{<<"json">> := #{<<"id">> := Id, <<"value">> := Value}})->
    {json,200,#{},#{<<"id">> => Id , <<"received">> => Value, <<"increment">> => Value+1}}.

```
## Nova plugins

Nova has a couple of plugins for some general purposes.

|Plugin|Description|Code|
|------|-----------|----|
|nova_correlation_plugin|This plugin will add a correlation id to header response but also add `#{correlation_id => CorrelationID}` to the request obj that is passed to the controller.|[nova_correlation_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_correlation_plugin.erl)|
|nova_cors_plugin|This plugin will handle cors and add the cors headers into the request.|[nova_cors_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_cors_plugin.erl)|
|nova_multipart_plugin|This plugin reads `multipart/form-data` bodies and streams uploaded files to a handler.|[nova_multipart_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_multipart_plugin.erl)|
|nova_request_plugin|This plugin will handle incomming data like qs, form urlencoded and json. Multipart bodies are left to `nova_multipart_plugin`.|[nova_request_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_request_plugin.erl)|
|nova_ws_origin_plugin|This plugin refuses a websocket upgrade that a page on another origin started. Options are described in the module.|[nova_ws_origin_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_ws_origin_plugin.erl)|


### Nova correlation

This plugin will generate a uuid v4 and set it as a response header as `X-Correlation-ID` if nothing is configuered.

```erlang
{pre_request, nova_correlation_plugin, #{request_correlation_header => CorrelationHeader,
                                         logger_metadata_key => LoggerMetaDataKey}}
```

|Option|Description|
|------|-----------|
|request_correlation_header|This is if you want a different correlation header than the standard `X-Correlation-ID`|
|logger_metadata_key| This is if you want to have a different metadata key then the standard `correaltion_id`|

A correlation ID from the request is used when it is 1 to 255 visible ASCII characters. A value with spaces, control characters or other bytes, or a longer one, is ignored and a new ID is generated, because the ID is copied to the logger metadata and to the response.

### Nova cors

This plugin answers CORS preflights (an `OPTIONS` request with both `Origin` and `Access-Control-Request-Method`) itself with a `204` and the CORS headers, so the controller never sees them. Any other request, including a plain `OPTIONS`, goes on to the application with the CORS headers added to the response.

The router picks the route, and checks its `methods`, before any plugin runs. A preflight therefore only reaches the plugin on a route that accepts `OPTIONS`: leave `methods` out, or list `options` in it. A route whose `methods` do not include `options` answers the preflight with a `405` and no CORS headers, and the browser blocks the real request.

```erlang
{pre_request, nova_cors_plugin, #{allow_origins => [<<"https://app.example.com">>,
                                                    <<"https://admin.example.com">>]}}
```

With a list (or a single origin), the request's `Origin` is echoed back in `access-control-allow-origin` only when it is on the list, and every response carries `vary: origin` (added to any `vary` value already set) so caches keep the answers apart. `<<"*">>` allows any origin.

By default a preflight is told that every header it asks for is allowed. That is only safe because `access-control-allow-origin` is sent to allowed origins alone: list the origins you trust, and set `allow_headers` explicitly when you use `<<"*">>`.

|Option|Description|
|------|-----------|
|allow_origins|Required. `<<"*">>`, an origin, or a list of origins.|
|allow_headers|Request headers a preflight allows, as a binary or a list. Defaults to the headers the preflight asks for in `access-control-request-headers`; a literal `*` would not cover `authorization`.|
|allow_methods|Methods a preflight allows, as a binary or a list. Defaults to `<<"*">>`.|
|max_age|Seconds a browser may cache the preflight answer.|

### Nova request

This plugins handle incoming data and can transform them to erlang maps depending on what the options are.

```erlang
{pre_request, nova_request_plugin, #{decode_json_body => true,
                                     read_urlencoded_body => true,
                                     parse_qs => true|list}}
```

An empty or malformed JSON body is answered with a bare `400` (no body, no status page).

The body is read only when `decode_json_body` or `read_urlencoded_body` is set,
and at most `max_body_length` bytes of it (default `1000000`, 1 MB). A body
whose `Content-Length` is over the limit gets a `413` before any of it is
read; a chunked body gets one as soon as it passes the limit. Decoded JSON
takes several times the size of its text in memory, and this happens before
your controller or security callback runs, so raise the limit only on the
routes that need it, for example
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
|decode_json_body|If header is application/json it will decode the body.| `Req#{json => Map}`|
|read_urlencoded_body|If header is application/x-www-form-urlencoded it will decode it.| `Req#{params => Map}`|
|parse_qs| If the path have qs in it we will get them.|`Req#{parsed_qs => Map or List}`|
|max_body_length|Bytes of body read for `decode_json_body` and `read_urlencoded_body`, or `infinity`. Default `1000000`. A longer body gets a `413`.| |
|body_read_timeout|Milliseconds allowed for reading that body (at least `1`), or `infinity`. Default `60000`. A slower body gets a `408`.| |

### Nova multipart

`nova_request_plugin` never buffers a `multipart/form-data` body. Add
`nova_multipart_plugin` to the routes that take uploads and give it a handler
that receives every file part chunk by chunk:

```erlang
#{prefix => "/files",
  plugins => [
    {pre_request, nova_multipart_plugin, #{handler => {nova_multipart_file_handler, #{dir => <<"/var/uploads">>}}}}
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

upload(#{params := #{<<"title">> := Title}, files := Files}) ->
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
|read_timeout|Reply 408 if the whole body is not read within this many ms (default 60 000).|

A malformed part gets a 400 and a handler error a 500. In every case the
controller is never called and the handler's `handle_abort/2` has run.

The files and bytes spooled at one time, over all requests being served, are
bounded as well. Set the bounds under `multipart_spool` in the `nova`
environment:

```erlang
{nova, [{multipart_spool, #{max_files => 1024, max_bytes => 8000000000}}]}
```

|Key|Description|
|---|-----------|
|max_files|Files spooled at one time (default 1024), or `infinity`.|
|max_bytes|Bytes spooled at one time (default 8 000 000 000), or `infinity`.|

A part that would pass a bound gets a 503. Files a controller kept do not
count. With the default part limits a request spools at most 32 files and
64 MB, so the defaults leave room for at least 32 uploads at once; raise them
together with `max_part_size` and `max_total_size` when single uploads are
larger than the bound.

By default the upload is read before the route's security callback runs. See
[Running a plugin after the security callback](#running-a-plugin-after-the-security-callback)
to read it only for requests that were let through.

To read the parts yourself, leave `nova_multipart_plugin` off the route and use
[cowboy_req:read_part/1](https://ninenines.eu/docs/en/cowboy/2.13/manual/cowboy_req.read_part/)
from the controller, handing the updated request back through a five element
return tuple:

```erlang
upload(Req) ->
    {Files, Req1} = read_parts(Req, []),
    {json, 200, #{}, #{uploaded => length(Files)}, Req1}.

read_parts(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
            read_parts(Req2, [{cow_multipart:form_data(Headers), Body}|Acc]);
        {done, Req1} ->
            {lists:reverse(Acc), Req1}
    end.
```
