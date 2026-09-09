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


## Optional callbacks

There's two optional callbacks for a plugin that can be used for storing a global long-lived state. This can be useful if you want to keep track of something like a `pid` or similar.
The first callback is `init/0` and the second is `stop/1`. `init/0` is called when the plugin is loaded and `stop/1` is called when the plugin is unloaded (Usually when the nova-application is started/stopped).
Following example shows how we spawn a process and returns the pid in a map - this map will become our new state. When the plugin is stopped, `stop/1` will get invoked and we will send a stop message to the pid.

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
         {dev_mode, true},
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
|nova_request_plugin|This plugin will handle incomming data like qs, form urlencoded, multipart and json|[nova_request_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_request_plugin.erl)|


### Nova correlation

This plugin will generate a uuid v4 and set it as a response header as `X-Correlation-ID` if nothing is configuered.

```erlang
{pre_request; nova_correlation_plugin, #{request_correlation_header => CorrelationHeader,
                                         logger_metadata_key => LoggerMetaDataKey}}
```

|Option|Description|
|------|-----------|
|request_correlation_header|This is if you want a different correlation header than the standard `X-Correlation-ID`|
|logger_metadata_key| This is if you want to have a different metadata key then the standard `correaltion_id`|

### Nova cors

This plugins will make it so that if we get method OPTIONS it will just return back the CORS headers. In this case you don't need a controller to handle it and the plugin stops after this.
For other methods it will add the CORS headers to the request.

```erlang
{pre_request; nova_cors_plugin, #{allow_origins => <<"*">>}}
```

|Option|Description|
|------|-----------|
|allow_origins|Specifies which origins to insert into Access-Control-Allow-Origin|

### Nova request

This plugins handle incoming data and can transform them to erlang maps depending on what the options are.

```erlang
{pre_request; nova_correlation_plugin, #{decode_json_body => true,
                                         read_urlencoded_body => true,
                                         read_multipart_body => true,
                                         parse_qs => true|list}}
```


|Option|Description|Req|
|------|-----------|----|
|decode_json_body|If header is application/json it will decode the body.| `Req#{json => Map}`|
|read_urlencoded_body|If header is application/x-www-form-urlencoded it will decode it.| `Req#{params => Map}`|
|read_multipart_body|If header is multipart/form-data it will read all the parts.|`Req#{params => Map, files => List}`|
|parse_qs| If the path have qs in it we will get them.|`Req#{parsed_qs => Map or List}`|

#### File uploads

`read_multipart_body` reads a `multipart/form-data` body in one go. Regular form
fields end up in `params` and every part that carries a filename ends up in
`files`:

```erlang
{pre_request, nova_request_plugin, #{read_multipart_body => true}}
```

```erlang
-module(upload_controller).
-export([upload/1]).

upload(#{params := #{<<"title">> := Title}, files := Files}) ->
    [ok = file:write_file(<<"/tmp/", Filename/binary>>, Body)
     || #{filename := Filename, body := Body} <- Files],
    {json, 200, #{}, #{title => Title, uploaded => length(Files)}}.
```

Each entry in `files` is a map with the keys `name` (the form field), `filename`,
`content_type` and `body`.

Parts are kept in memory, so the size of a single part is capped at 8 MB. Pass a
map instead of `true` to change that - a part above the limit gets a `413` reply
and the controller is never called:

```erlang
{pre_request, nova_request_plugin, #{read_multipart_body => #{max_file_size => 20000000}}}
```

Without the `read_multipart_body` option the plugin leaves a `multipart/form-data`
body untouched even when `decode_json_body` is set, so a controller can stream the
parts itself with [cowboy_req:read_part/1](https://ninenines.eu/docs/en/cowboy/2.13/manual/cowboy_req.read_part/)
and hand the updated request back through a four element return tuple:

```erlang
upload(Req) ->
    {Files, Req1} = read_parts(Req, []),
    {json, 200, #{}, Req1, #{uploaded => length(Files)}}.

read_parts(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
            read_parts(Req2, [{cow_multipart:form_data(Headers), Body}|Acc]);
        {done, Req1} ->
            {lists:reverse(Acc), Req1}
    end.
```

#### Streaming large uploads with `nova_multipart_plugin`

`read_multipart_body` keeps a whole part in memory (bounded by `max_file_size`),
which is fine for small attachments but wasteful for large files. For those,
`nova_multipart_plugin` streams each file part to a `nova_multipart_handler`
one chunk at a time, so a chunk's memory is released as soon as the handler
has consumed it:

```erlang
{pre_request, nova_multipart_plugin, #{handler => {nova_multipart_file_handler, #{dir => <<"/var/uploads">>}}}}
```

```erlang
-module(upload_controller).
-export([upload/1]).

upload(#{params := #{<<"title">> := Title}, files := Files}) ->
    %% each File's `result` is whatever the handler returned from
    %% handle_end/1 - for nova_multipart_file_handler, #{path => Path}
    {json, 200, #{}, #{title => Title, uploaded => length(Files)}}.
```

Write a handler by implementing the `nova_multipart_handler` behaviour
(`init/2`, `handle_data/2`, `handle_end/1`, `handle_abort/2`) - see
`nova_multipart_file_handler` for a disk-writing reference implementation.
Every field in a part's info - `filename`, `content_type` and the form field
`name` - is client-controlled, not just `filename`:

* **Never derive a filesystem path from `filename`** - using it directly is a
  path-traversal bug; generate the on-disk name yourself, as the reference
  handler does.
* **Never treat `content_type` as trustworthy** - don't use it to pick a
  code path, and don't echo it back verbatim as a response `Content-Type`
  header (a stored `text/html` content type reflected back is a stored-XSS
  vector).

A part without a `filename=` parameter is classified as a regular field, not
a file - regardless of how large it actually is. It's bounded separately, by
`max_field_size`, so an attacker can't dodge the streaming path (and its
larger size limits) just by omitting `filename=` from an otherwise identical
part.

Configure exactly one of `read_multipart_body` or `nova_multipart_plugin` per
request - the body is a one-shot stream, so both plugins can never run in the
same `pre_request` chain. On a non-multipart request `nova_multipart_plugin`
leaves `Req` untouched, so it never clobbers `params` a preceding plugin
already set.

|Option|Description|
|------|-----------|
|handler|Required. `{Mod, InitArgs}` implementing `nova_multipart_handler`.|
|max_parts|Reply 413 after this many parts (default 32).|
|max_part_size|Reply 413 past this many bytes in a single file part (default 8 000 000).|
|max_field_size|Reply 413 past this many bytes in a single non-file field (default 65 536).|
|max_total_size|Reply 413 past this many bytes across all parts (default 64 000 000).|
|read_timeout|Reply 408 if the whole multipart body isn't read within this many ms (default 60 000).|
