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
|nova_multipart_plugin|This plugin reads `multipart/form-data` bodies and streams uploaded files to a handler.|[nova_multipart_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_multipart_plugin.erl)|
|nova_request_plugin|This plugin will handle incomming data like qs, form urlencoded and json. Multipart bodies are left to `nova_multipart_plugin`.|[nova_request_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_request_plugin.erl)|


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
                                         parse_qs => true|list}}
```


|Option|Description|Req|
|------|-----------|----|
|decode_json_body|If header is application/json it will decode the body.| `Req#{json => Map}`|
|read_urlencoded_body|If header is application/x-www-form-urlencoded it will decode it.| `Req#{params => Map}`|
|parse_qs| If the path have qs in it we will get them.|`Req#{parsed_qs => Map or List}`|

### Nova multipart

`nova_request_plugin` never buffers a `multipart/form-data` body. Add
`nova_multipart_plugin` to the route instead and give it a handler that
receives every file part chunk by chunk:

```erlang
{pre_request, nova_multipart_plugin, #{handler => {nova_multipart_file_handler, #{dir => <<"/var/uploads">>}}}}
```

```erlang
-module(upload_controller).
-export([upload/1]).

upload(#{params := #{<<"title">> := Title}, files := Files}) ->
    Paths = [Path || #{result := #{path := Path}} <- Files],
    {json, 200, #{}, #{title => Title, uploaded => Paths}}.
```

Regular form fields end up in `params`. Every part with a filename ends up in
`files` as a map with `name`, `filename`, `content_type` and `result`, where
`result` is what the handler returned. On a non-multipart request `files` is
`[]` and `params` is left alone.

Two handlers ship with Nova:

|Handler|Result|
|-------|------|
|`nova_multipart_file_handler`|Streams the part to a random name under `dir` and returns `#{path => Path}`. Pass `extensions => [<<"png">>, <<"jpg">>]` to keep a listed extension from the client filename, lowercased, on the stored name; any other extension is dropped, never rejected. No list means no extension.|
|`nova_multipart_memory_handler`|Keeps the part in memory and returns `#{body => Binary}`. For small attachments only.|

Write your own by implementing the `nova_multipart_handler` behaviour
(`init/2`, `handle_data/2`, `handle_end/1`, `handle_abort/2`). `filename`,
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
|max_parts|Reply 413 after this many parts (default 32).|
|max_part_size|Reply 413 past this many bytes in a single file part (default 8 000 000).|
|max_field_size|Reply 413 past this many bytes in a single non-file field (default 65 536).|
|max_total_size|Reply 413 past this many bytes across all parts (default 64 000 000).|
|read_timeout|Reply 408 if the whole body is not read within this many ms (default 60 000).|

A malformed part gets a 400 and a handler error a 500. In every case the
controller is never called and the handler's `handle_abort/2` has run.

To read the parts yourself, leave `nova_multipart_plugin` off the route and use
[cowboy_req:read_part/1](https://ninenines.eu/docs/en/cowboy/2.13/manual/cowboy_req.read_part/)
from the controller, handing the updated request back through a five element
return tuple:

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
