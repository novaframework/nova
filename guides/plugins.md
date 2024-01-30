# Plugins

Plugins are a bit like the handlers except they are run on request. There's currently two different type of plugins; `pre_request` and `post_request`.
These can be used to create access logs, insert CORS headers or similar.

Plugins are used to handle things before and/or after a request. They are applied on all requests of a specified protocol.

This is an example:

```erlang
-module(correlation_id).
-behaviour(nova_plugin).
-export([
    pre_request/2,
    post_request/2,
    plugin_info/0
  ]).

pre_request(Req, NovaState) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    {ok, cowboy_req:set_resp_header(<<"x-correlation-id">>, UUID, Req), NovaState}.

post_request(Req, NovaState) ->
    {ok, Req, NovaState}.

plugin_info() ->
    {<<"Correlation plugin">>, <<"1.0.0">>, <<"Niclas Axelsson <niclas@burbas.se>">>,
     <<"Example plugin for nova">>}.
```

This plugin injects a UUID into the headers.


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
|nova_request_plugin|This plugin will handle incomming data like qs, form urlencoded and json|[nova_request_plugin](https://github.com/novaframework/nova/blob/master/src/plugins/nova_request_plugin.erl)|


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
