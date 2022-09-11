# Plugins

Plugins is used to handle things before and/or after a request. They are applied on all requests of a specified protocol.

This is an example:

```
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
A good example of a very useful plugin is the `decode_json_body` one. When we are developing a HTTP web api using json as the data format, we need the framework to
decode our message so that we can process it.
We can add this plugin by editing the  `rebar.config` file like below:


**rebar.config**


 ```
     {nova, [
         {environment, dev},
         {cowboy_configuration, #{
                                  port => 8080
                                 }},
         {dev_mode, true},
         {bootstrap_application, chatapp}, 
         {plugins, [
                    {pre_request, nova_request_plugin, #{parse_bindings => true}},
                    {pre_request, nova_request_plugin, #{decode_json_body => true}} -- here
                   ]}
        ]}
 ```
 We have added our plugin in the `plugins` section. As we can see this is a `pre_request` plugin since it processes and decodes the message to json format 
 before we can actually use it in our nova application endpoints.

Usage:

**controller**

```
-module(test_controller).
-behaviour(nova_router).
-export([increment/1]).

increment(#{json := #{<<"id">> := Id, <<"value">> := Value}})->
    {json,200,#{},#{<<"id">> => Id , <<"received">> => Value, <<"increment">> => Value+1}}.

```