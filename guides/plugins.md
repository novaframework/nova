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
