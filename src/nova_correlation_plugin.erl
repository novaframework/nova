-module(nova_correlation_plugin).
-behaviour(nova_plugin).

-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

pre_request(Req, _) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    Req1 = cowboy_req:set_resp_header(<<"x-correlation-id">>, UUID, Req),
    {ok, Req1}.

post_request(Req, _) ->
    {ok, Req}.

plugin_info() ->
    {<<"nova_cors_plugin">>, <<"0.1.0">>, <<"">>, <<"Add CORS headers to request">>, []}.
