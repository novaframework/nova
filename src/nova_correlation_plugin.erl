-module(nova_correlation_plugin).
-behaviour(nova_plugin).

-export([pre_http_request/2,
         post_http_request/2,
         plugin_info/0]).

pre_http_request(#{req := Req} = NovaState, _) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    Req1 = cowboy_req:set_resp_header(<<"x-correlation-id">>, UUID, Req),
    NewState = maps:put(req, Req1, NovaState),
    {ok, NewState}.

post_http_request(NovaState, _) ->
    {ok, NovaState}.

plugin_info() ->
    {<<"nova_cors_plugin">>, <<"0.1.0">>, <<"">>, <<"Add CORS headers to request">>, []}.
