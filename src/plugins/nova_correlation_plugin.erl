-module(nova_correlation_plugin).
-behaviour(nova_plugin).

-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

-include_lib("kernel/include/logger.hrl").

pre_request(Req, _) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    %% Update the loggers metadata with correlation-id
    logger:update_process_metadata(#{<<"correlation-id">> => UUID}),
    Req1 = cowboy_req:set_resp_header(<<"X-Correlation-ID">>, UUID, Req),
    {ok, Req1}.

post_request(Req, _) ->
    {ok, Req}.

plugin_info() ->
    {
     <<"nova_correlation_plugin">>,
     <<"0.1.0">>,
     <<"Nova team <info@novaframework.org">>,
     <<"Add X-Correlation-ID headers to response">>,
     []
    }.
