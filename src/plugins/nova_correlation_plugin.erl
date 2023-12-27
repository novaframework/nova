-module(nova_correlation_plugin).
-behaviour(nova_plugin).

-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

-include_lib("kernel/include/logger.hrl").

pre_request(Req0, Opts) ->
    CorrId = get_correlation_id(Req0, Opts),
    %% Update the loggers metadata with correlation-id
    ok = update_logger_metadata(CorrId, Opts),
    Req1 = cowboy_req:set_resp_header(<<"X-Correlation-ID">>, CorrId, Req0),
    Req = Req1#{correlation_id => CorrId},
    {ok, Req}.

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

get_correlation_id(Req, #{ request_correlation_header := CorrelationHeader }) ->
    case cowboy_req:header(CorrelationHeader, Req) of
        undefined ->
            uuid();
        CorrId ->
            CorrId
    end;
get_correlation_id(_Req, _Opts) ->
    uuid().

uuid() ->
    uuid:uuid_to_string(uuid:get_v4()).

update_logger_metadata(CorrId, Opts) ->
    LoggerKey = maps:get(logger_metadata_key, Opts, <<"correlation-id">>),
    logger:update_process_metadata(#{LoggerKey => CorrId}).
