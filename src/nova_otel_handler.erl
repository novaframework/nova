-module(nova_otel_handler).
-behaviour(cowboy_middleware).

%% Callbacks
-export([execute/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-spec execute(Req, Env) -> {ok, Req, Env}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, #{span_ctx := SpanCtx}=Env) ->
    % End span that was started
    Timestamp = undefined,
    otel_tracer:set_current_span(otel_span:end_span(SpanCtx, Timestamp)),
    {ok, Req, Env};
execute(Req, Env) ->
    %% Extract and start opentelemetry span
    Propagator = opentelemetry:get_text_map_extractor(),
    otel_propagator_text_map:extract(
        Propagator, Req, fun cowboy_headers/1, fun cowboy_header_value/2
    ),
    Headers = maps:get(headers, Req),
    {RemoteIP, _Port} = maps:get(peer, Req),
    Method = maps:get(method, Req),
    SpanAttrs =
        #{
          <<"correlation_id">> => maps:get(<<"x-correlation-id">>, Headers, undefined),
          <<"http.host">> => maps:get(host, Req),
          <<"http.host.port">> => maps:get(port, Req),
          <<"http.method">> => Method,
          <<"http.scheme">> => maps:get(scheme, Req),
          <<"http.target">> => maps:get(path, Req),
          <<"http.user_agent">> => maps:get(<<"user-agent">>, Headers, <<"">>),
          <<"net.host.ip">> => iolist_to_binary(inet:ntoa(RemoteIP))
    },
    SpanCtx = otel_tracer:start_span(?current_tracer, Method, empty_start_opts(SpanAttrs)),
    otel_tracer:set_current_span(SpanCtx),
    {ok, Req, Env#{span_ctx => SpanCtx}}.

%% otel_tracer is typespeced to require the field in the latest hex release
empty_start_opts(Attrs) ->
    #{
        attributes => Attrs,
        links => [],
        is_recording => true,
        start_time => opentelemetry:timestamp(),
        kind => ?SPAN_KIND_SERVER
    }.

cowboy_header_value(Header, Req) ->
    #{headers := Headers} = Req,
    maps:get(Header, Headers, undefined).

cowboy_headers(Req) ->
    Headers = cowboy_req:headers(Req),
    maps:keys(Headers).