-module(nova_otel_h).
-behavior(cowboy_stream).

-export([
         init/3,
         data/4,
         info/3,
         terminate/3,
         early_error/5
        ]).

-record(state, {
                next :: any(),
                span :: opentelemetry:span_ctx()
               }).

-type state() :: #state{}.

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
          -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
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
    SpanCtx = ?start_span(request, empty_start_opts(SpanAttrs)),
    Req1 = Req#{span_ctx => SpanCtx},
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{next = Next, span = SpanCtx}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
          -> {cowboy_stream:commands(), State} when State::state().
data(StreamID, IsFin, Data, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0}}.

-spec info(cowboy_stream:streamid(), any(), State)
          -> {cowboy_stream:commands(), State} when State::state().
info(StreamID, {response, Code, _Headers, _Body} = Info, State = #state{next = Next})
  when is_integer(Code) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}};
info(StreamID, Info, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), state()) -> any().
terminate(StreamID, Reason, #state{next = Next, span = SpanCtx}) ->
    Termination = cowboy_stream:terminate(StreamID, Reason, Next),
    otel_tracer:set_current_span(otel_span:end_span(SpanCtx, undefined)),
    Termination.

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
                  cowboy_stream:partial_req(), Resp, cowboy:opts())
                 -> Resp
                        when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, {_, _Status, _Headers, _} = Resp, Opts) ->
    otel_tracer:set_current_span(otel_span:end_span(?current_span_ctx, undefined)),
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

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