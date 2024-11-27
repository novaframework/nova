-module(nova_otel_handler).
-behaviour(cowboy_middleware).

%% Callbacks
-export([execute/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-spec execute(Req, Env) -> {ok, Req, Env} when
    Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
execute(#{otel_ctx := Ctx, otel_span_ctx := SpanCtx} = Req, Env) ->
    _ = otel_ctx:attach(Ctx),
    ?set_current_span(SpanCtx),
    {ok, Req, Env};
execute(Req, Env) ->
    {ok, Req, Env}.