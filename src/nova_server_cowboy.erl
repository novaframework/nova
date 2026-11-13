-module(nova_server_cowboy).
-moduledoc """
Nova's default server: Cowboy (`nova_server`).

The listener runs Nova's pipeline as Cowboy middlewares, and the request
callbacks are Cowboy's own `cowboy_req` functions.
""".

-behaviour(nova_server).

-export([
         start_listener/3,
         stop_listener/1,
         drain_listeners/2,
         reply/4,
         inform/3,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         read_body/2,
         read_part/2,
         read_part_body/2,
         upgrade/6
        ]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova.hrl").

-define(NOVA_STD_PORT, 8080).
-define(NOVA_STD_SSL_PORT, 8443).
-define(DRAIN_POLL_INTERVAL, 500).

%%--------------------------------------------------------------------
%% Listeners
%%--------------------------------------------------------------------

-doc false.
start_listener(Ref, Configuration, Env) ->
    Options = cowboy_options(Configuration, Env),
    case maps:get(use_ssl, Configuration, false) of
        false ->
            Port = maps:get(port, Configuration, ?NOVA_STD_PORT),
            Host = maps:get(ip, Configuration, {0, 0, 0, 0}),
            cowboy:start_clear(Ref, [{port, Port}, {ip, Host}], Options);
        _ ->
            start_tls(Ref, Configuration, Options)
    end.

-doc false.
stop_listener(Ref) ->
    cowboy:stop_listener(Ref).

-doc false.
drain_listeners(Refs, Timeout) ->
    [suspend(Ref) || Ref <- Refs],
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    drain_loop(Refs, Deadline).

cowboy_options(Configuration, Env0) ->
    StreamHandlers = maps:get(stream_handlers, Configuration,
                              [nova_stream_h, cowboy_compress_h, cowboy_stream_h]),
    Options = maps:get(options, Configuration, #{compress => true}),
    %% nova_router reads its routing table out of the Env, so a listener
    %% always carries the key of the table it serves.
    Env = maps:merge(maps:get(env, Options, #{}), maps:remove(nova_middlewares, Env0)),
    Options#{middlewares => nova_server:middlewares(Configuration),
             stream_handlers => StreamHandlers,
             env => Env}.

start_tls(Ref, Configuration, Options) ->
    Port = maps:get(ssl_port, Configuration, ?NOVA_STD_SSL_PORT),
    Host = maps:get(ip, Configuration, {0, 0, 0, 0}),
    TransportOpts =
        case maps:get(ca_cert, Configuration, undefined) of
            undefined ->
                SSLOptions = maps:get(ssl_options, Configuration, #{}),
                maps:to_list(SSLOptions#{port => Port, ip => Host});
            CACert ->
                Cert = maps:get(cert, Configuration),
                ?LOG_DEPRECATED(<<"0.10.3">>, <<"Use of ca_cert/cert is deprecated, use ssl_options instead">>),
                [{port, Port}, {ip, Host}, {certfile, Cert}, {cacertfile, CACert}]
        end,
    cowboy:start_tls(Ref, TransportOpts, Options).

%% A listener can already be gone if the application was removed at runtime,
%% so neither suspending nor inspecting it may bring the shutdown down.
suspend(Ref) ->
    try ranch:suspend_listener(Ref) of
        _Result -> ok
    catch
        _Class:_Reason -> ok
    end.

active_connections(Ref) ->
    try ranch:info(Ref) of
        #{active_connections := N} -> N
    catch
        _Class:_Reason -> 0
    end.

drain_loop(Refs, Deadline) ->
    case lists:sum([active_connections(Ref) || Ref <- Refs]) of
        0 ->
            ok;
        N ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true ->
                    ?LOG_WARNING(#{msg => <<"Drain timeout reached">>,
                                   remaining_connections => N}),
                    ok;
                false ->
                    timer:sleep(?DRAIN_POLL_INTERVAL),
                    drain_loop(Refs, Deadline)
            end
    end.

%%--------------------------------------------------------------------
%% Requests
%%--------------------------------------------------------------------

-doc false.
reply(Status, Headers, Body, Req) ->
    cowboy_req:reply(Status, Headers, Body, Req).

-doc false.
inform(Status, Headers, Req) ->
    cowboy_req:inform(Status, Headers, Req).

-doc false.
stream_reply(Status, Headers, Req) ->
    cowboy_req:stream_reply(Status, Headers, Req).

-doc false.
stream_body(Data, IsFin, Req) ->
    cowboy_req:stream_body(Data, IsFin, Req).

-doc false.
stream_trailers(Trailers, Req) ->
    cowboy_req:stream_trailers(Trailers, Req).

-doc false.
read_body(Req, Opts) ->
    cowboy_req:read_body(Req, Opts).

-doc false.
read_part(Req, Opts) ->
    cowboy_req:read_part(Req, Opts).

-doc false.
read_part_body(Req, Opts) ->
    cowboy_req:read_part_body(Req, Opts).

-doc false.
upgrade(Mod, Req, Env, Handler, State, none) ->
    Mod:upgrade(Req, Env, Handler, State);
upgrade(Mod, Req, Env, Handler, State, {opts, Opts}) ->
    Mod:upgrade(Req, Env, Handler, State, Opts).
