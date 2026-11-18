-module(nova_cowboy).
-moduledoc """
Run Nova on [Cowboy](https://github.com/ninenines/cowboy).

Add `nova_cowboy` to the dependencies and to the `applications` of the
application, and Nova's listeners run on Cowboy:

```erlang
{deps, [nova, nova_cowboy]}.
```

```erlang
{applications, [kernel, stdlib, nova, nova_cowboy]}
```

The application starts the listeners `nova_listener` describes as it starts,
and stops them as it stops: no new connections, a wait for the open ones,
and then the listeners go.

## What a listener can ask for

`http1` and `http2`, with or without `tls`. A browser only speaks HTTP/2
over TLS; without it HTTP/2 is for clients that start with it, such as a
proxy or gRPC. `http3` is not served: Cowboy's is experimental and needs a
build of its own. A node that asks for it does not start.

WebSockets are served over HTTP/1.1, and over HTTP/2 when `server_opts` has
`enable_connect_protocol => true`. A WebSocket route on a listener that
serves HTTP/2 alone without it stops the node from starting.

## `server_opts`

The options of a Cowboy listener, as `cowboy_http` and `cowboy_http2`
document them: `idle_timeout`, `max_keepalive`, `compress` and the rest. The
options a version is given, as in `{http2, #{max_concurrent_streams => 100}}`,
are added to them. Three of them Nova fills in:

- `stream_handlers`, by default `[nova_stream_h, cowboy_compress_h,
  cowboy_stream_h]`. A list of your own needs `nova_stream_h` first.
- `middlewares`, which is always `[nova_pipeline]`. The steps of a request
  are the `steps` of the listener.
- `env`, to which Nova adds its own keys

`transport_opts` is not Cowboy's: it is a list of socket options added to
those of the listener, such as `[{backlog, 4096}]`, or a map of ranch's
transport options, such as `#{num_acceptors => 100}`.

## Requests

A request is Cowboy's own request map with Nova's keys added, so code that
calls `cowboy_req` keeps working on this server. Code that calls `nova_req`
works on every server. A response sent with `cowboy_req:reply/4` carries no
session cookie: `nova_req:reply/4` adds it.
""".

-behaviour(nova_server).

-export([
         capabilities/0,
         start_listener/2,
         stop_listener/1,
         drain_listeners/2,
         ranch_ref/1,
         reply/4,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         inform/3,
         read_body/2,
         upgrade_websocket/6,
         websocket_protocols/1,
         response_begun/1
        ]).

-include_lib("kernel/include/logger.hrl").

-define(RESPONSE_BEGUN_TIMEOUT, 5000).
-define(STREAM_HANDLERS, [nova_stream_h, cowboy_compress_h, cowboy_stream_h]).
-define(DRAIN_INTERVAL, 500).

-spec capabilities() -> nova_server:capabilities().
capabilities() ->
    #{protocols => [http1, http2],
      cleartext => [http1, http2],
      websocket => [http1, http2]}.

%%%===================================================================
%%% Listeners
%%%===================================================================

-doc "The name ranch knows the listener by, for `ranch:info/1` and the like.".
-spec ranch_ref(nova_listener:name()) -> ranch:ref().
ranch_ref(Name) ->
    {nova_listener, Name}.

-spec start_listener(nova_listener:listener(), nova_server:env()) -> {ok, pid()} | {error, term()}.
start_listener(Listener = #{name := Name}, Env) ->
    Ref = ranch_ref(Name),
    case bind(Ref, Listener, Env) of
        {error, {already_started, _Pid}} = AlreadyStarted ->
            %% ranch owns the listener, so it outlives a node part that went
            %% down without stopping it. Only a listener Nova started is
            %% replaced.
            case stop_earlier(Ref) of
                ok -> bind(Ref, Listener, Env);
                {error, _Reason} -> AlreadyStarted
            end;
        Result ->
            Result
    end.

bind(Ref, Listener = #{tls := false}, Env) ->
    cowboy:start_clear(Ref, transport_opts(Listener, []), protocol_opts(Listener, Env));
bind(Ref, Listener = #{tls := Tls}, Env) ->
    cowboy:start_tls(Ref, transport_opts(Listener, Tls), protocol_opts(Listener, Env)).

transport_opts(#{ip := Ip, port := Port, server_opts := ServerOpts}, Tls) ->
    Socket = [{port, Port}, {ip, Ip} | Tls],
    case maps:get(transport_opts, ServerOpts, []) of
        More when is_list(More) -> #{socket_opts => Socket ++ More};
        #{} = Ranch -> Ranch#{socket_opts => Socket ++ maps:get(socket_opts, Ranch, [])}
    end.

protocol_opts(#{protocols := Protocols, server_opts := ServerOpts}, Env) ->
    Opts0 = maps:without([transport_opts], ServerOpts),
    Opts = with_version_opts(Protocols, Opts0),
    Opts#{protocols => [cowboy_protocol(Name) || {Name, _VersionOpts} <- Protocols],
          stream_handlers => maps:get(stream_handlers, Opts, ?STREAM_HANDLERS),
          middlewares => [nova_pipeline],
          env => maps:merge(maps:get(env, Opts, #{}), Env)}.

with_version_opts([], Opts) ->
    Opts;
with_version_opts([{_Version, VersionOpts} | Rest], Opts) ->
    with_version_opts(Rest, maps:merge(Opts, VersionOpts)).

cowboy_protocol(http1) -> http;
cowboy_protocol(http2) -> http2.

stop_earlier(Ref) ->
    try ranch:get_protocol_options(Ref) of
        #{env := #{nova_dispatch_key := _DispatchKey}} ->
            ?LOG_NOTICE(#{msg => ~"Replacing a listener that an earlier start of Nova left behind", listener => Ref}),
            cowboy:stop_listener(Ref);
        _OtherOwner ->
            {error, not_nova}
    catch
        error:badarg -> {error, not_found}
    end.

-spec stop_listener(nova_listener:name()) -> ok | {error, term()}.
stop_listener(Name) ->
    cowboy:stop_listener(ranch_ref(Name)).

%% A listener can already be gone, so neither suspending nor inspecting it
%% may bring the shutdown down.
-spec drain_listeners([nova_listener:name()], non_neg_integer()) -> ok.
drain_listeners(Names, Timeout) ->
    Refs = [ranch_ref(Name) || Name <- Names],
    _ = [suspend(Ref) || Ref <- Refs],
    drain(Refs, erlang:monotonic_time(millisecond) + Timeout).

suspend(Ref) ->
    try ranch:suspend_listener(Ref) of
        _Result -> ok
    catch
        _Class:_Reason -> ok
    end.

active_connections(Ref) ->
    try ranch:info(Ref) of
        #{active_connections := Count} -> Count
    catch
        _Class:_Reason -> 0
    end.

drain(Refs, Deadline) ->
    case lists:sum([active_connections(Ref) || Ref <- Refs]) of
        0 ->
            ok;
        Remaining ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    ?LOG_WARNING(#{msg => ~"Drain timeout reached", remaining_connections => Remaining}),
                    ok;
                false ->
                    timer:sleep(?DRAIN_INTERVAL),
                    drain(Refs, Deadline)
            end
    end.

%%%===================================================================
%%% Requests
%%%===================================================================

-spec reply(nova_server:status(), nova_server:headers(), nova_server:body(), Req) -> Req
              when Req :: nova_req:req().
reply(Status, Headers, Body, Req) ->
    {HeaderMap, Req0} = cowboy_headers(Headers, Req),
    cowboy_req:reply(Status, HeaderMap, Body, Req0).

-spec stream_reply(nova_server:status(), nova_server:headers(), Req) -> Req
              when Req :: nova_req:req().
stream_reply(Status, Headers, Req) ->
    {HeaderMap, Req0} = cowboy_headers(Headers, Req),
    cowboy_req:stream_reply(Status, HeaderMap, Req0).

%% The request a controller streams with may be the one from before the
%% headers went out, which cowboy_req has no clause for.
-spec stream_body(iodata(), fin | nofin, nova_req:req()) -> ok.
stream_body(Data, IsFin, Req) ->
    cowboy_req:stream_body(Data, IsFin, Req#{has_sent_resp => headers}).

-spec stream_trailers(nova_server:headers(), nova_req:req()) -> ok.
stream_trailers(Trailers, Req) ->
    cowboy_req:stream_trailers(header_map(Trailers), Req#{has_sent_resp => headers}).

-spec inform(nova_server:status(), nova_server:headers(), nova_req:req()) -> ok.
inform(Status, Headers, Req) ->
    cowboy_req:inform(Status, header_map(Headers), Req).

%% Cowboy exits with a bare timeout when the body does not arrive.
-spec read_body(Req, cowboy_req:read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
              when Req :: nova_req:req().
read_body(Req, Opts) ->
    try
        cowboy_req:read_body(Req, Opts)
    catch
        exit:timeout ->
            exit({request_error, timeout, 'The request body was not received within the configured time.'})
    end.

%% Cowboy answers the upgrade with the headers and cookies of the request,
%% so the request is given those Nova checked and no other.
-spec upgrade_websocket(nova_server:headers(), Req, Env, module(), term(), cowboy_websocket:opts()) ->
          {ok, Req, Env} | {stop, Req} when Req :: nova_req:req(), Env :: nova_server:env().
upgrade_websocket(Headers, Req, Env, Handler, State, Opts) ->
    {HeaderMap, Req0} = cowboy_headers(Headers, Req),
    cowboy_websocket:upgrade(Req0#{resp_headers => HeaderMap}, Env, Handler, State, Opts).

%% Over HTTP/2 a WebSocket is an extended CONNECT, which Cowboy takes when
%% the listener turns it on.
-spec websocket_protocols(nova_listener:listener()) -> [nova_server:protocol()].
websocket_protocols(#{protocols := Protocols, server_opts := ServerOpts}) ->
    case with_version_opts(Protocols, ServerOpts) of
        #{enable_connect_protocol := true} -> [http1, http2];
        #{} -> [http1]
    end.

%% A response sent with cowboy_req went by the stream handler in the
%% connection process, so that is where it is known.
-spec response_begun(nova_req:req()) -> nova_server:response_progress().
response_begun(#{nova_stream := true, pid := _, streamid := _} = Req) ->
    Ref = make_ref(),
    cowboy_req:cast({nova_response_begun, self(), Ref}, Req),
    receive
        {Ref, Sent} -> Sent
    after ?RESPONSE_BEGUN_TIMEOUT ->
            false
    end;
response_begun(_Req) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Nova merged the headers and cookies of the request into the list, so the
%% request Cowboy gets carries none, or Cowboy would add them again. Cowboy
%% takes cookies apart from the headers, as complete set-cookie values.
cowboy_headers(Headers, Req) ->
    {Cookies, Others} = lists:partition(fun({Name, _Value}) -> Name =:= ~"set-cookie" end, Headers),
    Req0 = maps:without([resp_headers, resp_cookies, resp_body], Req),
    case Cookies of
        [] ->
            {header_map(Others), Req0};
        _ ->
            Indexed = lists:zip(lists:seq(1, length(Cookies)), [Value || {_Name, Value} <- Cookies]),
            {header_map(Others), Req0#{resp_cookies => maps:from_list(Indexed)}}
    end.

%% A header named more than once is one header with its values joined.
header_map(Headers) ->
    header_map(Headers, #{}).

header_map([], Map) ->
    Map;
header_map([{Name, Value} | Rest], Map) ->
    case Map of
        #{Name := Earlier} -> header_map(Rest, Map#{Name := [Earlier, ~", ", Value]});
        #{} -> header_map(Rest, Map#{Name => Value})
    end.
