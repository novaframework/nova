-module(nova_roadrunner).
-moduledoc """
Run Nova on [Roadrunner](https://github.com/arizona-framework/roadrunner).

Add `nova_roadrunner` to the dependencies and to the `applications` of the
application, and Nova's listeners run on Roadrunner:

```erlang
{deps, [nova, nova_roadrunner]}.
```

```erlang
{applications, [kernel, stdlib, nova, nova_roadrunner]}
```

The application starts the listeners `nova_listener` describes as it starts,
and stops them as it stops: no new connections, a wait for the open ones,
and then the listeners go. A node that has `nova_cowboy` as well says which
of the two serves with `{server, nova_roadrunner}` in the environment of
`nova`.

## What a listener can ask for

`http1`, `http2` and `http3`:

```erlang
{listeners, #{
    web => #{port => 8443,
             tls => #{certfile => "cert.pem", keyfile => "key.pem"},
             protocols => [http1, http2, http3]}
}}
```

HTTP/3 is served over UDP on the port of the listener and needs `tls`. A
listener that serves it tells clients so with `alt-svc` on its HTTP/1.1 and
HTTP/2 responses, which is how a browser finds it.

Without `tls` a listener serves one version: `[http1]`, or `[http2]` for
clients that start with HTTP/2, such as a proxy or gRPC. A node that asks
for both on one port without `tls` does not start.

WebSockets are served over HTTP/1.1.

## `server_opts`

The options of a Roadrunner listener, as `roadrunner_listener` documents
them: `max_clients`, `request_timeout`, `max_content_length`, `real_ip` and
the rest. The options a version is given, as in
`{http2, #{max_concurrent_streams => 100}}`, are Roadrunner's for that
version. Nova fills in `port`, `ip`, `tls`, `protocols`, `routes` and
`body_buffering`.

`compress` is Nova's: responses that are sent whole are compressed for
clients that accept it, unless it is `false`. A streamed response is sent
as it is.

## Requests

Controllers, plugins and WebSocket handlers that call `nova_req` run as they
do on any server. The request has Roadrunner's own under `roadrunner_req`.

## What differs from Cowboy

- A request body has the time its listener gives it: `request_timeout` for
  all of it, and `min_bytes_per_second` for one that trickles in. The
  `period` and `timeout` of `nova_req:read_body/2`, and so the
  `body_read_timeout` of `nova_request_plugin`, are not looked at.
- After a response that asks to close the connection while a body is still
  owed, Roadrunner waits for that body before it closes.
- An HTTP/1.1 connection is closed after a streamed response.
- A method is made of uppercase letters. A request with another, such as
  `M-SEARCH`, is answered `400`.
- Requests with more header bytes than `max_header_block` of `http1`, 10240
  unless set, are answered `431`.
- WebSockets are not served over HTTP/2, and a route that is a Cowboy
  handler is not served at all.
- Every request of a connection over HTTP/1.1 runs in the process of the
  connection, one after the other. What a request leaves in the process
  dictionary or the mailbox is there for the next. Nova puts the logger
  metadata of the process back after each request.

Derived from [roadrunner_nova](https://github.com/arizona-framework/roadrunner_nova)
by William Fank Thomé.
""".

-behaviour(nova_server).

-export([
         capabilities/0,
         check_listener/1,
         start_listener/2,
         stop_listener/1,
         drain_listeners/2,
         roadrunner_name/1,
         reply/4,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         inform/3,
         read_body/2,
         upgrade_websocket/6
        ]).

%% Called by nova_roadrunner_handler
-export([start_request/0, end_request/0]).

-ifdef(TEST).
-export([why/1]).
-endif.

-include_lib("kernel/include/logger.hrl").

-define(READ_BODY_LENGTH, 8000000).
-define(STREAM, {?MODULE, stream}).

-spec capabilities() -> nova_server:capabilities().
capabilities() ->
    #{protocols => [http1, http2, http3],
      cleartext => [http1, http2],
      websocket => [http1]}.

%%%===================================================================
%%% Listeners
%%%===================================================================

-doc """
The name Roadrunner knows the listener by, for `roadrunner_listener:info/1`
and the like. Names that start with `nova_listener_` are Nova's: a Roadrunner
listener that has one is stopped and replaced when Nova starts the listener
of that name.
""".
-spec roadrunner_name(nova_listener:name()) -> atom().
roadrunner_name(Name) ->
    %% One atom for each listener of the configuration.
    % elp:ignore W0023 (atoms_exhaustion)
    list_to_atom("nova_listener_" ++ atom_to_list(Name)).

%% HTTP/1.1 and HTTP/2 share a port by ALPN, which there is none of without
%% TLS, and Roadrunner does not upgrade a clear connection to HTTP/2.
-spec check_listener(nova_listener:listener()) -> ok | {error, term()}.
check_listener(#{name := Name, tls := false, protocols := [_, _ | _] = Protocols}) ->
    {error, {listener_without_tls_serves_one_protocol, Name, nova_listener:protocol_names(Protocols)}};
check_listener(#{name := Name, ip := Ip}) when not is_tuple(Ip) ->
    {error, {invalid_listener_setting, Name, ip, Ip}};
check_listener(_Listener) ->
    ok.

-spec start_listener(nova_listener:listener(), nova_server:env()) -> {ok, pid()} | {error, term()}.
start_listener(#{name := Name, ip := Ip, port := Port, tls := Tls, protocols := Protocols,
                 server_opts := ServerOpts}, Env) when is_tuple(Ip) ->
    State = #{env => Env, compress => maps:get(compress, ServerOpts, true) =/= false},
    Opts0 = (maps:remove(compress, ServerOpts))#{ip => Ip,
                                                 port => Port,
                                                 protocols => [protocol(Protocol) || Protocol <- Protocols],
                                                 body_buffering => manual,
                                                 routes => {nova_roadrunner_handler, State}},
    Opts = case Tls of
               false -> Opts0;
               _ -> Opts0#{tls => Tls}
           end,
    case started(roadrunner_name(Name), Opts) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, why(Reason)}
    end;
start_listener(#{name := Name, ip := Ip}, _Env) ->
    {error, {invalid_listener_setting, Name, ip, Ip}}.

%% Roadrunner owns the listener, so it outlives a node part that went down
%% without stopping it. Its name is one that only this module gives, so a
%% listener that has it is one Nova started.
started(Ref, Opts) ->
    case roadrunner:start_listener(Ref, Opts) of
        {error, {already_started, _Pid}} ->
            ?LOG_NOTICE(#{msg => ~"Replacing a listener that an earlier start of Nova left behind", listener => Ref}),
            _ = roadrunner:stop_listener(Ref),
            roadrunner:start_listener(Ref, Opts);
        Result ->
            Result
    end.

%% What kept the listener from starting, without the child specification
%% Roadrunner's supervisor reports it with: that has the TLS options in it.
why({{listen_failed, Reason}, _Child}) -> Reason;
why({Reason, Child}) when element(1, Child) =:= child -> Reason;
why(Reason) -> Reason.

protocol({Version, Opts}) when map_size(Opts) =:= 0 -> Version;
protocol(Protocol) -> Protocol.

%% A listener that was drained is gone already.
-spec stop_listener(nova_listener:name()) -> ok | {error, term()}.
stop_listener(Name) ->
    case roadrunner:stop_listener(roadrunner_name(Name)) of
        ok -> ok;
        {error, not_found} -> ok
    end.

%% Each drain waits for its own connections, so they run side by side.
-spec drain_listeners([nova_listener:name()], timeout()) -> ok.
drain_listeners(Names, Timeout) ->
    Parent = self(),
    Drains = [spawn_monitor(fun() -> Parent ! {drained, self(), drain(roadrunner_name(Name), Timeout)} end)
              || Name <- Names],
    _ = [receive
             {drained, Pid, _Result} -> erlang:demonitor(Monitor, [flush]);
             {'DOWN', Monitor, process, Pid, _Reason} -> ok
         end || {Pid, Monitor} <- Drains],
    ok.

drain(Name, Timeout) ->
    try
        roadrunner_listener:drain(Name, Timeout)
    catch
        %% Already gone, as that of an application removed at runtime is.
        exit:{noproc, _} -> ok
    end.

%%%===================================================================
%%% Requests
%%%===================================================================

-doc false.
%% The process that serves a request serves the next one of the connection
%% too, so what it kept of the last one is forgotten.
-spec start_request() -> ok.
start_request() ->
    _ = erase(?STREAM),
    ok.

-doc false.
%% A response that was begun and not ended, as one whose controller crashed
%% halfway is, is ended here, so the connection can serve the next request.
-spec end_request() -> ok.
end_request() ->
    ok = stream(<<>>, fin),
    start_request().

-spec reply(nova_server:status(), nova_server:headers(), nova_server:body(), Req) -> Req
              when Req :: nova_req:req().
reply(Status, Headers, {sendfile, Offset, Length, Path}, Req) ->
    File = {filename(Path), Offset, Length},
    sent(roadrunner_send:file(Status, with_content_length(Length, header_list(Headers)), File, roadrunner_req(Req)),
         Status, Req);
reply(Status, Headers0, Body0, #{roadrunner_compress := Compress} = Req) ->
    RR = roadrunner_req(Req),
    {Headers, Body} = case Compress of
                          true -> roadrunner_compress:buffered(RR, header_list(Headers0), Body0);
                          false -> {header_list(Headers0), Body0}
                      end,
    sent(roadrunner_send:reply(Status, Headers, Body, RR), Status, Req).

-spec stream_reply(nova_server:status(), nova_server:headers(), Req) -> Req when Req :: nova_req:req().
stream_reply(Status, Headers, Req) ->
    case roadrunner_send:stream(Status, header_list(Headers), roadrunner_req(Req)) of
        {ok, Send} ->
            _ = put(?STREAM, Send),
            Req;
        {error, _Reason} = Error ->
            sent(Error, Status, Req)
    end.

%% The fun that writes the body is kept in the process, since a controller
%% may stream with the request from before the headers went out.
-spec stream_body(iodata(), fin | nofin, nova_req:req()) -> ok.
stream_body(Data, IsFin, _Req) ->
    stream(Data, IsFin).

-spec stream_trailers(nova_server:headers(), nova_req:req()) -> ok.
stream_trailers(Trailers, _Req) ->
    stream(<<>>, {fin, header_list(Trailers)}).

stream(Data, IsFin) ->
    case get(?STREAM) of
        undefined ->
            ok;
        Send when IsFin =:= nofin ->
            _ = Send(Data, nofin),
            ok;
        Send ->
            _ = erase(?STREAM),
            _ = Send(Data, IsFin),
            ok
    end.

-spec inform(nova_server:status(), nova_server:headers(), nova_req:req()) -> ok.
inform(Status, Headers, Req) ->
    _ = roadrunner_send:inform(Status, header_list(Headers), roadrunner_req(Req)),
    ok.

filename(Path) when is_atom(Path) -> atom_to_list(Path);
filename(Path) -> Path.

-spec read_body(Req, nova_server:read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
              when Req :: nova_req:req().
read_body(Req, Opts) ->
    Read = case maps:get(length, Opts, ?READ_BODY_LENGTH) of
               infinity -> roadrunner_req:read_body(roadrunner_req(Req));
               Length -> roadrunner_req:read_body(roadrunner_req(Req), #{length => Length})
           end,
    case Read of
        {ok, Data, RR} -> {ok, iolist_to_binary(Data), Req#{roadrunner_req := RR}};
        {more, Data, RR} -> {more, iolist_to_binary(Data), Req#{roadrunner_req := RR}};
        {error, Reason} -> exit(request_error(Reason))
    end.

%% A request that does not ask for a WebSocket is told what the route is
%% for, and one that asks for it over a version it is not served over that
%% it is not there.
-spec upgrade_websocket(nova_server:headers(), Req, Env, module(), term(), nova_server:ws_opts()) ->
          {ok, Req, Env} | {stop, Req} when Req :: nova_req:req(), Env :: nova_server:env().
upgrade_websocket(Headers, Req = #{version := 'HTTP/1.1'}, Env, Handler, State, Opts) ->
    case is_upgrade(Req) of
        true ->
            Session = #{handler => Handler,
                        state => State,
                        req => maps:without([roadrunner_req, roadrunner_compress], Req)},
            {ok, sent(roadrunner_send:websocket(nova_roadrunner_ws, Session, ws_opts(Opts, Headers), roadrunner_req(Req)), 101,
                      Req),
             Env};
        false ->
            {ok, reply(426, [{~"connection", ~"upgrade"}, {~"upgrade", ~"websocket"}, {~"content-length", ~"0"}],
                       <<>>, Req),
             Env}
    end;
upgrade_websocket(_Headers, Req, Env, _Handler, _State, _Opts) ->
    {ok, reply(501, [{~"content-length", ~"0"}], <<>>, Req), Env}.

is_upgrade(Req) ->
    try
        nova_req:is_websocket_upgrade(Req)
    catch
        _Class:_Reason -> false
    end.

%% The options of a connection that Roadrunner has a counterpart of, and the
%% headers of the answer to the upgrade.
ws_opts(Opts, Headers) ->
    with_ws_opts(maps:to_list(Opts), #{headers => header_list(Headers)}).

with_ws_opts([], Acc) ->
    Acc;
with_ws_opts([{idle_timeout, Timeout} | Rest], Acc) ->
    with_ws_opts(Rest, Acc#{idle_timeout => Timeout});
with_ws_opts([{max_frame_size, Size} | Rest], Acc) when is_integer(Size) ->
    with_ws_opts(Rest, Acc#{max_frame_size => Size, max_message_size => Size});
with_ws_opts([{compress, Compress} | Rest], Acc) ->
    with_ws_opts(Rest, Acc#{compress => Compress});
with_ws_opts([_Other | Rest], Acc) ->
    with_ws_opts(Rest, Acc).

%%%===================================================================
%%% Internal functions
%%%===================================================================

roadrunner_req(#{roadrunner_req := RR}) -> RR.

%% Roadrunner answers 500 itself for a response it could not send.
sent(ok, _Status, Req) ->
    Req;
sent({error, Reason}, Status, Req) ->
    ?LOG_ERROR(#{msg => ~"Roadrunner could not send the response", status => Status, reason => Reason}),
    Req.

header_list(Headers) ->
    [{Name, iolist_to_binary(Value)} || {Name, Value} <- Headers].

with_content_length(Length, Headers) ->
    case lists:keymember(~"content-length", 1, Headers) of
        true -> Headers;
        false -> [{~"content-length", integer_to_binary(Length)} | Headers]
    end.

%% A body that could not be read, as the exit Nova's plugins answer with
%% `408`, `413` or `400`.
request_error(Reason) when Reason =:= request_timeout; Reason =:= slow_client; Reason =:= timeout ->
    {request_error, timeout, 'The request body was not received within the configured time.'};
request_error(content_length_too_large) ->
    {request_error, payload_too_large, 'The request body is larger than allowed by configuration.'};
request_error(Reason) ->
    {request_error, Reason, 'The request body could not be read.'}.
