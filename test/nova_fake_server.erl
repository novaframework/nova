-module(nova_fake_server).
-moduledoc false.

%% A nova_server without a socket: what Nova sends goes to the mailbox of the
%% process that sent it, and the body is read from the request's `body_chunks`.

-behaviour(nova_server).

-export([capabilities/0, start_listener/2, stop_listener/1, drain_listeners/2,
         reply/4, stream_reply/3, stream_body/3, stream_trailers/2, inform/3,
         read_body/2, upgrade_websocket/5]).
-export([req/0, req/1, sent/0]).

capabilities() ->
    #{protocols => [http1, http2, http3], cleartext => [http1, http2], websocket => [http1]}.

start_listener(_Listener, _Env) -> {ok, undefined}.

stop_listener(_Name) -> ok.

drain_listeners(_Names, _Timeout) -> ok.

reply(Status, Headers, Body, Req) ->
    self() ! {?MODULE, {reply, Status, Headers, Body}},
    Req.

stream_reply(Status, Headers, Req) ->
    self() ! {?MODULE, {stream_reply, Status, Headers}},
    Req.

stream_body(Data, IsFin, _Req) ->
    self() ! {?MODULE, {stream_body, iolist_to_binary(Data), IsFin}},
    ok.

stream_trailers(Trailers, _Req) ->
    self() ! {?MODULE, {stream_trailers, Trailers}},
    ok.

inform(Status, Headers, _Req) ->
    self() ! {?MODULE, {inform, Status, Headers}},
    ok.

read_body(Req = #{body_chunks := [Last]}, _Opts) ->
    {ok, Last, Req#{body_chunks := []}};
read_body(Req = #{body_chunks := [Chunk | Rest]}, _Opts) ->
    {more, Chunk, Req#{body_chunks := Rest}};
read_body(Req, _Opts) ->
    {ok, <<>>, Req}.

upgrade_websocket(Req, Env, Handler, State, Opts) ->
    self() ! {?MODULE, {upgrade_websocket, Handler, State, Opts}},
    {ok, Req, Env}.

req() ->
    req(#{}).

req(Fields) ->
    _ = erase(nova_response_begun),
    maps:merge(#{nova_server => ?MODULE,
                 method => ~"GET",
                 version => 'HTTP/1.1',
                 scheme => ~"http",
                 host => ~"localhost",
                 port => 8080,
                 path => ~"/",
                 qs => <<>>,
                 headers => #{},
                 peer => {{127, 0, 0, 1}, 50000},
                 has_body => false,
                 body_length => 0}, Fields).

%% Everything sent from this process so far, oldest first.
sent() ->
    receive
        {?MODULE, Sent} -> [Sent | sent()]
    after 0 ->
            []
    end.
