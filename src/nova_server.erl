-module(nova_server).
-moduledoc """
The HTTP server Nova runs on.

Nova has no HTTP server of its own. A server package implements this
behaviour and Nova runs on it: `nova_cowboy` for Cowboy, `nova_roadrunner`
for Roadrunner. The `server` key of Nova's environment names the module.

```erlang
{nova, [{server, nova_cowboy}]}
```

When the key is not set, Nova uses `nova_cowboy` if that package is in the
release, and `nova_roadrunner` otherwise.

A server does four things:

- It says what it can do with `capabilities/0`, which Nova checks the
  listeners and routes against when the node starts.
- It starts, stops and drains the listeners of `nova_listener`.
- It runs `nova_pipeline:execute/2` on every request, with a request map as
  `nova_req` describes it and the environment its listener was started with.
- It does what touches the connection: sending a response, reading the body
  and upgrading to a WebSocket. `nova_req` calls these callbacks, so
  application code never calls a server module.

## Responses

`reply/4` and `stream_reply/3` get the status as an integer and the complete
list of response headers: names are lowercase binaries, and a name can be in
the list more than once, as `set-cookie` is. Nova has merged the headers and
cookies set on the request, set `content-length`, and left the body out of a
response to `HEAD`. The server adds `date` and whatever is its own.

A body is `iodata()` or `{sendfile, Offset, Length, Path}`.

## Reading the body

`read_body/2` takes `length` (the bytes to read before it returns `more`),
`period` (the milliseconds to read for before it returns what it has) and
`timeout`. A body that cannot be read is an exit with
`{request_error, Reason, Message}`, where `Reason` is `timeout`,
`payload_too_large` or what was wrong with the request. Nova answers 408, 413
and 400 for these.

## WebSockets

`upgrade_websocket/5` hands the connection to `Handler`, which is
`nova_ws_handler`. The server calls its `websocket_init/1`,
`websocket_handle/2`, `websocket_info/2` and `terminate/3`. Each of the first
three returns `{Commands, State}`, `{Commands, State, hibernate}` or
`{stop, State}`, where a command is a frame to send: `{text, Data}`,
`{binary, Data}`, `ping`, `pong`, `{ping, Data}`, `{pong, Data}`, `close` or
`{close, Code, Reason}`. A command the server does not know is skipped.

The options are `idle_timeout`, `max_frame_size` and `compress`. Other keys
are the server's own.
""".

-export([
         module/0,
         of_req/1,
         capabilities/0,
         capabilities/1,
         supports/2
        ]).

-export_type([
              capabilities/0,
              protocol/0,
              env/0,
              status/0,
              headers/0,
              body/0,
              read_body_opts/0,
              ws_opts/0,
              ws_frame/0,
              response_progress/0
             ]).

-type protocol() :: http1 | http2 | http3.

-doc """
What a server can do.

- `protocols`: the HTTP versions a listener can serve.
- `cleartext`: those of them it serves without TLS.
- `websocket`: the HTTP versions a WebSocket can be opened over.
""".
-type capabilities() :: #{protocols := [protocol()],
                          cleartext := [protocol()],
                          websocket := [protocol()]}.

-doc "The environment a listener's requests run in. Nova's own.".
-type env() :: map().

-type status() :: 100..599.
-type headers() :: [{binary(), iodata()}].
-type body() :: iodata() | {sendfile, Offset :: non_neg_integer(),
                            Length :: non_neg_integer(), Path :: file:name_all()}.
-type read_body_opts() :: #{length => non_neg_integer() | infinity,
                            period => non_neg_integer(),
                            timeout => timeout()}.
-type ws_opts() :: #{idle_timeout => timeout(),
                     max_frame_size => non_neg_integer() | infinity,
                     compress => boolean(),
                     _ => _}.
-type ws_frame() :: {text | binary | ping | pong, iodata()}
                  | ping | pong | close
                  | {close, Code :: 1000..4999, Reason :: iodata()}.

-doc "How far the response to a request got: not begun, its headers sent, or all of it.".
-type response_progress() :: false | headers | true.

%%--------------------------------------------------------------------
%% Listeners
%%--------------------------------------------------------------------

-callback capabilities() -> capabilities().

-doc """
Start a listener. `Listener` is checked and complete: the server reads
`name`, `ip`, `port`, `tls`, `protocols` and `server_opts` from it. Every
request it serves runs `nova_pipeline:execute/2` with `Env`.
""".
-callback start_listener(Listener :: map(), Env :: env()) ->
    {ok, pid() | undefined} | {error, term()}.

-callback stop_listener(Name :: atom()) -> ok | {error, term()}.

-doc """
Stop accepting connections on the listeners and wait, up to `Timeout`
milliseconds, for the open ones to finish. It returns when they have, or when
the time is up. Nova stops the listeners afterwards.
""".
-callback drain_listeners(Names :: [atom()], Timeout :: non_neg_integer()) -> ok.

%%--------------------------------------------------------------------
%% Requests
%%--------------------------------------------------------------------

-callback reply(status(), headers(), body(), Req) -> Req when Req :: nova_req:req().

-callback stream_reply(status(), headers(), Req) -> Req when Req :: nova_req:req().

-callback stream_body(iodata(), fin | nofin, nova_req:req()) -> ok.

-callback stream_trailers(headers(), nova_req:req()) -> ok.

-callback inform(status(), headers(), nova_req:req()) -> ok.

-callback read_body(Req, read_body_opts()) ->
    {ok, binary(), Req} | {more, binary(), Req} when Req :: nova_req:req().

-callback upgrade_websocket(Req, Env, Handler :: module(), State :: term(), ws_opts()) ->
    {ok, Req, Env} | {stop, Req} when Req :: nova_req:req(), Env :: env().

-doc """
How far the response got, for a server that can see a response Nova did not
send, such as one a controller sent with the server's own functions.
""".
-callback response_begun(nova_req:req()) -> response_progress().

-doc """
What the server cannot do for a listener and `capabilities/0` cannot say,
such as two HTTP versions that cannot share a port without TLS. The node
does not start with a listener that is refused.
""".
-callback check_listener(nova_listener:listener()) -> ok | {error, term()}.

-optional_callbacks([stream_trailers/2, inform/3, response_begun/1, check_listener/1]).

%%--------------------------------------------------------------------
-doc "The server module of the node.".
-spec module() -> module().
module() ->
    case application:get_env(nova, server) of
        {ok, Server} when is_atom(Server) -> Server;
        undefined -> default_module()
    end.

-doc "The server module serving a request.".
-spec of_req(nova_req:req()) -> module().
of_req(#{nova_server := Server}) -> Server;
of_req(_Req) -> module().

-doc "What the node's server can do.".
-spec capabilities() -> capabilities().
capabilities() ->
    capabilities(module()).

-spec capabilities(module()) -> capabilities().
capabilities(Server) ->
    Server:capabilities().

-doc "Whether a server has one of the optional callbacks.".
-spec supports(module(), {atom(), arity()}) -> boolean().
supports(Server, {Function, Arity}) ->
    _ = code:ensure_loaded(Server),
    erlang:function_exported(Server, Function, Arity).

%% A release has one server package in it, so the one that is there is the
%% one meant.
default_module() ->
    case code:ensure_loaded(nova_cowboy) of
        {module, nova_cowboy} -> nova_cowboy;
        {error, _} -> nova_roadrunner
    end.
