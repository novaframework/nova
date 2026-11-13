-module(nova_server).
-moduledoc """
The HTTP server Nova runs on.

Nova runs on Cowboy by default (`nova_server_cowboy`). Another server plugs
in through this behaviour: set the `server` key of Nova's environment to the
module that implements it.

```erlang
{nova, [
    {server, my_server},
    {server_configuration, #{port => 8080}}
]}
```

A server module does three things:

- It starts, stops and drains the listeners `nova_sup` asks for, from the
  same configuration map as the `server_configuration` key. Its shared
  keys (`ip`, `port`, `use_ssl`, `ssl_port`, `ssl_options`) mean the same
  for every server, and `options` holds the server's own options.
- It runs Nova's pipeline on each request with `execute/2`, after
  `nova_session:start_request/1`, handing it a request map shaped like
  Cowboy's, described below.
- It performs what touches the connection: sending responses, reading the
  body, and upgrading to a WebSocket. `nova_req` calls these callbacks for
  the server in the request's `nova_server` key, and Cowboy's when the key
  is absent.

## The request map

`nova_req` reads and sets fields of the request map the way `cowboy_req`
does, so a server's map carries the keys and value types of Cowboy's:

- `nova_server`: the server module
- `method` (`<<"GET">>`), `version` (an atom, `'HTTP/1.1'`), `scheme`
  (`<<"http">>` or `<<"https">>`), `host`, `port`, `path` and `qs` (the raw
  query string)
- `headers`: a map of lowercase names to values, repeated headers joined
  with `, ` (`; ` for `cookie`)
- `peer` (`{IP, Port}`), and `sock` and `cert` when the server has them
- `has_body` (a boolean) and `body_length` (an integer, or `undefined`
  when unknown)

Nova's pipeline adds its own keys (`bindings`, `plugins`, `extra_state`,
`resp_status_code`, `auth_data`), and `cowboy_req`'s setters add the
response keys: `resp_headers` (a map), `resp_cookies` (a map of names to
complete `set-cookie` values) and `resp_body`.

## What stays with Cowboy

A few settings are Cowboy's alone, and another server leaves them out:

- `stream_handlers` in the configuration, and with them `nova_stream_h`,
  which sets the `server: Cowboy/Nova` response header. Another server
  sends its own `server` header, if any.
- `env` in the configuration's `options`, which Cowboy merges into the
  pipeline's starting environment. The environment `start_listener/3`
  gets is Nova's whole.

## The request callbacks

They take and return what the `cowboy_req` functions of the same name do,
so code written against `nova_req` runs on any server:

- `reply/4` sends `resp_headers` and every `resp_cookies` value as
  `set-cookie` along with the call's headers (which win), and a
  `{sendfile, Offset, Length, Path}` body as a file. It returns the request
  without the `resp_*` keys and with `has_sent_resp` set. A `204` or `304`
  response, or a reply to a `HEAD` request, has no body.
- A pipeline that ends with nothing sent is answered `204`.
- `read_body/2`, `read_part/2` and `read_part_body/2` take Cowboy's options
  (`length`, `period`) and exit with `{request_error, Reason, Message}` (or
  `timeout`) when the body can't be read, as Cowboy does. `read_part/2`
  returns the part's headers as a map.
- `upgrade/5` and `upgrade/6` get the module a handler's `init/2` named
  (`cowboy_websocket` for a WebSocket) and run that handler's callbacks the
  way that module does, as Cowboy's `upgrade/4` and `upgrade/5` do.
""".

-export([
         module/0,
         of_req/1,
         execute/2,
         middlewares/1
        ]).

-export_type([listener_ref/0]).

-doc """
The name Nova gives a listener: `nova_listener` for the bootstrap
application's, `{nova_listener, App, Port}` for one started at runtime.
""".
-type listener_ref() :: term().

%% Listeners

-doc """
Start a listener named `Ref` from `Configuration` (the shape of the
`server_configuration` environment key), with `ip` and `port` resolved:
`port` is the one to bind, `ssl_port` for TLS. Every request it serves runs
Nova's pipeline with `Env` as the pipeline's starting environment.
""".
-callback start_listener(Ref :: listener_ref(), Configuration :: map(), Env :: map()) ->
    {ok, pid()} | {error, term()}.

-callback stop_listener(Ref :: listener_ref()) -> ok | {error, term()}.

-doc """
Stop accepting new connections on `Refs` and wait, up to `Timeout`
milliseconds, for the open ones to finish. The listeners are stopped
afterwards with `stop_listener/1`.
""".
-callback drain_listeners(Refs :: [listener_ref()], Timeout :: non_neg_integer()) -> ok.

%% Requests

-callback reply(Status :: cowboy:http_status(), Headers :: cowboy:http_headers(),
                Body :: cowboy_req:resp_body(), Req) -> Req when Req :: cowboy_req:req().

-callback inform(Status :: cowboy:http_status(), Headers :: cowboy:http_headers(),
                 Req :: cowboy_req:req()) -> ok.

-callback stream_reply(Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Req) ->
    Req when Req :: cowboy_req:req().

-callback stream_body(Data :: iodata(), IsFin :: fin | nofin, Req :: cowboy_req:req()) -> ok.

-callback stream_trailers(Trailers :: cowboy:http_headers(), Req :: cowboy_req:req()) -> ok.

-callback read_body(Req, Opts :: cowboy_req:read_body_opts()) ->
    {ok, binary(), Req} | {more, binary(), Req} when Req :: cowboy_req:req().

-callback read_part(Req, Opts :: cowboy_req:read_body_opts()) ->
    {ok, cowboy:http_headers(), Req} | {done, Req} when Req :: cowboy_req:req().

-callback read_part_body(Req, Opts :: cowboy_req:read_body_opts()) ->
    {ok, binary(), Req} | {more, binary(), Req} when Req :: cowboy_req:req().

-doc """
Upgrade the request to the protocol a handler's `init/2` asked for with
`{Mod, Req, State}` (as `cowboy_websocket` for a WebSocket), running
`Handler`'s callbacks for it.
""".
-callback upgrade(Mod :: module(), Req, Env, Handler :: module(), State :: term()) ->
    {ok, Req, Env} | {stop, Req} when Req :: cowboy_req:req(), Env :: map().

-doc """
As `upgrade/5`, for a handler whose `init/2` returned options too:
`{Mod, Req, State, Opts}`.
""".
-callback upgrade(Mod :: module(), Req, Env, Handler :: module(), State :: term(),
                  Opts :: term()) -> {ok, Req, Env} | {stop, Req}
                                         when Req :: cowboy_req:req(), Env :: map().

%%--------------------------------------------------------------------
-doc "The server module Nova's environment names, `nova_server_cowboy` by default.".
-spec module() -> module().
module() ->
    application:get_env(nova, server, nova_server_cowboy).

-doc "The server module serving a request.".
-spec of_req(cowboy_req:req()) -> module().
of_req(#{nova_server := Server}) -> Server;
of_req(_Req) -> nova_server_cowboy.

-doc """
Run Nova's pipeline on a request: the middlewares in `Env`'s
`nova_middlewares` (the listener's `middleware_handlers`), each a
`cowboy_middleware`, in order, until one stops or the last returns.
""".
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
              when Req :: cowboy_req:req(), Env :: map().
execute(Req, #{nova_middlewares := Middlewares} = Env) ->
    execute(Middlewares, Req, Env).

execute([], Req, Env) ->
    {ok, Req, Env};
execute([Middleware | Rest], Req, Env) ->
    case Middleware:execute(Req, Env) of
        {ok, Req1, Env1} -> execute(Rest, Req1, Env1);
        {stop, Req1} -> {stop, Req1}
    end.

-doc "The middlewares a listener configuration runs, Nova's own by default.".
-spec middlewares(Configuration :: map()) -> [module()].
middlewares(Configuration) ->
    maps:get(middleware_handlers, Configuration,
             [
              nova_router, %% Lookup routes
              nova_plugin_handler, %% Handle pre-request plugins
              nova_security_handler, %% Handle security
              nova_handler, %% Controller
              nova_plugin_handler %% Handle post-request plugins
             ]).
