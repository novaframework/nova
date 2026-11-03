-module(nova_websocket).
-moduledoc """
Behaviour for websocket controllers. Route to one with `protocol => ws`:

```erlang
{"/ws", my_ws_controller, #{protocol => ws}}
```

`init/1` runs in the request process before the upgrade. It gets the
controller data (with `req` set to the upgrade request, and `auth_data` when
the route's security function returned `{true, AuthData}`) and returns
`{ok, State}` to accept the connection. Anything else answers the upgrade
request with a 500.

After the upgrade, `websocket_init/1`, `websocket_handle/2` and
`websocket_info/2` return one of:

- `{ok, State}`, `{ok, State, hibernate}`
- `{reply, Frame | [Frame], State}`, `{reply, Frame | [Frame], State, hibernate}`
- `{stop, State}`, which closes the connection

`terminate/3` is called with the reason, the partial request and the state
when the connection goes away.

The upgrade request runs the route's `pre_request` plugins like any HTTP
request. After the upgrade, plugins listed under `pre_ws_request`,
`post_ws_request` and `post_ws_connection` are called as
`Module:Phase(ControllerState, State, Options)` and return `{ok,
ControllerState, State}`, `{break, State}` (skip the rest of the phase),
`{stop, State}` (close the connection) or `{error, Reason}` (close it and log).
This is not the `nova_plugin` contract.

```erlang
-module(my_ws_controller).
-behaviour(nova_websocket).
-export([init/1, websocket_handle/2, websocket_info/2]).

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, ~"ping"}, State) ->
    {reply, {text, ~"pong"}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({broadcast, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Msg, State) ->
    {ok, State}.
```
""".

-type call_result() :: {ok, State :: map()} |
                       {ok, State :: map(), hibernate} |
                       {reply, OutFrame :: cow_ws:frame() | [OutFrame :: cow_ws:frame()],
                        State :: map()} |
                       {reply, OutFrame :: cow_ws:frame() | [OutFrame :: cow_ws:frame()],
                        State :: map(), hibernate} |
                       {stop, State :: map()}.
-export_type([call_result/0]).

-type in_frame() :: ping | pong | {text | binary | ping | pong, binary()}.
-export_type([in_frame/0]).

-type reason() ::  normal | stop | timeout | remote |
                   {remote, cow_ws:close_code(), binary()} |
                   {error, badencoding | badframe | closed | atom()} |
                   {crash, error | exit | throw, any()}.
-export_type([reason/0]).

-callback init(State :: map()) -> {ok, State0 :: map()} | any().
-callback websocket_init(State :: map()) -> Result :: call_result().
-optional_callbacks([websocket_init/1]).

-callback websocket_handle(Frame :: in_frame(), State :: map()) -> call_result().
-callback websocket_info(Info :: any(), State :: map()) -> call_result().
-callback terminate(Reason :: reason(), PartialReq :: map(), State :: map()) -> ok.
-optional_callbacks([terminate/3]).
