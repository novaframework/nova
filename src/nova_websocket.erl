-module(nova_websocket).
-moduledoc """
Behaviour for websocket controllers. Route to one with `protocol => ws`:

```erlang
{"/ws", my_ws_controller, #{protocol => ws}}
```

`init/1` runs in the request process before the upgrade. It gets the
controller data (with `req` set to the upgrade request, and `auth_data` set
to the `auth_data` of that request when it has one, which is what a
controller of an HTTP route finds in its request) and returns
`{ok, State}` to accept the connection. Any other return refuses the upgrade
and is logged as an error; the client gets a `500`, rendered by the status
page route for `500` when the application has one.

After the upgrade, `websocket_init/1`, `websocket_handle/2` and
`websocket_info/2` return one of:

- `{ok, State}`, `{ok, State, hibernate}`
- `{reply, Frame | [Frame], State}`, `{reply, Frame | [Frame], State, hibernate}`
- `{stop, State}`, which closes the connection

`terminate/3` is optional. It is called with the reason, the partial request
and the state when the connection goes away. A crash in it is logged and does
not stop the `post_ws_connection` plugins from running.

A route can hand the controller a term of its own with `extra_state`, set on
the route or on its route group. It is found in the upgrade request, under
`req` in the controller data:

```erlang
{"/ws", my_ws_controller, #{protocol => ws, extra_state => #{room => lobby}}}
```

The upgrade request runs the route's `pre_request` plugins like any HTTP
request. `nova_ws_handler` has plugin phases of its own for the time after
the upgrade: `pre_ws_request` and `post_ws_request` run around every
`websocket_init/1`, `websocket_handle/2` and `websocket_info/2` call, and
`post_ws_connection` runs once when the connection goes away, after
`terminate/3` and whether or not the controller exports it. It does not run
for an upgrade that was refused. A plugin exports the phase as a
function of `(ControllerState, State, Options)` and returns
`{ok, ControllerState, State}`, `{break, State}`, `{stop, State}` or
`{error, Reason}`.

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

%% The options of the connection. They are Cowboy's today; the callback
%% names Nova's type so that can change without changing the callback.
-type opts() :: cowboy_websocket:opts().
-export_type([opts/0]).

-callback init(State :: map()) -> {ok, State0 :: map()} |
                                  {ok, State0 :: map(), WebsocketOptions :: opts()} |
                                  any().
-callback websocket_init(State :: map()) -> Result :: call_result().
-optional_callbacks([websocket_init/1]).

-callback websocket_handle(Frame :: in_frame(), State :: map()) -> call_result().
-callback websocket_info(Info :: any(), State :: map()) -> call_result().
-callback terminate(Reason :: reason(), PartialReq :: map(), State :: map()) -> ok.
-optional_callbacks([terminate/3]).
