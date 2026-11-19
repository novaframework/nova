-module(nova_websocket).
-moduledoc false.

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

-doc """
The options of a connection, as `init/1` returns them in
`{ok, State, Options}`: those of `cowboy_websocket`, such as `idle_timeout`
and `max_frame_size`.
""".
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
