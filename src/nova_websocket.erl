-module(nova_websocket).

-type call_result() :: {ok, State :: any()} |
                       {ok, State :: any(), hibernate} |
                       {reply, OutFrame :: cow_ws:frame() | [OutFrame :: cow_ws:frame()],
                        State :: any()} |
                       {reply, OutFrame :: cow_ws:frame() | [OutFrame :: cow_ws:frame()],
                        State :: any(), hibernate} |
                       {stop, State :: any()}.
-export_type([call_result/0]).

-type in_frame() :: ping | pong | {text | binary | ping | pong, binary()}.
-export_type([in_frame/0]).

-type reason() ::  normal | stop | timeout | remote |
                   {remote, cow_ws:close_code(), binary()} |
                   {error, badencoding | badframe | closed | atom()} |
                   {crash, error | exit | throw, any()}.

-callback websocket_init() -> Result :: call_result().
-optional_callbacks([websocket_init/0]).

-callback websocket_handle(Frame :: in_frame(), State :: any()) -> call_result().
-callback websocket_info(Info :: any(), State :: any()) -> call_result().
-callback terminate(Reason :: reason(), PartialReq :: map(), State :: any()) -> ok.
-optional_callbacks([terminate/3]).
