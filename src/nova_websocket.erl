-module(nova_websocket).

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

-callback init(State :: map()) -> {ok, State0 :: map()} | any().
-callback websocket_init(State :: map()) -> Result :: call_result().
-optional_callbacks([websocket_init/1]).

-callback websocket_handle(Frame :: in_frame(), State :: map()) -> call_result().
-callback websocket_info(Info :: any(), State :: map()) -> call_result().
-callback terminate(Reason :: reason(), PartialReq :: map(), State :: map()) -> ok.
-optional_callbacks([terminate/3]).
