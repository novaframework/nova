-module({{modname}}_ws_controller).
-export([
         init/1,
         websocket_init/0,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).


%%--------------------------------------------------------------------
%% @doc
%% Initialize the HTTP-request to upgrade into websocket.
%% @end
%%--------------------------------------------------------------------
-spec init(State :: map()) -> {ok, State0 :: map()} | any().
init(State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Initialize the websocket connection
%% @end
%%--------------------------------------------------------------------
-spec websocket_init(State :: map()) -> CallResult :: nova_websocket:call_result().
websocket_init(State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @doc
%% Handle received frame from websocket
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle(Frame :: nova_websocket:in_frame(), State :: map()) ->
                              nova_websocket:call_result().
websocket_handle(_Frame, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handle received message from Erlang that's not sent via websocket
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(Info :: any(), State :: map()) -> nova_websocket:call_result().
websocket_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminate the server
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: nova_websocket:reason(), PartialReq :: map(), State :: map() -> ok.
terminate(_Reason, _PartialReq, _State) ->
    ok.
