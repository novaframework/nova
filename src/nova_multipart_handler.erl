%%%-------------------------------------------------------------------
%%% @doc
%%% Behaviour for a `nova_multipart_plugin' file sink, configured as
%%% `{Mod, InitArgs}'. Chunks arrive straight off the socket, so a handler
%%% that writes each one away keeps memory flat regardless of upload size.
%%%
%%% Every field of `part_info()' is client controlled. Never build a
%%% filesystem path from `filename' and never trust `content_type'.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_multipart_handler).

-export_type([part_info/0]).

-type part_info() :: #{name := binary(),
                        filename := binary(),
                        content_type := binary()}.

%% `{reject, Reason}' answers 400 (the part is unacceptable), `{error, Reason}'
%% answers 500 (the sink failed). Neither leads to handle_data/handle_end.
-callback init(PartInfo :: part_info(), InitArgs :: term()) ->
    {ok, HandlerState :: term()} | {reject, Reason :: term()} | {error, Reason :: term()}.

%% `{error, Reason}' aborts the part: handle_abort/2 runs and the client gets 500.
-callback handle_data(Data :: binary(), HandlerState :: term()) ->
    {ok, NewHandlerState :: term()} | {error, Reason :: term()}.

%% `Result' ends up under `result' in the request's `files' list.
-callback handle_end(HandlerState :: term()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

%% Cleanup for a part abandoned mid-stream. Must not raise.
-callback handle_abort(Reason :: term(), HandlerState :: term()) -> ok.
