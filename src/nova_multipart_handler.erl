%%%-------------------------------------------------------------------
-module(nova_multipart_handler).
-moduledoc """
Behaviour for a `nova_multipart_plugin` file sink, configured as `{Mod, InitArgs}`. Chunks arrive straight off the socket, so a handler that writes each one away keeps memory flat regardless of upload size.

Every field of `part_info()` is client controlled. Never build a filesystem path from `filename` and never trust `content_type`.

A handler that writes files should hand them to `nova_multipart_spool`, as `nova_multipart_file_handler` does, so they are deleted with the request and count towards the spool limits. `{error, spool_full}` from `init/2` or `handle_data/2` is answered with a `503`, any other error with a `500`.
""".

-export_type([part_info/0]).

-doc """
What the client said about a file part: the `name` of the form field, the
`filename` and the `content_type`. All three come from the client.
""".
-type part_info() :: #{name := binary(),
                        filename := binary(),
                        content_type := binary()}.

-doc """
Called once for every file part, before its first chunk, with the
`InitArgs` of the plugin configuration.

`{ok, HandlerState}` accepts the part. `{reject, Reason}` refuses it and the
request is answered with a `400`. `{error, Reason}` says the sink failed: the
request is answered with a `503` for `spool_full` and a `500` otherwise.
After a refusal or an error no other callback is called for the part.

An exception in `init/2`, `handle_data/2` or `handle_end/1`, or a return that
is none of those documented, is logged and handled as
`{error, handler_crashed}`.
""".
-callback init(PartInfo :: part_info(), InitArgs :: term()) ->
    {ok, HandlerState :: term()} | {reject, Reason :: term()} | {error, Reason :: term()}.

-doc """
Called for every chunk of the part, in the order they are read.

`{ok, NewHandlerState}` goes on. `{error, Reason}` aborts the part:
`handle_abort/2` is called and the request is answered with a `503` for
`spool_full` and a `500` otherwise.
""".
-callback handle_data(Data :: binary(), HandlerState :: term()) ->
    {ok, NewHandlerState :: term()} | {error, Reason :: term()}.

-doc """
Called after the last chunk of the part.

`{ok, Result}` puts `Result` under `result` in the entry of the part in the
`files` list of the request. `{error, Reason}` aborts the part:
`handle_abort/2` is called and the request is answered with a `500`.
""".
-callback handle_end(HandlerState :: term()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

-doc """
Called for a part that was accepted and is not finished: it went past a size
limit (`too_large`), was not read in time (`timeout`), could not be read, or
`handle_data/2` or `handle_end/1` returned an error. Clean up what the part
left behind. The return value is not used, and an exception is logged.
""".
-callback handle_abort(Reason :: term(), HandlerState :: term()) -> ok.
