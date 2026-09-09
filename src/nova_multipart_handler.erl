%%%-------------------------------------------------------------------
%%% @doc
%%% Behaviour for a `nova_multipart_plugin' part sink. Configured as
%%% `{Mod, InitArgs}', never a raw fun/0 - the callback crosses the
%%% plugin<->handler module boundary and needs to survive being carried in
%%% app config.
%%%
%%% Every chunk handed to `handle_data/2' comes straight off the socket -
%%% the plugin never buffers a part body itself, so a handler that spools to
%%% disk (see `nova_multipart_file_handler') keeps memory flat regardless of
%%% upload size.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_multipart_handler).

-export_type([part_info/0]).

-type part_info() :: #{name := binary(),
                        filename := binary(),
                        content_type := binary()}.

%% Called once a file part's headers are known. Every field of `PartInfo' -
%% `filename', `content_type' and `name' - is attacker controlled (the
%% client sets all three; `content_type' even defaults to `<<"text/plain">>'
%% when the client omits it). A handler must never use `filename' to build a
%% filesystem path, and must not treat `content_type' as a trustworthy basis
%% for a type/extension decision or echo it back verbatim as a response
%% header (a stored-content-type reflected as `Content-Type: text/html' is a
%% stored XSS vector). Returning {error, Reason} rejects the part with a 400
%% and the plugin never calls handle_data/handle_end for it.
-callback init(PartInfo :: part_info(), InitArgs :: term()) ->
    {ok, HandlerState :: term()} | {error, Reason :: term()}.

%% Called for every chunk read off the wire, in order, as soon as it
%% arrives. {error, Reason} aborts the part (the plugin calls handle_abort/2
%% and replies 500) - a handler that hits a write error should return here
%% rather than raise, so cleanup is guaranteed.
-callback handle_data(Data :: binary(), HandlerState :: term()) ->
    {ok, NewHandlerState :: term()} | {error, Reason :: term()}.

%% Called once after the last chunk of a part. The returned `Result' is what
%% ends up in the `files' list the plugin puts on the request - typically a
%% reference to where the data landed (a path, an object-store key), not the
%% data itself.
-callback handle_end(HandlerState :: term()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

%% Called whenever a part is abandoned mid-stream: oversize, over budget,
%% a handle_data/handle_end error, a malformed part, or the read deadline
%% expiring. Must be idempotent-safe cleanup (e.g. close + delete a partial
%% file) and must not raise - the plugin is already on an error path.
-callback handle_abort(Reason :: term(), HandlerState :: term()) -> ok.
