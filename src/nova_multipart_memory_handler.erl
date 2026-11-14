%%%-------------------------------------------------------------------
-module(nova_multipart_memory_handler).
-moduledoc """
`nova_multipart_handler` that keeps a file part in memory. Bounded by the plugin's `max_part_size` and `max_total_size`, so use it for small attachments only.

Result: `#{body => Binary}`.
""".
-behaviour(nova_multipart_handler).

-export([init/2, handle_data/2, handle_end/1, handle_abort/2]).

-doc "Starts an empty buffer for the part. `InitArgs` is not used.".
-spec init(nova_multipart_handler:part_info(), InitArgs :: term()) -> {ok, binary()}.
init(_PartInfo, _InitArgs) ->
    {ok, <<>>}.

-doc "Appends the chunk to the buffer.".
-spec handle_data(Data :: binary(), Acc :: binary()) -> {ok, binary()}.
handle_data(Data, Acc) ->
    {ok, <<Acc/binary, Data/binary>>}.

-doc "Returns the part as `{ok, #{body => Binary}}`.".
-spec handle_end(Acc :: binary()) -> {ok, #{body := binary()}}.
handle_end(Acc) ->
    {ok, #{body => Acc}}.

-doc "Drops the buffer of a part that was not finished.".
-spec handle_abort(Reason :: term(), Acc :: binary()) -> ok.
handle_abort(_Reason, _Acc) ->
    ok.
