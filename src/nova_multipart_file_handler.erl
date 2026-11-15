%%%-------------------------------------------------------------------
-module(nova_multipart_file_handler).
-moduledoc """
`nova_multipart_handler` that streams a file part to disk under a random name in `InitArgs``dir`. The client filename is never used in the path. The part is written as `<name>.part` and renamed on handle_end/1, so a file at its final name is always complete.

The file belongs to the request. It is deleted when the request is over, whatever the outcome, unless the controller took it over with `nova_multipart_spool:keep/1` or moved it elsewhere. A part left behind by a client that went away is deleted the same way. `nova_multipart_spool` also bounds how many files and bytes are spooled at one time; a part past the bound gets `{error, spool_full}`.

Only a client extension listed in `extensions` is kept on the stored name, lowercased. It is an allowlist because `dir` may be served by a web server, where a stored `.html` or `.svg` is stored XSS. Anything else is dropped, not rejected: the extension is client controlled and says nothing about the bytes.

Result: `#{path => Path}`.
""".
-behaviour(nova_multipart_handler).

-export([init/2, handle_data/2, handle_end/1, handle_abort/2]).

-doc """
The `InitArgs` of the handler: `dir`, the directory the files are written
to, and `extensions`, the extensions that are kept on the stored name, as
lowercase binaries without the dot. `extensions` defaults to none.
""".
-type init_args() :: #{dir := file:filename_all(),
                       extensions => [binary()]}.

-export_type([init_args/0]).

-doc """
Opens a new file with a random name in `dir` for the part. Returns
`{error, spool_full}` when the spool is at its limit, and `{error, Reason}`
when the file cannot be opened.
""".
-spec init(nova_multipart_handler:part_info(), InitArgs :: init_args()) ->
          {ok, HandlerState :: map()} | {error, Reason :: term()}.
init(#{filename := Filename}, #{dir := Dir} = InitArgs) ->
    Name = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Ext = extension(Filename, maps:get(extensions, InitArgs, [])),
    FinalPath = filename:join(Dir, <<Name/binary, Ext/binary>>),
    TmpPath = filename:join(Dir, <<Name/binary, Ext/binary, ".part">>),
    case nova_multipart_spool:track(TmpPath) of
        ok ->
            case file:open(TmpPath, [write, exclusive, binary]) of
                {ok, Fd} ->
                    {ok, #{fd => Fd, tmp_path => TmpPath, final_path => FinalPath}};
                {error, _} = Error ->
                    _ = nova_multipart_spool:discard(TmpPath),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-doc """
Appends the chunk to the file. Returns `{error, spool_full}` when the chunk
takes the spool past its limit, and `{error, Reason}` when the write fails.
""".
-spec handle_data(Data :: binary(), HandlerState :: map()) ->
          {ok, NewHandlerState :: map()} | {error, Reason :: term()}.
handle_data(Data, #{fd := Fd, tmp_path := TmpPath} = State) ->
    case nova_multipart_spool:grow(TmpPath, byte_size(Data)) of
        ok ->
            case file:write(Fd, Data) of
                ok -> {ok, State};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

-doc """
Closes the file and gives it its final name. Returns `{ok, #{path => Path}}`
with that name, or `{error, Reason}` after deleting the file when it cannot
be closed or renamed.
""".
-spec handle_end(HandlerState :: map()) ->
          {ok, #{path := file:filename_all()}} | {error, Reason :: term()}.
handle_end(#{fd := Fd, tmp_path := TmpPath, final_path := FinalPath}) ->
    case file:close(Fd) of
        ok ->
            case nova_multipart_spool:rename(TmpPath, FinalPath) of
                ok ->
                    {ok, #{path => FinalPath}};
                {error, _} = Error ->
                    _ = nova_multipart_spool:discard(TmpPath),
                    Error
            end;
        {error, _} = Error ->
            _ = nova_multipart_spool:discard(TmpPath),
            Error
    end.

-doc "Closes and deletes the file of a part that was not finished.".
-spec handle_abort(Reason :: term(), HandlerState :: map()) -> ok.
handle_abort(_Reason, #{fd := Fd, tmp_path := TmpPath}) ->
    _ = file:close(Fd),
    _ = nova_multipart_spool:discard(TmpPath),
    ok.

%% Only the extension survives, and only via the allowlist: the filename is
%% otherwise never looked at, so `../../x.png' contributes exactly `.png'.
-spec extension(Filename :: binary(), Allowed :: [binary()]) -> binary().
extension(_Filename, []) ->
    ~"";
extension(Filename, Allowed) ->
    case filename:extension(Filename) of
        <<$., Ext/binary>> when byte_size(Ext) =< 16 ->
            Lower = <<<<(lower(C))>> || <<C>> <= Ext>>,
            case lists:member(Lower, Allowed) andalso is_token(Lower) of
                true -> <<$., Lower/binary>>;
                false -> ~""
            end;
        _ ->
            ~""
    end.

-spec lower(byte()) -> byte().
lower(C) when C >= $A, C =< $Z -> C + 32;
lower(C) -> C.

-spec is_token(binary()) -> boolean().
is_token(Bin) ->
    lists:all(fun(C) -> (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) end,
              binary_to_list(Bin)).
