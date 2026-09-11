%%%-------------------------------------------------------------------
%%% @doc
%%% `nova_multipart_handler' that streams a file part to disk under a random
%%% name in `InitArgs' `dir'. The client filename is never used in the path.
%%% The part is written as `<name>.part' and renamed on handle_end/1, so a
%%% file at its final name is always complete. Sweep `dir' for stale
%%% `*.part' files left by a process crash.
%%%
%%% Only a client extension listed in `extensions' is kept on the stored
%%% name, lowercased. It is an allowlist because `dir' may be served by a
%%% web server, where a stored `.html' or `.svg' is stored XSS. Anything
%%% else is dropped, not rejected: the extension is client controlled and
%%% says nothing about the bytes.
%%%
%%% Result: `#{path => Path}'.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_multipart_file_handler).
-behaviour(nova_multipart_handler).

-export([init/2, handle_data/2, handle_end/1, handle_abort/2]).

-type init_args() :: #{dir := file:filename_all(),
                       extensions => [binary()]}.

-export_type([init_args/0]).

-spec init(nova_multipart_handler:part_info(), InitArgs :: init_args()) ->
          {ok, HandlerState :: map()} | {error, Reason :: term()}.
init(#{filename := Filename}, #{dir := Dir} = InitArgs) ->
    Name = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Ext = extension(Filename, maps:get(extensions, InitArgs, [])),
    FinalPath = filename:join(Dir, <<Name/binary, Ext/binary>>),
    TmpPath = filename:join(Dir, <<Name/binary, Ext/binary, ".part">>),
    case file:open(TmpPath, [write, exclusive, binary]) of
        {ok, Fd} ->
            {ok, #{fd => Fd, tmp_path => TmpPath, final_path => FinalPath}};
        {error, _} = Error ->
            Error
    end.

-spec handle_data(Data :: binary(), HandlerState :: map()) ->
          {ok, NewHandlerState :: map()} | {error, Reason :: term()}.
handle_data(Data, #{fd := Fd} = State) ->
    case file:write(Fd, Data) of
        ok -> {ok, State};
        {error, _} = Error -> Error
    end.

-spec handle_end(HandlerState :: map()) ->
          {ok, #{path := file:filename_all()}} | {error, Reason :: term()}.
handle_end(#{fd := Fd, tmp_path := TmpPath, final_path := FinalPath}) ->
    case file:close(Fd) of
        ok ->
            case file:rename(TmpPath, FinalPath) of
                ok ->
                    {ok, #{path => FinalPath}};
                {error, _} = Error ->
                    _ = file:delete(TmpPath),
                    Error
            end;
        {error, _} = Error ->
            _ = file:delete(TmpPath),
            Error
    end.

-spec handle_abort(Reason :: term(), HandlerState :: map()) -> ok.
handle_abort(_Reason, #{fd := Fd, tmp_path := TmpPath}) ->
    _ = file:close(Fd),
    _ = file:delete(TmpPath),
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
