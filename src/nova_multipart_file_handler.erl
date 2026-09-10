%%%-------------------------------------------------------------------
%%% @doc
%%% `nova_multipart_handler' that streams a file part to disk under a random
%%% name in `InitArgs' `dir'. The client filename is never used in the path.
%%% The part is written as `<name>.part' and renamed on handle_end/1, so a
%%% file at its final name is always complete. Sweep `dir' for stale
%%% `*.part' files left by a process crash.
%%%
%%% Result: `#{path => Path}'.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_multipart_file_handler).
-behaviour(nova_multipart_handler).

-export([init/2, handle_data/2, handle_end/1, handle_abort/2]).

-spec init(nova_multipart_handler:part_info(), InitArgs :: #{dir := file:filename_all()}) ->
          {ok, HandlerState :: map()} | {error, Reason :: term()}.
init(_PartInfo, #{dir := Dir}) ->
    Name = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    TmpPath = filename:join(Dir, <<Name/binary, ".part">>),
    FinalPath = filename:join(Dir, Name),
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
