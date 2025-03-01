-module(nova_file_controller).
-export([
         get_file/1,
         get_dir/1
        ]).

-include_lib("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Get the information about a file. If partial content is not supported
%% the accept-ranges header will be set to none.
%% @end
%%--------------------------------------------------------------------
get_file(#{method := <<"HEAD">>, extra_state := #{static := File, options := Options}}) ->
    Filepath = get_filepath(File),
    MimeType = get_mimetype(Filepath, Options),
    case nova:get_env(nova, partial_file_support, true) of
        true ->
            {status, 200, #{<<"accept-ranges">> => <<"bytes">>,
                            <<"content-type">> => MimeType}};
        false ->
            {status, 200, #{<<"accept-ranges">> => <<"none">>,
                            <<"content-type">> => MimeType}}
    end;
get_file(#{extra_state := #{static := File, options := _Options}, headers := Headers} = State) ->
    Filepath = get_filepath(File),
    %% Filepath if the file is cached
    case maps:get(<<"if-none-match">>, Headers, undefined) of
        undefined ->
            maybe_get_range(State);
        Match ->
            {ok, FileHash} = get_filehash(Filepath),
            case Match of
                FileHash ->
                    %% Nothing have changed so send out a 304 Not Modified response
                    {status, 304, #{<<"etag">> => FileHash}};
                _ ->
                    maybe_get_range(State)
            end
    end;
get_file(_Req) ->
    {status, 404}.

get_dir(#{extra_state := #{pathinfo := Pathinfo, static := Dir, options := Options}} = Req) ->
    %% This case will be invoked if a directory was set with wildcard - pathinfo will then
    %% contain the segments of the wildcard value
    Filepath = get_filepath(Dir),
    Filepath0 = lists:foldl(fun(F, Acc) -> filename:join(Acc, binary_to_list(F)) end, Filepath, Pathinfo),
    case filelib:is_dir(Filepath0) of
        false ->
            %% Check if it's a file
            case filelib:is_file(Filepath0) of
                true ->
                    %% It's a file
                    get_file(Req#{extra_state => #{static => {file, Filepath0}, options => Options}});
                false ->
                    {status, 404}
            end;
        true ->
            get_dir(Req#{extra_state => #{static => {dir, Filepath0}, options => Options}})
    end;
get_dir(#{path := Path, extra_state := #{static := Dir, options := Options}}) ->
    Filepath = get_filepath(Dir),
    {ok, Files} = file:list_dir(Filepath),
    case get_index_file(Files, maps:get(index_files, Options, ["index.html"])) of
        {ok, IndexFile} ->
            get_file(#{extra_state => #{static => {file, filename:join(Filepath, IndexFile)}, options => Options}});
        false ->
            case maps:get(list_dir, Options, false) of
                false ->
                    %% We will not show the directory listing
                    {status, 403};
                true ->
                    {ok, Files} = file:list_dir(Filepath),
                    FileInfos = [file_info(Filepath, F) || F <- Files],
                    ParentDir = case re:replace(Path , "/[^/]+/?$", "") of
                                    [[]] -> undefined;
                                    Parent -> Parent
                                end,
                    {ok, #{date => calendar:local_time(), parent_dir => ParentDir, path => Path, files => FileInfos}}
            end
    end;
get_dir(_Req) ->
    {status, 404}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%
maybe_get_range(#{extra_state := #{static := File, options := Options}, headers := Headers}) ->
    %% Check if Range header is present
    Filepath = get_filepath(File),
    {ok, Size} = file_info("", Filepath),
    case maps:get(<<"range">>, Headers, undefined) of
        undefined ->
            MimeType = get_mimetype(Filepath, Options),
            %% No range header, return full file
            {sendfile, 200, #{}, {0, Size, Filepath}, MimeType};

        <<"bytes=", RangeSpec/binary>> ->
            %% Handle Range Request
            case parse_ranges(RangeSpec, Size) of
                {ok, {Start, End}} when Start < Size, End < Size, Start =< End ->
                    MimeType = get_mimetype(Filepath, Options),
                    Length = End - Start + 1,
                    RespHeaders = #{<<"content-range">> => <<"bytes ", (integer_to_binary(Start))/binary, "-",
                                                             (integer_to_binary(End))/binary, "/",
                                                             (integer_to_binary(Size))/binary>>,
                                    <<"content-length">> => integer_to_binary(Length)},
                    {sendfile, 206, RespHeaders, {Start, Length, Filepath}, MimeType};
                _ ->
                    %% Invalid range request
                    {status, 416, #{<<"content-range">> => <<"bytes */", (integer_to_binary(Size))/binary>>}}
            end
    end.

get_mimetype(Filepath, Options) ->
    case maps:get(mimetype, Options, undefined) of
        undefined ->
            {T, V, _} = cow_mimetypes:web(erlang:list_to_binary(Filepath)),
            <<T/binary, "/", V/binary>>;
        MType ->
            MType
    end.


get_filehash(Filepath) ->
    #{last_modified := LastModified, size := Size} = file_info("", Filepath),
    Data = io_lib:format("~s-~p~p", [Filepath, LastModified, Size]),
    Binary = erlang:md5(Data),
    base64:encode_to_string(Binary).

%%--------------------------------------------------------------------
%% @doc
%% Parses the range header value and returns the start and end of the
%% range. If the range is invalid the function will return error.
%% @end
%%--------------------------------------------------------------------
parse_ranges(RangeSpec, FileSize) ->
    case binary:split(RangeSpec, <<",">>, [global]) of
        [L|Tl] = List when length(List) > 1 ->
            %% We have a multiple range request
            case parse_range(L, FileSize) of
                {ok, Range} ->
                    {ok, [Range | parse_ranges(Tl, FileSize)]};
                error ->
                    error
            end;
        [] ->
            {ok, []}
    end.

parse_range(RangeSpec, FileSize) ->
    case binary:split(RangeSpec, <<"-">>, [global]) of
        [StartBin, EndBin] when EndBin =/= <<>> ->
            case {binary_to_integer(StartBin), binary_to_integer(EndBin)} of
                {Start, End} when Start >= 0, End >= Start, End < FileSize ->
                    {ok, {Start, End}};
                _ ->
                    error
            end;
        [StartBin] when StartBin =/= <<>> ->
            case binary_to_integer(StartBin) of
                Start when Start >= 0, Start < FileSize ->
                    {ok, {Start, FileSize - 1}};
                Start when Start < 0 ->
                    {ok, {FileSize - Start - 1, FileSize - 1}};
                _ ->
                    error
            end;
        _ ->
            error
    end.

get_index_file([], _) -> false;
get_index_file([File|Tl], IndexFiles) ->
    case lists:member(File, IndexFiles) of
        true ->
            {ok, File};
        false ->
            get_index_file(Tl, IndexFiles)
    end.


get_filepath({file, LocalFile}) ->
    LocalFile;
get_filepath({priv_file, App, PrivFile}) ->
    filename:join(code:priv_dir(App), PrivFile);
get_filepath({dir, LocalPath}) ->
    LocalPath;
get_filepath({priv_dir, App, LocalPath}) ->
    filename:join(code:priv_dir(App), LocalPath).


file_info(Filepath, Filename) ->
    case file:read_file_info(filename:join(Filepath, Filename)) of
        {ok, #file_info{type = Type, size = Size, mtime = LastModified}} ->
            #{type => Type, size => Size,
              last_modified => LastModified, filename => Filename};
        _ ->
            undefined
    end.
