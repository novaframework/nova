-module(nova_file_controller).
-moduledoc false.
-export([
         get_file/1,
         get_dir/1,
         static_state/2
        ]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

-define(MAX_SYMLINKS, 40).

-type static() :: {file, file:name_all()} | {priv_file, atom(), file:name_all()} |
                  {dir, file:name_all()} | {priv_dir, atom(), file:name_all()}.

-doc false.
-spec static_state(Static :: static(), Options :: map()) -> map().
static_state({dir, _LocalPath} = Dir, Options) ->
    directory_state(Dir, Options);
static_state({priv_dir, _App, _LocalPath} = Dir, Options) ->
    directory_state(Dir, Options);
static_state({file, LocalFile}, Options) ->
    #{static => {file, filename:absname(LocalFile)}, options => Options};
static_state(File, Options) ->
    #{static => File, options => Options}.

directory_state(Dir, Options) ->
    Root = get_filepath(Dir),
    #{static => Dir, options => Options, static_root => {Root, resolve_root(Root, Options)}}.

get_file(#{extra_state := #{static := File, options := Options}, headers := Headers}) ->
    Filepath = get_filepath(File),
    MimeType =
        case maps:get(mimetype, Options, undefined) of
            undefined ->
                mimetype(unicode:characters_to_binary(Filepath));
            MType ->
                MType
        end,

    case file:read_file_info(Filepath) of
        {ok, #file_info{size = Size}} ->
            serve_file(Filepath, Size, MimeType, Headers);
        {error, Reason} ->
            unusable_file(Filepath, Reason),
            {status, 404}
    end;
get_file(_Req) ->
    {status, 404}.

serve_file(Filepath, Size, MimeType, Headers) ->
    %% Check if Range header is present
    case maps:get(<<"range">>, Headers, undefined) of
        undefined ->
            %% No range header, return full file
            {sendfile, 200, #{}, {0, Size, Filepath}, MimeType};

        <<"bytes=", RangeSpec/binary>> ->
            %% Handle Range Request
            case parse_range(RangeSpec, Size) of
                {ok, {Start, End}} when Start < Size, End < Size, Start =< End ->
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

get_dir(#{extra_state := #{static := Dir, options := Options} = ExtraState} = Req) ->
    Root = get_filepath(Dir),
    case static_root(ExtraState, Root, Options) of
        error ->
            unusable_root(Root, unresolved),
            {status, 404};
        Resolved ->
            serve_dir(Resolved, Req)
    end;
get_dir(_Req) ->
    {status, 404}.

serve_dir(Resolved, #{extra_state := #{pathinfo := Pathinfo, options := Options}} = Req) ->
    %% This case will be invoked if a directory was set with wildcard - pathinfo will then
    %% contain the segments of the wildcard value
    case path_inside_root(Resolved, Pathinfo, Options) of
        {ok, Filepath0, FileResolved} ->
            case filelib:is_dir(Filepath0) of
                false ->
                    case filelib:is_file(Filepath0) of
                        true ->
                            get_file(Req#{extra_state => #{static => {file, Filepath0}, options => Options}});
                        false ->
                            not_found(Resolved)
                    end;
                true ->
                    get_dir(Req#{extra_state => #{static => {dir, Filepath0}, static_root => {Filepath0, FileResolved},
                                                  options => Options}})
            end;
        error ->
            not_found(Resolved)
    end;
serve_dir({_RootSegments, DirSegments} = Resolved, #{path := Path, extra_state := #{options := Options}} = Req) ->
    Filepath = filename:join(DirSegments),
    case file:list_dir(Filepath) of
        {ok, Files} ->
            serve_index(Files, Filepath, Resolved, Path, Options, Req);
        {error, Reason} ->
            unusable_root(Filepath, Reason),
            {status, 404}
    end;
serve_dir(_Resolved, _Req) ->
    {status, 404}.

serve_index(Files, Filepath, Resolved, Path, Options, Req) ->
    case get_index_file(Files, maps:get(index_files, Options, ["index.html"])) of
        {ok, IndexFile} ->
            case inside_root(Resolved, [IndexFile], Options) of
                {ok, {_, IndexSegments}} ->
                    IndexPath = filename:join(IndexSegments),
                    get_file(Req#{extra_state => #{static => {file, IndexPath}, options => Options}});
                error ->
                    {status, 404}
            end;
        false ->
            case maps:get(list_dir, Options, false) of
                false ->
                    %% We will not show the directory listing
                    {status, 403};
                true ->
                    FileInfos = [file_info(Filepath, F) || F <- Files],
                    ParentDir = case re:replace(Path , "/[^/]+/?$", "") of
                                    [[]] -> undefined;
                                    Parent -> Parent
                                end,
                    {ok, #{date => calendar:local_time(), parent_dir => ParentDir, path => Path, files => FileInfos}}
            end
    end.

not_found({RootSegments, _DirSegments}) ->
    Root = filename:join(RootSegments),
    case file:read_file_info(Root) of
        {ok, _Info} -> ok;
        {error, Reason} -> unusable_root(Root, Reason)
    end,
    {status, 404}.

unusable_root(Directory, Reason) ->
    Key = {?MODULE, unusable_root, Directory},
    case nova_once:first(Key) of
        true ->
            ?LOG_ERROR(#{msg => ~"Static directory cannot be served", directory => Directory, reason => Reason});
        false ->
            ok
    end.

unusable_file(File, Reason) ->
    Key = {?MODULE, unusable_file, File},
    case nova_once:first(Key) of
        true ->
            ?LOG_ERROR(#{msg => ~"Static file cannot be served", file => File, reason => Reason});
        false ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

path_inside_root(Resolved, Pathinfo, Options) ->
    case lists:all(fun safe_segment/1, Pathinfo) of
        true ->
            Segments = [binary_to_list(Segment) || Segment <- Pathinfo],
            case inside_root(Resolved, Segments, Options) of
                {ok, {_, FileSegments} = FileResolved} -> {ok, filename:join(FileSegments), FileResolved};
                error -> error
            end;
        false ->
            error
    end.

safe_segment(<<>>)     -> false;
safe_segment(<<".">>)  -> false;
safe_segment(<<"..">>) -> false;
safe_segment(Segment) when is_binary(Segment) ->
    binary:match(Segment, [<<"/">>, <<"\\">>, <<0>>]) =:= nomatch;
safe_segment(_Segment) ->
    false.

inside_root({RootSegments, DirSegments}, Segments, Options) ->
    case canonical_path(Segments, lists:reverse(DirSegments), resolve_links(Options), ?MAX_SYMLINKS) of
        {ok, FileSegments} ->
            case lists:prefix(RootSegments, FileSegments) of
                true -> {ok, {RootSegments, FileSegments}};
                false -> error
            end;
        error ->
            error
    end;
inside_root(error, _Segments, _Options) ->
    error.

static_root(#{static_root := {Dir, Resolved}}, Dir, _Options) ->
    Resolved;
static_root(_ExtraState, Dir, Options) ->
    resolve_root(Dir, Options).

resolve_root(Dir, Options) ->
    case canonical_path(Dir, resolve_links(Options)) of
        {ok, Segments} -> {Segments, Segments};
        error -> error
    end.

resolve_links(Options) ->
    not maps:get(follow_external_symlinks, Options, false).

canonical_path(Path, ResolveLinks) ->
    canonical_path(filename:split(filename:absname(Path)), [], ResolveLinks, ?MAX_SYMLINKS).

canonical_path([], Resolved, _ResolveLinks, _LinksLeft) ->
    {ok, lists:reverse(Resolved)};
canonical_path(["." | Rest], Resolved, ResolveLinks, LinksLeft) ->
    canonical_path(Rest, Resolved, ResolveLinks, LinksLeft);
canonical_path([".." | Rest], [_Volume] = Resolved, ResolveLinks, LinksLeft) ->
    canonical_path(Rest, Resolved, ResolveLinks, LinksLeft);
canonical_path([".." | Rest], [_Parent | Resolved], ResolveLinks, LinksLeft) ->
    canonical_path(Rest, Resolved, ResolveLinks, LinksLeft);
canonical_path([Segment | Rest], Resolved, false, LinksLeft) ->
    canonical_path(Rest, [Segment | Resolved], false, LinksLeft);
canonical_path([Segment | Rest], Resolved, true, LinksLeft) ->
    case file:read_link_all(filename:join(lists:reverse([Segment | Resolved]))) of
        {ok, Target} when is_list(Target), LinksLeft > 0 ->
            case filename:pathtype(Target) of
                absolute ->
                    canonical_path(filename:split(Target) ++ Rest, [], true, LinksLeft - 1);
                _Relative ->
                    canonical_path(filename:split(Target) ++ Rest, Resolved, true, LinksLeft - 1)
            end;
        {ok, _Target} ->
            error;
        {error, _NotALink} ->
            canonical_path(Rest, [Segment | Resolved], true, LinksLeft)
    end.

parse_range(RangeSpec, FileSize) ->
    %% Parse the "bytes=" range header value
    case binary:split(RangeSpec, <<"-">>, [global]) of
        [StartBin, EndBin] when EndBin =/= <<>> ->
            case {binary_to_integer(StartBin), binary_to_integer(EndBin)} of
                {Start, End} when Start >= 0, End >= Start, End < FileSize ->
                    {ok, {Start, End}};
                _ ->
                    error
            end;
        [StartBin, <<>>] when StartBin =/= <<>> ->
            case binary_to_integer(StartBin) of
                Start when Start >= 0, Start < FileSize ->
                    {ok, {Start, FileSize - 1}};
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

mimetype(Path) ->
    {T, V, _} = case cow_mimetypes:web(Path) of
                    {~"application", ~"octet-stream", _} = Default -> fallback(cow_mimetypes:all(Path), Default);
                    Type -> Type
                end,
    <<T/binary, "/", V/binary>>.

fallback({_, SubType, _} = Type, Default) ->
    case document_type(SubType) of
        true -> Default;
        false -> Type
    end.

document_type(~"xml") -> true;
document_type(~"html") -> true;
document_type(SubType) -> binary:longest_common_suffix([SubType, ~"+xml"]) =:= 4.
