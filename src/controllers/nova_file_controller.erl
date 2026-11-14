-module(nova_file_controller).
-moduledoc false.
-export([
         get_file/1,
         get_dir/1,
         static_state/2
        ]).

-include_lib("kernel/include/file.hrl").

-define(MAX_SYMLINKS, 40).

%% The extra_state of a static route, for the router. The root of a
%% directory is resolved here, when the route is compiled, so a request only
%% has to resolve the segments that come after it.
-doc false.
-spec static_state(Static :: tuple(), Options :: map()) -> map().
static_state({dir, _LocalPath} = Dir, Options) ->
    directory_state(Dir, Options);
static_state({priv_dir, _App, _LocalPath} = Dir, Options) ->
    directory_state(Dir, Options);
static_state(File, Options) ->
    #{static => File, options => Options}.

directory_state(Dir, Options) ->
    Root = get_filepath(Dir),
    #{static => Dir, options => Options, static_root => {Root, resolve_root(Root, Options)}}.

%% No ETag or Last-Modified is sent with a file, so an If-Range has nothing
%% to match and the range it guards is not used (RFC 9110 section 13.1.5).
get_file(#{headers := #{<<"range">> := Range} = Headers} = Req) ->
    case is_map_key(~"if-range", Headers) orelse ignored_range(Range) of
        true -> serve_file(Req#{headers => maps:remove(<<"range">>, Headers)});
        false -> serve_file(Req)
    end;
get_file(Req) ->
    serve_file(Req).

serve_file(#{extra_state := #{static := File, options := Options}, headers := Headers}) ->
    Filepath = get_filepath(File),
    MimeType =
        case maps:get(mimetype, Options, undefined) of
            undefined ->
                {T, V, _} = cow_mimetypes:web(erlang:list_to_binary(Filepath)),
                <<T/binary, "/", V/binary>>;
            MType ->
                MType
        end,

    %% Fetch file size
    #{size := Size} = file_info(filename:dirname(Filepath), filename:basename(Filepath)),

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
                error ->
                    {sendfile, 200, #{}, {0, Size, Filepath}, MimeType};
                _ ->
                    {status, 416, #{<<"content-range">> => <<"bytes */", (integer_to_binary(Size))/binary>>}}
            end
    end;

serve_file(_Req) ->
    {status, 404}.

get_dir(#{extra_state := #{pathinfo := Pathinfo, static := Dir, options := Options} = ExtraState} = Req) ->
    %% This case will be invoked if a directory was set with wildcard - pathinfo will then
    %% contain the segments of the wildcard value
    Root = get_filepath(Dir),
    case path_inside_root(static_root(ExtraState, Root, Options), Root, Pathinfo, Options) of
        {ok, Filepath0, Resolved} ->
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
                    get_dir(Req#{extra_state => #{static => {dir, Filepath0}, static_root => {Filepath0, Resolved},
                                                  options => Options}})
            end;
        error ->
            {status, 404}
    end;
get_dir(#{path := Path, extra_state := #{static := Dir, options := Options} = ExtraState} = Req) ->
    Filepath = get_filepath(Dir),
    {ok, Files} = file:list_dir(Filepath),
    case get_index_file(Files, maps:get(index_files, Options, ["index.html"])) of
        {ok, IndexFile} ->
            IndexPath = filename:join(Filepath, IndexFile),
            case inside_root(static_root(ExtraState, Filepath, Options), [IndexFile], Options) of
                {ok, _Resolved} ->
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

%% The router resolves dot segments before a request gets here. This holds
%% on its own, so the files served do not depend on what is in front of it.
path_inside_root(Resolved, Root, Pathinfo, Options) ->
    case lists:all(fun safe_segment/1, Pathinfo) of
        true ->
            Segments = [binary_to_list(Segment) || Segment <- Pathinfo],
            case inside_root(Resolved, Segments, Options) of
                {ok, FileResolved} -> {ok, filename:join([Root | Segments]), FileResolved};
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

%% Symbolic links are resolved on both sides, so a link below the root is
%% served only if it lands inside the root, unless the route opts out with
%% follow_external_symlinks. The directory the segments are in is already
%% resolved, so only the segments are looked at here.
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

%% Where a directory resolved to, and the root it is served from. The
%% router resolves the root of a static route when it compiles the route.
%% A directory is resolved here when it is not the one that was compiled,
%% which a priv directory is after a release upgrade, and when the request
%% did not come through the router.
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

%% RFC 9110 section 14.2: a range unit other than bytes must be ignored, and
%% a set of several ranges may be. Either way the whole file is sent.
ignored_range(<<"bytes=", RangeSpec/binary>>) ->
    binary:match(RangeSpec, <<",">>) =/= nomatch;
ignored_range(_OtherUnit) ->
    true.

%% RFC 9110 section 14.1.1: a range that is not written as one, or whose
%% last position is before its first, is invalid and gives error, which
%% sends the whole file. A valid range that no part of the file answers,
%% a suffix of no bytes included, is unsatisfiable.
parse_range(RangeSpec, FileSize) ->
    case binary:split(RangeSpec, <<"-">>, [global]) of
        [<<>>, SuffixLength] ->
            case digits(SuffixLength) of
                {ok, 0} ->
                    unsatisfiable;
                {ok, Length} ->
                    {ok, {max(FileSize - Length, 0), FileSize - 1}};
                error ->
                    error
            end;
        [First, <<>>] ->
            case digits(First) of
                {ok, Start} ->
                    {ok, {Start, FileSize - 1}};
                error ->
                    error
            end;
        [First, Last] ->
            case {digits(First), digits(Last)} of
                {{ok, Start}, {ok, End}} when Start =< End ->
                    {ok, {Start, min(End, FileSize - 1)}};
                _ ->
                    error
            end;
        _ ->
            error
    end.

digits(<<>>) ->
    error;
digits(Binary) ->
    digits(Binary, 0).

digits(<<Digit, Rest/binary>>, Number) when Digit >= $0, Digit =< $9 ->
    digits(Rest, Number * 10 + Digit - $0);
digits(<<>>, Number) ->
    {ok, Number};
digits(_NotADigit, _Number) ->
    error.

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
