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
-define(MAX_RANGE_DIGITS, 19).
-define(PAST_ANY_FILE, 1 bsl 64).

%% The extra_state of a static route, for the router. The root of a
%% directory is resolved here, when the route is compiled, so a request only
%% has to resolve the segments that come after it. The path of a file is
%% made absolute, so it does not follow a change of the working directory.
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
serve_file(_Req) ->
    {status, 404}.

serve_file(Filepath, Size, MimeType, Headers) ->
    %% Check if Range header is present
    case maps:get(<<"range">>, Headers, undefined) of
        undefined ->
            %% No range header, return full file
            {sendfile, 200, #{~"x-content-type-options" => ~"nosniff"}, {0, Size, Filepath}, MimeType};

        <<"bytes=", RangeSpec/binary>> ->
            %% Handle Range Request
            case parse_range(RangeSpec, Size) of
                {ok, {Start, End}} when Start < Size, End < Size, Start =< End ->
                    Length = End - Start + 1,
                    RespHeaders = #{<<"content-range">> => <<"bytes ", (integer_to_binary(Start))/binary, "-",
                                                             (integer_to_binary(End))/binary, "/",
                                                             (integer_to_binary(Size))/binary>>,
                                    ~"content-length" => integer_to_binary(Length),
                                    ~"x-content-type-options" => ~"nosniff"},
                    {sendfile, 206, RespHeaders, {Start, Length, Filepath}, MimeType};
                error ->
                    {sendfile, 200, #{}, {0, Size, Filepath}, MimeType};
                _ ->
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
                    %% Check if it's a file
                    case filelib:is_file(Filepath0) of
                        true ->
                            %% It's a file
                            get_file(Req#{extra_state => #{static => {file, Filepath0}, options => Options}});
                        false ->
                            {status, 404}
                    end;
                true ->
                    get_dir(Req#{extra_state => #{static => {dir, Filepath0}, static_root => {Filepath0, FileResolved},
                                                  options => Options}})
            end;
        error ->
            {status, 404}
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

%% A static directory that cannot be served is the same on every request,
%% so it is logged the first time only.
unusable_root(Directory, Reason) ->
    Key = {?MODULE, unusable_root, Directory},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?LOG_ERROR(#{msg => ~"Static directory cannot be served", directory => Directory, reason => Reason})
    end.

%% As is a static file that is gone or cannot be read.
unusable_file(File, Reason) ->
    Key = {?MODULE, unusable_file, File},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?LOG_ERROR(#{msg => ~"Static file cannot be served", file => File, reason => Reason})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% The router resolves dot segments before a request gets here. This holds
%% on its own, so the files served do not depend on what is in front of it.
%% The path returned is the one that was checked, from the resolved root,
%% so a root that has come to mean another directory is not served from.
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

%% A client chooses how many digits a number has. One of more than 19
%% digits is past the end of any file, so it is never made into an integer.
digits(<<>>) ->
    error;
digits(Binary) ->
    case all_digits(Binary) of
        true -> significant_digits(without_leading_zeros(Binary));
        false -> error
    end.

all_digits(<<Digit, Rest/binary>>) when Digit >= $0, Digit =< $9 ->
    all_digits(Rest);
all_digits(<<>>) ->
    true;
all_digits(_NotADigit) ->
    false.

without_leading_zeros(<<$0, Rest/binary>>) ->
    without_leading_zeros(Rest);
without_leading_zeros(Digits) ->
    Digits.

significant_digits(<<>>) ->
    {ok, 0};
significant_digits(Digits) when byte_size(Digits) > ?MAX_RANGE_DIGITS ->
    {ok, ?PAST_ANY_FILE};
significant_digits(Digits) ->
    {ok, binary_to_integer(Digits)}.

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

%% cow_mimetypes:web/1 only knows a handful of types (no .txt, .json, .woff2
%% or .webp), so fall back to the full table before octet-stream. Types a
%% browser renders as a document and runs scripts in (XML, XHTML, compressed
%% SVG and the other +xml types) stay octet-stream unless the route sets a
%% mimetype.
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
