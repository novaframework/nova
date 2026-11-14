-module(nova_file_controller).
-moduledoc false.
-export([
         get_file/1,
         get_dir/1
        ]).

-include_lib("kernel/include/file.hrl").

-define(MAX_SYMLINKS, 40).

get_file(#{extra_state := #{static := File, options := Options}, headers := Headers}) ->
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
    #{size := Size} = file_info("", Filepath),

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
    end;

get_file(_Req) ->
    {status, 404}.

get_dir(#{extra_state := #{pathinfo := Pathinfo, static := Dir, options := Options}} = Req) ->
    %% This case will be invoked if a directory was set with wildcard - pathinfo will then
    %% contain the segments of the wildcard value
    Root = get_filepath(Dir),
    case path_inside_root(Root, Pathinfo, Options) of
        {ok, Filepath0} ->
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
                    get_dir(Req#{extra_state => #{static => {dir, Filepath0}, static_root => Root,
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
            case inside_root(maps:get(static_root, ExtraState, Filepath), IndexPath, Options) of
                true ->
                    get_file(Req#{extra_state => #{static => {file, IndexPath}, options => Options}});
                false ->
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
path_inside_root(Root, Pathinfo, Options) ->
    case lists:all(fun safe_segment/1, Pathinfo) of
        true ->
            Filepath = filename:join([Root | [binary_to_list(Segment) || Segment <- Pathinfo]]),
            case inside_root(Root, Filepath, Options) of
                true  -> {ok, Filepath};
                false -> error
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
%% follow_external_symlinks.
inside_root(Root, Filepath, Options) ->
    ResolveLinks = not maps:get(follow_external_symlinks, Options, false),
    case {canonical_path(Root, ResolveLinks), canonical_path(Filepath, ResolveLinks)} of
        {{ok, RootSegments}, {ok, FileSegments}} ->
            lists:prefix(RootSegments, FileSegments);
        _ ->
            false
    end.

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
