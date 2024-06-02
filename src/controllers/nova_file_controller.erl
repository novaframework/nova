-module(nova_file_controller).
-export([
         get_file/1,
         get_dir/1
        ]).


-include_lib("kernel/include/file.hrl").

get_file(#{extra_state := #{static := File, options := Options}}) ->
    Filepath = get_filepath(File),
    MimeType =
        case maps:get(mimetype, Options, undefined) of
            undefined ->
                {T, V, _} = cow_mimetypes:web(erlang:list_to_binary(Filepath)),
                <<T/binary, "/", V/binary>>;
            MType ->
                MType
        end,
    %% We can just build the full path
    #{size := Size} = file_info("", Filepath),
    {sendfile, 200, #{}, {0, Size, Filepath}, MimeType};
get_file(_Req) ->
    {status, 404}.

get_dir(#{extra_state := #{pathinfo := Pathinfo, static := Dir, options := Options}} = Req) ->
    %% This case will be invoked if a directory was set with wildcard - pathinfo will then
    %% contain the segments of the wildcard value
    Filepath = get_filepath(Dir),
    Filepath0 = lists:foldl(fun(F, Acc) -> filename:join(Acc, binary_to_list(F)) end, Filepath, Pathinfo),
    case filelib:is_dir(Filepath0) of
        false ->
            %% Check if it's a directory
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
