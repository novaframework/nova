%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Nova compiler handles the initial compilation of controllers and
%%% views.
%%% @end
-module(nova_compiler).

%% API
-export([
         compile_all/0
        ]).

-include_lib("nova/include/nova.hrl").

%%%===================================================================
%%% API
%%%===================================================================
compile_all() ->
    Apps =
        case application:get_env(nova_applications) of
            undefined -> [];
            {ok, AppList} -> AppList
        end,
    lists:foreach(fun compile_files/1, Apps).


%%%===================================================================
%%% Internal functions
%%%===================================================================
compile_files(#{name := App}) ->
    case code:lib_dir(App, src) of
        {error, _} ->
            ?WARNING("Could not find the src directory of app ~p", [App]),
            error;
        Filepath ->
            {ok, FilesAndDirs} = file:list_dir(Filepath),
            Result = [ do_compile(filename:join([Filepath, FileOrDir])) || FileOrDir <- FilesAndDirs ],
            {ok, maps:from_list(lists:flatten(Result))}
    end.

do_compile(File) ->
    BeamDir =
        case application:get_application() of
            {ok, App} ->
                EbinDir = code:lib_dir(App, ebin),
                filelib:ensure_dir(EbinDir),
                EbinDir;
            _ ->
                %% We're running inside an unknown application - just put it in somewhere (This is ugly)
                [Dir|_] = code:get_path(),
                Dir
        end,

    case filelib:is_dir(File) of
        true ->
            {ok, SubFiles} = file:list_dir(File),
            [ do_compile(filename:join(File, SubFile)) || SubFile <- SubFiles ];
        _ ->
            case filename:extension(File) of
                ".dtl" ->
                    ModName = list_to_atom(filename:basename(File, ".dtl") ++ "_dtl"),
                    {module, _} = code:ensure_loaded(erlydtl),
                    try erlydtl:compile_file(File, ModName, [{out_dir, BeamDir}, return]) of
                        {ok, Module} ->
                            ?INFO("Compiled dtl view: ~p", [Module]);
                        {ok, Module, Binary} when is_binary(Binary) ->
                            ?INFO("Compiled dtl view: ~p", [Module]);
                        {ok, Module, Warnings} ->
                            ?WARNING("Compiled dtl view: ~p with warnings: ~p", [Module, Warnings]);
                        {ok, Module, _Binary, Warnings} ->
                            ?WARNING("Compiled dtl view: ~p with warnings: ~p", [Module, Warnings]);
                        {error, Error, Warnings} ->
                            format_msg(warning, Warnings),
                            format_msg(error, Error)
                    catch
                        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                        ?ERROR("Could not compile file ~s - ~p:~p~nStacktrace:~n~p", [File, Type, Reason, Stacktrace])
                    end,
                    {ModName, File};
                ".erl" ->
                    try compile:file(File, [{out_dir, BeamDir}]) of
                        {error, Errors, Warnings} ->
                            ?WARNING("Got error when compiling ~p. Errors: ~p, Warnings: ~p", [File, Errors, Warnings]),
                            [];
                        error ->
                            ?WARNING("Could not compile file ~p", [File]),
                            [];
                        {ok, ModuleName, Warnings} ->
                            ?WARNING("Compiled erlang module from file: ~p with warnings: ~p", [File, Warnings]),
                            {ModuleName, File};
                        {ok, ModuleName} ->
                            ?INFO("Compiled erlang module from file: ~p to dir: ~p", [File, BeamDir]),
                            {ModuleName, File}
                    catch
                        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
                          ?ERROR("Could not compile file ~s - ~p:~p~nStacktrace: ~p", [File, Type, Reason, Stacktrace])
                    end;
                Unsupported ->
                    %% Not supported file
                    ?WARNING("Nova does not support files ending with ~s. If this was intentionally just disregard this warning.", [Unsupported]),
                    []
            end
    end.



format_msg(Level, [{File, [{Row, Module, [Msg, Param]}]}]) ->
    case Level of
        error ->
            ?ERROR("In file \"~s\", ~p:~p \"~s\", ~p", [File, Module, Row, Msg, Param]);
        _ ->
            ?WARNING("In file \"~s\", ~p:~p \"~s\", ~p", [File, Module, Row, Msg, Param])
    end;
format_msg(_, _) ->
    ok.
