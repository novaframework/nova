-module(nova_multipart_spool_tests).
-include_lib("eunit/include/eunit.hrl").

spool_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun file_is_deleted_when_its_owner_ends/1,
      fun kept_file_outlives_its_owner/1,
      fun renamed_file_is_deleted_at_its_new_path/1,
      fun file_renamed_by_the_spool_is_deleted_at_its_new_path/1,
      fun discarded_file_is_deleted_at_once/1,
      fun owner_crash_deletes_its_files_only/1,
      fun owner_with_a_kept_file_loses_the_others/1,
      fun owner_without_files_is_no_longer_watched/1,
      fun path_tracked_again_starts_over/1,
      fun kept_file_no_longer_counts/1,
      fun unknown_path_is_not_tracked/1,
      fun string_and_binary_path_name_one_file/1,
      fun unknown_request_is_refused/1,
      fun stopping_the_spool_deletes_what_it_tracks/1]}.

setup() ->
    Dir = nova_test_helper:make_tmp_dir("nova_spool_test"),
    try
        {ok, Spool} = nova_multipart_spool:start_link(),
        unlink(Spool),
        Dir
    catch
        Class:Reason:Stacktrace ->
            ok = file:del_dir_r(Dir),
            erlang:raise(Class, Reason, Stacktrace)
    end.

teardown(Dir) ->
    try
        case whereis(nova_multipart_spool) of
            undefined -> ok;
            _Pid -> gen_server:stop(nova_multipart_spool)
        end
    after
        ok = file:del_dir_r(Dir)
    end.

file_is_deleted_when_its_owner_ends(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        owner(fun() -> spool(Path, ~"data") end),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage()),
        ?assertEqual({error, enoent}, file:read_file(Path))
    end.

kept_file_outlives_its_owner(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        owner(fun() ->
                      spool(Path, ~"data"),
                      ok = nova_multipart_spool:keep(Path)
              end),
        ?assertEqual({ok, ~"data"}, file:read_file(Path))
    end.

renamed_file_is_deleted_at_its_new_path(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload.part"),
        NewPath = filename:join(Dir, "upload"),
        owner(fun() ->
                      spool(Path, ~"data"),
                      ok = file:rename(Path, NewPath),
                      ok = nova_multipart_spool:renamed(Path, NewPath)
              end),
        ?assertEqual({ok, []}, file:list_dir(Dir))
    end.

file_renamed_by_the_spool_is_deleted_at_its_new_path(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload.part"),
        NewPath = filename:join(Dir, "upload"),
        spool(Path, ~"data"),
        ok = nova_multipart_spool:rename(Path, NewPath),
        ?assertEqual({ok, ["upload"]}, file:list_dir(Dir)),
        ?assertEqual(#{files => 1, bytes => 4}, nova_multipart_spool:usage()),
        ?assertEqual({error, not_tracked}, nova_multipart_spool:rename(Path, NewPath)),
        ?assertEqual({error, enoent}, nova_multipart_spool:rename(NewPath, filename:join([Dir, "missing", "upload"]))),
        ok = nova_multipart_spool:discard(NewPath),
        ?assertEqual({ok, []}, file:list_dir(Dir))
    end.

discarded_file_is_deleted_at_once(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        spool(Path, ~"data"),
        ok = nova_multipart_spool:discard(Path),
        ?assertEqual({error, enoent}, file:read_file(Path)),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage())
    end.

owner_crash_deletes_its_files_only(Dir) ->
    fun() ->
        Mine = filename:join(Dir, "mine"),
        Theirs = filename:join(Dir, "theirs"),
        spool(Mine, ~"mine"),
        owner(fun() ->
                      spool(Theirs, ~"theirs"),
                      exit(crashed)
              end),
        ?assertEqual({ok, ["mine"]}, file:list_dir(Dir)),
        ?assertEqual(#{files => 1, bytes => 4}, nova_multipart_spool:usage())
    end.

owner_with_a_kept_file_loses_the_others(Dir) ->
    fun() ->
        Kept = filename:join(Dir, "kept"),
        Moved = filename:join(Dir, "moved"),
        owner(fun() ->
                      spool(Kept, ~"kept"),
                      spool(filename:join(Dir, "left"), ~"left"),
                      spool(filename:join(Dir, "moved.part"), ~"moved"),
                      ok = file:rename(filename:join(Dir, "moved.part"), Moved),
                      ok = nova_multipart_spool:renamed(filename:join(Dir, "moved.part"), Moved),
                      ok = nova_multipart_spool:keep(Kept)
              end),
        ?assertEqual({ok, ["kept"]}, file:list_dir(Dir)),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage())
    end.

owner_without_files_is_no_longer_watched(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        spool(Path, ~"data"),
        ?assertEqual([{process, self()}], watched()),
        ok = nova_multipart_spool:discard(Path),
        ?assertEqual([], watched())
    end.

path_tracked_again_starts_over(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        spool(Path, ~"first"),
        spool(Path, ~"data"),
        ?assertEqual(#{files => 1, bytes => 4}, nova_multipart_spool:usage()),
        ok = nova_multipart_spool:discard(Path),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage())
    end.

kept_file_no_longer_counts(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        spool(Path, ~"data"),
        ?assertEqual(#{files => 1, bytes => 4}, nova_multipart_spool:usage()),
        ok = nova_multipart_spool:keep(Path),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage())
    end.

unknown_path_is_not_tracked(Dir) ->
    fun() ->
        Path = filename:join(Dir, "never-tracked"),
        ?assertEqual({error, not_tracked}, nova_multipart_spool:keep(Path)),
        ?assertEqual({error, not_tracked}, nova_multipart_spool:discard(Path)),
        ?assertEqual({error, not_tracked}, nova_multipart_spool:grow(Path, 1)),
        ?assertEqual({error, not_tracked}, nova_multipart_spool:renamed(Path, Path))
    end.

string_and_binary_path_name_one_file(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upl\x{f6}ad"),
        spool(Path, ~"data"),
        ok = nova_multipart_spool:keep(iolist_to_binary([Dir, <<"/upl\x{f6}ad"/utf8>>])),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage()),
        ?assertEqual({ok, ~"data"}, file:read_file(Path))
    end.

unknown_request_is_refused(_Dir) ->
    fun() ->
        ?assertEqual({error, badarg}, gen_server:call(nova_multipart_spool, {track, not_a_path, self()})),
        ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage())
    end.

stopping_the_spool_deletes_what_it_tracks(Dir) ->
    fun() ->
        Path = filename:join(Dir, "upload"),
        spool(Path, ~"data"),
        ok = gen_server:stop(nova_multipart_spool),
        ?assertEqual({error, enoent}, file:read_file(Path)),
        ?assertEqual({error, spool_not_running}, nova_multipart_spool:track(Path))
    end.

watched() ->
    case whereis(nova_multipart_spool) of
        Spool when is_pid(Spool) ->
            {monitors, Monitors} = process_info(Spool, monitors),
            Monitors
    end.

spool(Path, Data) ->
    ok = nova_multipart_spool:track(Path),
    ok = nova_multipart_spool:grow(Path, byte_size(Data)),
    ok = file:write_file(Path, Data).

%% Runs Fun in a process of its own and returns once the spool has seen
%% that process end.
owner(Fun) ->
    {Pid, Monitor} = spawn_monitor(Fun),
    receive
        {'DOWN', Monitor, process, Pid, _Reason} -> ok
    end,
    _ = sys:get_state(nova_multipart_spool),
    ok.
