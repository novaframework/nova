-module(nova_multipart_spool_tests).
-include_lib("eunit/include/eunit.hrl").

spool_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun file_is_deleted_when_its_owner_ends/1,
      fun kept_file_outlives_its_owner/1,
      fun renamed_file_is_deleted_at_its_new_path/1,
      fun discarded_file_is_deleted_at_once/1,
      fun owner_crash_deletes_its_files_only/1,
      fun kept_file_no_longer_counts/1,
      fun unknown_path_is_not_tracked/1,
      fun string_and_binary_path_name_one_file/1,
      fun unknown_request_is_refused/1,
      fun stopping_the_spool_deletes_what_it_tracks/1]}.

setup() ->
    Dir = filename:join("/tmp", "nova_spool_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    {ok, Spool} = nova_multipart_spool:start_link(),
    unlink(Spool),
    Dir.

teardown(Dir) ->
    case whereis(nova_multipart_spool) of
        undefined -> ok;
        _Pid -> gen_server:stop(nova_multipart_spool)
    end,
    ok = file:del_dir_r(Dir).

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
        ok = nova_multipart_spool:keep(unicode:characters_to_binary(Path)),
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
