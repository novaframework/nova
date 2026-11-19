-module(nova_watcher_tests).

-include_lib("eunit/include/eunit.hrl").

watcher_test_() ->
    {foreach,
     fun() ->
             {ok, Cwd} = file:get_cwd(),
             Dir = fresh_dir(Cwd),
             application:load(nova),
             {ok, _Pid} = nova_watcher:start_link(),
             {Cwd, Dir}
     end,
     fun({_Cwd, Dir}) ->
             ok = nova_watcher:stop(),
             application:unset_env(nova, bootstrap_application),
             ok = file:del_dir_r(Dir)
     end,
     [fun runs_in_the_workdir_and_leaves_the_node_where_it_was/1,
      fun workdir_is_relative_to_the_application/1,
      fun command_alone_runs_without_a_bootstrap_application/1,
      fun command_alone_runs_in_the_bootstrap_application/1,
      fun command_with_arguments/1,
      fun command_with_options/1,
      fun unknown_application_does_not_stop_the_watcher/1,
      fun missing_workdir_does_not_stop_the_watcher/1,
      fun malformed_watcher_does_not_stop_the_watcher/1,
      fun command_of_nested_characters_runs/1,
      fun command_holding_more_than_characters_does_not_stop_the_watcher/1,
      fun arguments_that_are_not_strings_do_not_stop_the_watcher/1]}.

configured_watchers_start_with_the_watcher_test() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = fresh_dir(Cwd),
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, watchers, [{"touch", [filename:join(Dir, "ran")]}]),
    {ok, _Pid} = nova_watcher:start_link(),
    try
        ?assert(appears(filename:join(Dir, "ran")))
    after
        ok = nova_watcher:stop(),
        application:unset_env(nova, watchers),
        application:unset_env(nova, bootstrap_application),
        ok = file:del_dir_r(Dir)
    end.

%% The number is unique in this node only, so a directory a run that was
%% stopped half way left behind can have the same name.
fresh_dir(Cwd) ->
    Dir = filename:join(Cwd, "nova_watcher_tests_" ++ integer_to_list(erlang:unique_integer([positive]))),
    _ = file:del_dir_r(Dir),
    ok = file:make_dir(Dir),
    Dir.

runs_in_the_workdir_and_leaves_the_node_where_it_was({Cwd, Dir}) ->
    ?_test(begin
               ok = nova_watcher:async_cast(nova, "touch", ["ran"], #{workdir => Dir}),
               ?assert(appears(filename:join(Dir, "ran"))),
               ?assertEqual({ok, Cwd}, file:get_cwd())
           end).

workdir_is_relative_to_the_application({_Cwd, Dir}) ->
    ?_test(begin
               Marker = filename:join(Dir, "listing"),
               ok = nova_watcher:async_cast(nova, "ls > " ++ Marker, #{workdir => "ebin"}),
               ?assert(appears(Marker)),
               ?assert(contains(Marker, ~"nova.app"))
           end).

command_alone_runs_without_a_bootstrap_application({Cwd, Dir}) ->
    ?_test(begin
               Marker = filename:join(Dir, "cwd"),
               ok = nova_watcher:async_cast("pwd > " ++ Marker),
               ?assert(appears(Marker)),
               ?assert(contains(Marker, unicode:characters_to_binary(Cwd)))
           end).

command_alone_runs_in_the_bootstrap_application({_Cwd, Dir}) ->
    ?_test(begin
               application:set_env(nova, bootstrap_application, nova),
               Marker = filename:join(Dir, "cwd"),
               ok = nova_watcher:async_cast("pwd > " ++ Marker),
               ?assert(appears(Marker)),
               ?assert(contains(Marker, unicode:characters_to_binary(code:lib_dir(nova))))
           end).

command_with_arguments({_Cwd, Dir}) ->
    ?_test(begin
               ok = nova_watcher:async_cast("touch", [filename:join(Dir, "one"), filename:join(Dir, "two")]),
               ?assert(appears(filename:join(Dir, "one"))),
               ?assert(appears(filename:join(Dir, "two")))
           end).

command_with_options({_Cwd, Dir}) ->
    ?_test(begin
               application:set_env(nova, bootstrap_application, nova),
               ok = nova_watcher:async_cast("touch ran", #{workdir => Dir}),
               ?assert(appears(filename:join(Dir, "ran")))
           end).

unknown_application_does_not_stop_the_watcher({_Cwd, Dir}) ->
    ?_test(begin
               Watcher = whereis(nova_watcher),
               ok = nova_watcher:async_cast(no_such_application, "touch", ["ran"], #{workdir => Dir}),
               _ = sys:get_state(nova_watcher),
               ?assertEqual(Watcher, whereis(nova_watcher)),
               ?assertNot(filelib:is_file(filename:join(Dir, "ran")))
           end).

missing_workdir_does_not_stop_the_watcher({_Cwd, Dir}) ->
    ?_test(begin
               Watcher = whereis(nova_watcher),
               ok = nova_watcher:async_cast(nova, "touch", [filename:join(Dir, "ran")],
                                            #{workdir => filename:join(Dir, "missing")}),
               _ = sys:get_state(nova_watcher),
               ?assertEqual(Watcher, whereis(nova_watcher)),
               ?assertNot(filelib:is_file(filename:join(Dir, "ran")))
           end).

malformed_watcher_does_not_stop_the_watcher({_Cwd, _Dir}) ->
    ?_test(begin
               Watcher = whereis(nova_watcher),
               ok = nova_watcher:async_cast(nova, ~"touch", ["ran"], #{}),
               ok = nova_watcher:async_cast(nova, "touch", ["ran"], [not_a_map]),
               _ = sys:get_state(nova_watcher),
               ?assertEqual(Watcher, whereis(nova_watcher))
           end).

command_of_nested_characters_runs({_Cwd, Dir}) ->
    ?_test(begin
               ok = nova_watcher:async_cast(nova, [["touch", [$\s]], [filename:join(Dir, "ran")]], [], #{}),
               ?assert(appears(filename:join(Dir, "ran")))
           end).

command_holding_more_than_characters_does_not_stop_the_watcher({_Cwd, Dir}) ->
    ?_test(begin
               Watcher = whereis(nova_watcher),
               ok = nova_watcher:async_cast(nova, [~"touch ", filename:join(Dir, "binary")], [], #{}),
               ok = nova_watcher:async_cast(nova, [touch, $\s | filename:join(Dir, "atom")], [], #{}),
               ok = nova_watcher:async_cast(nova, "touch " ++ filename:join(Dir, "ran"), [], #{}),
               ?assert(appears(filename:join(Dir, "ran"))),
               ?assertEqual(Watcher, whereis(nova_watcher)),
               ?assertEqual({ok, ["ran"]}, file:list_dir(Dir))
           end).

arguments_that_are_not_strings_do_not_stop_the_watcher({_Cwd, Dir}) ->
    ?_test(begin
               Watcher = whereis(nova_watcher),
               ok = nova_test_log:start(),
               ok = nova_watcher:async_cast(nova, "touch", [ran, twice], #{workdir => Dir}),
               ok = nova_watcher:async_cast(nova, "touch", [~"ran", ~"twice"], #{workdir => Dir}),
               ok = nova_watcher:async_cast(nova, "touch", ["ran", 2], #{workdir => Dir}),
               _ = sys:get_state(nova_watcher),
               Logged = [Report || #{level := error, msg := {report, Report}} <- nova_test_log:stop()],
               ?assertEqual(Watcher, whereis(nova_watcher)),
               ?assertMatch([#{msg := ~"Could not start watcher", arguments := [ran, twice]},
                             #{msg := ~"Could not start watcher", arguments := [~"ran", ~"twice"]},
                             #{msg := ~"Could not start watcher", arguments := ["ran", 2]}],
                            Logged),
               ?assertEqual({ok, []}, file:list_dir(Dir))
           end).

appears(File) ->
    appears(File, 50).

appears(_File, 0) ->
    false;
appears(File, Attempts) ->
    case filelib:is_file(File) of
        true ->
            true;
        false ->
            timer:sleep(100),
            appears(File, Attempts - 1)
    end.

contains(File, Expected) ->
    contains(File, Expected, 50).

contains(_File, _Expected, 0) ->
    false;
contains(File, Expected, Attempts) ->
    {ok, Content} = file:read_file(File),
    case binary:match(Content, Expected) of
        nomatch ->
            timer:sleep(100),
            contains(File, Expected, Attempts - 1);
        _Found ->
            true
    end.
