-module(nova_multipart_file_handler_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Dir = tmp_dir(),
    ok = file:make_dir(Dir),
    {ok, Spool} = nova_multipart_spool:start_link(),
    unlink(Spool),
    Dir.

teardown(Dir) ->
    ok = gen_server:stop(nova_multipart_spool),
    {ok, Files} = file:list_dir(Dir),
    [file:delete(filename:join(Dir, F)) || F <- Files],
    ok = file:del_dir(Dir).

tmp_dir() ->
    Name = io_lib:format("nova_multipart_test_~p", [erlang:unique_integer([positive])]),
    filename:join("/tmp", lists:flatten(Name)).

client_filename_never_used_in_path_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             PartInfo = #{name => <<"upload">>,
                          filename => <<"../../../../etc/passwd">>,
                          content_type => <<"text/plain">>},
             {ok, State} = nova_multipart_file_handler:init(PartInfo, #{dir => Dir}),
             {ok, State1} = nova_multipart_file_handler:handle_data(<<"hello ">>, State),
             {ok, State2} = nova_multipart_file_handler:handle_data(<<"world">>, State1),
             {ok, #{path := Path}} = nova_multipart_file_handler:handle_end(State2),
             %% the final path must land inside Dir, never honouring the
             %% client-supplied filename's path traversal
             ?assertEqual(iolist_to_binary(Dir), iolist_to_binary(filename:dirname(Path))),
             ?assertEqual({ok, <<"hello world">>}, file:read_file(Path))
         end
     end}.

abort_deletes_partial_file_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             PartInfo = #{name => <<"upload">>, filename => <<"x.bin">>, content_type => <<"application/octet-stream">>},
             {ok, State} = nova_multipart_file_handler:init(PartInfo, #{dir => Dir}),
             {ok, State1} = nova_multipart_file_handler:handle_data(<<"partial">>, State),
             ok = nova_multipart_file_handler:handle_abort(too_large, State1),
             {ok, Remaining} = file:list_dir(Dir),
             ?assertEqual([], Remaining)
         end
     end}.

concurrent_parts_never_collide_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             PartInfo = #{name => <<"upload">>, filename => <<"same-name.bin">>, content_type => <<"application/octet-stream">>},
             {ok, S1} = nova_multipart_file_handler:init(PartInfo, #{dir => Dir}),
             {ok, S2} = nova_multipart_file_handler:init(PartInfo, #{dir => Dir}),
             {ok, #{path := P1}} = nova_multipart_file_handler:handle_end(S1),
             {ok, #{path := P2}} = nova_multipart_file_handler:handle_end(S2),
             ?assertNotEqual(P1, P2)
         end
     end}.

no_extension_by_default_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             {ok, #{path := Path}} = store(Dir, <<"photo.png">>, #{dir => Dir}),
             ?assertEqual(<<>>, iolist_to_binary(filename:extension(Path)))
         end
     end}.

allowlisted_extension_kept_and_lowercased_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             Args = #{dir => Dir, extensions => [<<"png">>, <<"jpg">>]},
             {ok, #{path := Path}} = store(Dir, <<"Photo.PNG">>, Args),
             ?assertEqual(<<".png">>, iolist_to_binary(filename:extension(Path))),
             ?assertEqual({ok, <<"data">>}, file:read_file(Path)),
             {ok, Remaining} = file:list_dir(Dir),
             ?assertEqual(1, length(Remaining))
         end
     end}.

non_listed_extension_dropped_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             Args = #{dir => Dir, extensions => [<<"png">>]},
             {ok, #{path := Path}} = store(Dir, <<"page.html">>, Args),
             ?assertEqual(<<>>, iolist_to_binary(filename:extension(Path)))
         end
     end}.

traversal_filename_contributes_only_extension_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             Args = #{dir => Dir, extensions => [<<"png">>]},
             {ok, #{path := Path}} = store(Dir, <<"../../../etc/x.png">>, Args),
             ?assertEqual(iolist_to_binary(Dir), iolist_to_binary(filename:dirname(Path))),
             ?assertEqual(<<".png">>, iolist_to_binary(filename:extension(Path)))
         end
     end}.

extension_with_bad_chars_dropped_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             Args = #{dir => Dir, extensions => [<<"p g">>, <<"p/g">>, <<"png">>]},
             {ok, #{path := P1}} = store(Dir, <<"a.p g">>, Args),
             {ok, #{path := P2}} = store(Dir, <<"a.p/g">>, Args),
             {ok, #{path := P3}} = store(Dir, <<"a.">>, Args),
             {ok, #{path := P4}} = store(Dir, <<"a">>, Args),
             [?assertEqual(<<>>, iolist_to_binary(filename:extension(P))) || P <- [P1, P2, P3, P4]],
             [?assertEqual(iolist_to_binary(Dir), iolist_to_binary(filename:dirname(P))) || P <- [P1, P2, P3, P4]]
         end
     end}.

abort_deletes_partial_file_with_extension_test_() ->
    {setup, fun setup/0, fun teardown/1,
     fun(Dir) ->
         fun() ->
             Args = #{dir => Dir, extensions => [<<"png">>]},
             PartInfo = #{name => <<"upload">>, filename => <<"x.png">>, content_type => <<"image/png">>},
             {ok, State} = nova_multipart_file_handler:init(PartInfo, Args),
             {ok, [Tmp]} = file:list_dir(Dir),
             ?assertEqual(".part", filename:extension(Tmp)),
             ?assertEqual(".png", filename:extension(filename:rootname(Tmp))),
             ok = nova_multipart_file_handler:handle_abort(too_large, State),
             ?assertEqual({ok, []}, file:list_dir(Dir))
         end
     end}.

part_is_refused_without_the_spool_test() ->
    PartInfo = #{name => ~"upload", filename => ~"x.bin", content_type => ~"application/octet-stream"},
    ?assertEqual({error, spool_not_running}, nova_multipart_file_handler:init(PartInfo, #{dir => "/tmp"})).

part_past_the_file_limit_is_refused_test_() ->
    {setup, fun() -> with_limits(#{max_files => 1}) end, fun without_limits/1,
     fun(Dir) ->
         fun() ->
             PartInfo = #{name => ~"upload", filename => ~"x.bin", content_type => ~"application/octet-stream"},
             {ok, _State} = nova_multipart_file_handler:init(PartInfo, #{dir => Dir}),
             ?assertEqual({error, spool_full}, nova_multipart_file_handler:init(PartInfo, #{dir => Dir})),
             ?assertMatch({ok, [_OnlyTheFirst]}, file:list_dir(Dir))
         end
     end}.

chunk_past_the_byte_limit_is_refused_test_() ->
    {setup, fun() -> with_limits(#{max_bytes => 10}) end, fun without_limits/1,
     fun(Dir) ->
         fun() ->
             PartInfo = #{name => ~"upload", filename => ~"x.bin", content_type => ~"application/octet-stream"},
             {ok, State} = nova_multipart_file_handler:init(PartInfo, #{dir => Dir}),
             {ok, State1} = nova_multipart_file_handler:handle_data(~"12345678", State),
             ?assertEqual({error, spool_full}, nova_multipart_file_handler:handle_data(~"901", State1)),
             ok = nova_multipart_file_handler:handle_abort(spool_full, State1),
             ?assertEqual({ok, []}, file:list_dir(Dir)),
             ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage())
         end
     end}.

with_limits(Limits) ->
    application:set_env(nova, multipart_spool, Limits),
    setup().

without_limits(Dir) ->
    application:unset_env(nova, multipart_spool),
    teardown(Dir).

store(Dir, Filename, Args) ->
    PartInfo = #{name => <<"upload">>, filename => Filename, content_type => <<"application/octet-stream">>},
    {ok, State} = nova_multipart_file_handler:init(PartInfo, Args#{dir => Dir}),
    {ok, State1} = nova_multipart_file_handler:handle_data(<<"data">>, State),
    nova_multipart_file_handler:handle_end(State1).
