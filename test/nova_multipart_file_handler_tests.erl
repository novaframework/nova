-module(nova_multipart_file_handler_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Dir = tmp_dir(),
    ok = file:make_dir(Dir),
    Dir.

teardown(Dir) ->
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

store(Dir, Filename, Args) ->
    PartInfo = #{name => <<"upload">>, filename => Filename, content_type => <<"application/octet-stream">>},
    {ok, State} = nova_multipart_file_handler:init(PartInfo, Args#{dir => Dir}),
    {ok, State1} = nova_multipart_file_handler:handle_data(<<"data">>, State),
    nova_multipart_file_handler:handle_end(State1).
