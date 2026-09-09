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
