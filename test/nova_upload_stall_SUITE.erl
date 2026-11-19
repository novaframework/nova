%%% What a client that stops sending in the middle of a multipart upload
%%% holds on to, and for how long.
-module(nova_upload_stall_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([stalled_upload_is_answered_at_the_chunk_timeout/1,
         trickled_upload_is_answered_at_the_read_timeout/1,
         timed_out_upload_closes_the_connection/1,
         stalled_upload_over_http2_is_answered_and_the_connection_goes_on/1,
         stalled_uploads_leave_room_for_others/1,
         refused_upload_holds_nothing_when_the_plugin_runs_after_security/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(BOUNDARY, "nova-upload-boundary").

all() ->
    [stalled_upload_is_answered_at_the_chunk_timeout,
     trickled_upload_is_answered_at_the_read_timeout,
     timed_out_upload_closes_the_connection,
     stalled_upload_over_http2_is_answered_and_the_connection_goes_on,
     stalled_uploads_leave_room_for_others,
     refused_upload_holds_nothing_when_the_plugin_runs_after_security].

init_per_suite(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "stalled"),
    ok = filelib:ensure_path(Dir),
    Handler = {nova_multipart_file_handler, #{dir => Dir}},
    Upload = [{"/upload", fun nova_upload_cleanup_pages:leave/1, #{methods => [post]}}],
    Routes = [#{prefix => "/open",
                plugins => [{pre_request, nova_multipart_plugin, #{handler => Handler}}],
                routes => Upload},
              #{prefix => "/quick",
                plugins => [{pre_request, nova_multipart_plugin, #{handler => Handler, chunk_timeout => 1000}}],
                routes => Upload},
              #{prefix => "/short",
                plugins => [{pre_request, nova_multipart_plugin, #{handler => Handler, read_timeout => 600}}],
                routes => Upload},
              #{prefix => "/guarded",
                security => fun nova_upload_cleanup_pages:refuse/1,
                plugins => [{pre_request, nova_multipart_plugin, #{handler => Handler}}],
                routes => Upload}],
    [{port, nova_test_listener:start(#{host => Routes})}, {spool, Dir} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

stalled_upload_is_answered_at_the_chunk_timeout(Config) ->
    Socket = stalled(Config, "/quick/upload", 1, 70000),
    ?assertMatch(#{files := 1}, tracked(1)),
    ?assertEqual(~"HTTP/1.1 408", status_line(Socket)),
    ok = gen_tcp:close(Socket),
    ?assertEqual(#{files => 0, bytes => 0}, settled()),
    ?assertEqual({ok, []}, file:list_dir(?config(spool, Config))).

trickled_upload_is_answered_at_the_read_timeout(Config) ->
    Socket = stalled(Config, "/short/upload", 1, 70000),
    Trickle = spawn_link(fun() -> trickle(Socket) end),
    Status = status_line(Socket),
    unlink(Trickle),
    exit(Trickle, kill),
    ok = gen_tcp:close(Socket),
    ?assertEqual(~"HTTP/1.1 408", Status),
    ?assertEqual(#{files => 0, bytes => 0}, settled()).

timed_out_upload_closes_the_connection(Config) ->
    Socket = stalled(Config, "/quick/upload", 1, 70000),
    Answer = answer(Socket),
    ?assertEqual(~"HTTP/1.1 408", binary:part(Answer, 0, 12)),
    ?assertMatch({_, _}, binary:match(Answer, ~"connection: close")),
    ?assertEqual({error, closed}, gen_tcp:recv(Socket, 0, 3000)),
    ok = gen_tcp:close(Socket).

%% Stream 1 stops inside a file. Its answer is a 408 for that stream only:
%% stream 3, opened after it, is served on the same connection, and the file
%% of stream 1 is gone.
stalled_upload_over_http2_is_answered_and_the_connection_goes_on(Config) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config), [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, [~"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
        {Stalled, Encoder} = cow_hpack:encode(h2_post(~"/quick/upload")),
        Sent = iolist_to_binary([part("done.bin", binary:copy(~"x", 64600)), part_head("stalled.bin"), "abc"]),
        ok = gen_tcp:send(Socket, [cow_http2:settings_ack(),
                                   cow_http2:headers(1, nofin, Stalled),
                                   [cow_http2:data(1, nofin, Frame) || Frame <- h2_frames(Sent)]]),
        ?assertMatch(#{files := 2, bytes := 64600}, tracked(2)),
        {First, Buffer, Decoder} = h2_status(Socket, <<>>, cow_hpack:init(), 1),
        ?assertEqual(408, First),
        ?assertEqual(#{files => 0, bytes => 0}, settled()),
        {_Headers, Body} = multipart([{"a.bin", ~"first"}]),
        {Whole, _Encoder} = cow_hpack:encode(h2_post(~"/open/upload"), Encoder),
        ok = gen_tcp:send(Socket, [cow_http2:headers(3, nofin, Whole), cow_http2:data(3, fin, Body)]),
        ?assertMatch({200, _, _}, h2_status(Socket, Buffer, Decoder, 3))
    after
        gen_tcp:close(Socket)
    end,
    ?assertEqual(#{files => 0, bytes => 0}, settled()).

h2_post(Path) ->
    [{~":method", ~"POST"}, {~":scheme", ~"http"}, {~":path", Path}, {~":authority", ~"localhost"},
     {~"content-type", ~"multipart/form-data; boundary=nova-upload-boundary"}].

%% As much as fits the window a stream starts with, in frames of the
%% largest size a server takes without being asked.
h2_frames(Data) when byte_size(Data) =< 65535 ->
    [binary:part(Data, At, min(16384, byte_size(Data) - At)) || At <- lists:seq(0, byte_size(Data) - 1, 16384)].

%% The status of a stream, or how the stream or the connection ended
%% without one, with what is left to read the next answer.
h2_status(Socket, Buffer, Decoder, StreamID) ->
    case cow_http2:parse(Buffer) of
        {ok, {headers, Answered, _IsFin, head_fin, HeaderBlock}, Rest} ->
            {Headers, Decoder1} = cow_hpack:decode(HeaderBlock, Decoder),
            case Answered of
                StreamID ->
                    {~":status", <<Status/binary>>} = lists:keyfind(~":status", 1, Headers),
                    {binary_to_integer(Status), Rest, Decoder1};
                _ ->
                    h2_status(Socket, Rest, Decoder1, StreamID)
            end;
        {ok, {rst_stream, StreamID, Reason}, Rest} ->
            {{reset, Reason}, Rest, Decoder};
        {ok, {goaway, _LastStreamID, Reason, _Debug}, Rest} ->
            {{goaway, Reason}, Rest, Decoder};
        {ok, _OtherFrame, Rest} ->
            h2_status(Socket, Rest, Decoder, StreamID);
        more ->
            case gen_tcp:recv(Socket, 0, 5000) of
                {ok, <<Data/binary>>} -> h2_status(Socket, <<Buffer/binary, Data/binary>>, Decoder, StreamID);
                {error, Reason} -> {{closed, Reason}, Buffer, Decoder}
            end
    end.

%% Forty clients send thirty-one files each and stop inside the next one,
%% which is more files than the spool once took from all requests together.
stalled_uploads_leave_room_for_others(Config) ->
    Sockets = [stalled(Config, "/open/upload", 31, 2200) || _ <- lists:seq(1, 40)],
    try
        #{files := Held} = tracked(1025),
        ?assert(Held > 1024),
        {Headers, Body} = multipart([{"a.bin", ~"first"}]),
        ?assertMatch({200, _, _},
                     nova_test_listener:response(?config(port, Config), "POST", "/open/upload", Headers, Body))
    after
        [gen_tcp:close(Socket) || Socket <- Sockets]
    end,
    ?assertEqual(#{files => 0, bytes => 0}, settled()).

refused_upload_holds_nothing_when_the_plugin_runs_after_security(Config) ->
    Socket = stalled(Config, "/guarded/upload", 1, 70000),
    ?assertEqual(~"HTTP/1.1 401", status_line(Socket)),
    ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage()),
    ?assertEqual({ok, []}, file:list_dir(?config(spool, Config))),
    ok = gen_tcp:close(Socket).

%% Sends Complete whole files of Size bytes and the beginning of one more,
%% and says that a megabyte is on its way.
stalled(Config, Path, Complete, Size) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config), [binary, {active, false}, {packet, raw}]),
    ok = gen_tcp:send(Socket, ["POST ", Path, " HTTP/1.1\r\n",
                               "host: localhost\r\n",
                               "content-type: multipart/form-data; boundary=", ?BOUNDARY, "\r\n",
                               "content-length: 1000000\r\n\r\n",
                               [part("done.bin", binary:copy(~"x", Size)) || _ <- lists:seq(1, Complete)],
                               part_head("stalled.bin"), "abc"]),
    Socket.

trickle(Socket) ->
    timer:sleep(50),
    _ = gen_tcp:send(Socket, "x"),
    trickle(Socket).

status_line(Socket) ->
    binary:part(answer(Socket), 0, 12).

answer(Socket) ->
    case gen_tcp:recv(Socket, 0, 3000) of
        {ok, Data} -> Data;
        {error, Reason} -> atom_to_binary(Reason)
    end.

multipart(Files) ->
    {[{"content-type", ["multipart/form-data; boundary=", ?BOUNDARY]}],
     iolist_to_binary([[part(Filename, Data) || {Filename, Data} <- Files], "--", ?BOUNDARY, "--\r\n"])}.

part(Filename, Data) ->
    [part_head(Filename), Data, "\r\n"].

part_head(Filename) ->
    ["--", ?BOUNDARY, "\r\n",
     "content-disposition: form-data; name=\"upload\"; filename=\"", Filename, "\"\r\n",
     "content-type: application/octet-stream\r\n\r\n"].

%% The files go when the process that served the request is gone, which is
%% shortly after the answer.
settled() ->
    settled(40).

settled(Attempts) ->
    case nova_multipart_spool:usage() of
        #{files := Files} = Usage when Files =:= 0; Attempts =:= 0 ->
            Usage;
        _ ->
            timer:sleep(50),
            settled(Attempts - 1)
    end.

%% What the spool tracks once it tracks at least Files files.
tracked(Files) ->
    tracked(Files, 160).

tracked(Files, Attempts) ->
    case nova_multipart_spool:usage() of
        #{files := Tracked} = Usage when Tracked >= Files; Attempts =:= 0 ->
            Usage;
        _ ->
            timer:sleep(50),
            tracked(Files, Attempts - 1)
    end.
