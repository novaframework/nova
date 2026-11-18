-module(nova_req_tests).
-include_lib("eunit/include/eunit.hrl").

-define(REQ(), nova_fake_server:req()).
-define(REQ(Fields), nova_fake_server:req(Fields)).

request_fields_test() ->
    Req = ?REQ(#{qs => ~"a=1&b", bindings => #{~"id" => ~"7"}, listener => admin,
                 headers => #{~"content-type" => ~"text/plain; charset=utf-8",
                              ~"cookie" => ~"a=1; b=2"}}),
    ?assertEqual(~"GET", nova_req:method(Req)),
    ?assertEqual(~"http", nova_req:scheme(Req)),
    ?assertEqual(~"localhost", nova_req:host(Req)),
    ?assertEqual(8080, nova_req:port(Req)),
    ?assertEqual(~"/", nova_req:path(Req)),
    ?assertEqual(admin, nova_req:listener(Req)),
    ?assertEqual([{~"a", ~"1"}, {~"b", true}], nova_req:parse_qs(Req)),
    ?assertEqual(~"7", nova_req:binding(~"id", Req)),
    ?assertEqual(none, nova_req:binding(~"other", Req, none)),
    ?assertEqual(undefined, nova_req:header(~"accept", Req)),
    ?assertEqual({~"text", ~"plain", [{~"charset", ~"utf-8"}]}, nova_req:parse_header(~"content-type", Req)),
    ?assertEqual([{~"a", ~"1"}, {~"b", ~"2"}], nova_req:parse_cookies(Req)).

http_version_test() ->
    ?assertEqual(http1, nova_req:http_version(?REQ(#{version => 'HTTP/1.0'}))),
    ?assertEqual(http1, nova_req:http_version(?REQ())),
    ?assertEqual(http2, nova_req:http_version(?REQ(#{version => 'HTTP/2'}))),
    ?assertEqual(http3, nova_req:http_version(?REQ(#{version => 'HTTP/3'}))).

malformed_request_test() ->
    ?assertExit({request_error, qs, _}, nova_req:parse_qs(?REQ(#{qs => ~"a=%zz"}))),
    ?assertExit({request_error, {header, ~"content-type"}, _},
                nova_req:parse_header(~"content-type", ?REQ(#{headers => #{~"content-type" => ~"nonsense"}}))),
    ?assertError({unsupported_header, ~"x-unknown"}, nova_req:parse_header(~"x-unknown", ?REQ())).

reply_merge_test() ->
    Req0 = nova_req:set_resp_header(~"x-a", ~"request", ?REQ()),
    Req1 = nova_req:set_resp_headers(#{~"x-b" => ~"request"}, Req0),
    Req2 = nova_req:set_resp_cookie(~"one", ~"1", Req1),
    Req3 = nova_req:set_resp_cookie(~"two", ~"2", Req2, #{http_only => true}),
    Req = nova_req:reply(200, #{~"x-b" => ~"call"}, ~"hello", Req3),
    [{reply, 200, Headers, ~"hello"}] = nova_fake_server:sent(),
    ?assertEqual(~"request", proplists:get_value(~"x-a", Headers)),
    ?assertEqual(~"call", proplists:get_value(~"x-b", Headers)),
    ?assertEqual(~"5", proplists:get_value(~"content-length", Headers)),
    ?assertEqual([~"one=1", ~"two=2; HttpOnly"],
                 lists:sort([iolist_to_binary(Cookie) || {~"set-cookie", Cookie} <- Headers])),
    ?assertEqual(true, maps:get(has_sent_resp, Req)),
    ?assertNot(is_map_key(resp_headers, Req)),
    ?assertNot(is_map_key(resp_cookies, Req)).

headers_from_a_list_test() ->
    Req = nova_req:set_resp_headers([{~"vary", ~"accept"}, {~"x-a", ~"1"}, {~"vary", ~"origin"}], ?REQ()),
    ?assertEqual(~"accept, origin", iolist_to_binary(nova_req:resp_header(~"vary", Req, <<>>))),
    ?assertEqual(~"1", nova_req:resp_header(~"x-a", Req)),
    ?assertExit({response_error, invalid_header, _}, nova_req:set_resp_headers([{~"set-cookie", ~"a=1"}], ?REQ())).

reply_resp_body_test() ->
    _ = nova_req:reply(201, nova_req:set_resp_body(~"made", ?REQ())),
    ?assertMatch([{reply, 201, _, ~"made"}], nova_fake_server:sent()).

reply_status_phrase_test() ->
    _ = nova_req:reply(~"404 Not Found", ?REQ()),
    ?assertMatch([{reply, 404, _, <<>>}], nova_fake_server:sent()).

head_reply_test() ->
    _ = nova_req:reply(200, #{}, ~"hello", ?REQ(#{method => ~"HEAD"})),
    [{reply, 200, Headers, <<>>}] = nova_fake_server:sent(),
    ?assertEqual(~"5", proplists:get_value(~"content-length", Headers)).

no_content_length_test() ->
    _ = nova_req:reply(204, ?REQ()),
    [{reply, 204, Headers, <<>>}] = nova_fake_server:sent(),
    ?assertEqual(undefined, proplists:get_value(~"content-length", Headers)).

no_content_body_test() ->
    ?assertExit({response_error, payload_too_large, _}, nova_req:reply(204, #{}, ~"body", ?REQ())),
    ?assertExit({response_error, payload_too_large, _}, nova_req:reply(304, #{}, ~"body", ?REQ())),
    ?assertEqual([], nova_fake_server:sent()).

sendfile_test() ->
    _ = nova_req:reply(200, #{}, {sendfile, 0, 12, "/tmp/file"}, ?REQ()),
    [{reply, 200, Headers, {sendfile, 0, 12, "/tmp/file"}}] = nova_fake_server:sent(),
    ?assertEqual(~"12", proplists:get_value(~"content-length", Headers)),
    _ = nova_req:reply(200, #{}, {sendfile, 0, 0, "/tmp/empty"}, ?REQ()),
    ?assertMatch([{reply, 200, _, <<>>}], nova_fake_server:sent()).

set_cookie_header_test() ->
    ?assertExit({response_error, invalid_header, _}, nova_req:set_resp_header(~"set-cookie", ~"a=1", ?REQ())),
    ?assertExit({response_error, invalid_header, _}, nova_req:set_resp_headers(#{~"set-cookie" => ~"a=1"}, ?REQ())),
    ?assertExit({response_error, invalid_header, _}, nova_req:reply(200, #{~"set-cookie" => ~"a=1"}, ?REQ())),
    ?assertExit({response_error, invalid_header, _}, nova_req:stream_reply(200, #{~"set-cookie" => ~"a=1"}, ?REQ())),
    ?assertError(_, nova_req:set_resp_cookie(~"bad name", ~"1", ?REQ())).

%% A value somebody else chose, a redirect target for instance, must not be
%% able to write a header or a response of its own.
header_injection_test() ->
    [begin
         Req = nova_req:reply(302, Headers, ?REQ()),
         ?assertEqual(true, maps:get(has_sent_resp, Req)),
         ?assertEqual([{reply, 500, [{~"content-length", ~"0"}], <<>>}], nova_fake_server:sent())
     end || Headers <- [#{~"location" => ~"/next\r\nset-cookie: a=b"},
                        #{~"location" => ~"/next\nx: y"},
                        #{~"x-a" => <<"a", 0, "b">>},
                        #{~"x-a" => [~"a", [~"\r\n"]]},
                        #{~"x a" => ~"1"},
                        #{~"x-a\r\nx-b" => ~"1"},
                        #{<<>> => ~"1"}]],
    _ = nova_req:stream_reply(200, #{~"x-a" => ~"a\r\nb"}, ?REQ()),
    ?assertEqual([{reply, 500, [{~"content-length", ~"0"}], <<>>}], nova_fake_server:sent()),
    _ = nova_req:reply(200, nova_req:set_resp_header(~"x-a", ~"a\nb", ?REQ())),
    ?assertEqual([{reply, 500, [{~"content-length", ~"0"}], <<>>}], nova_fake_server:sent()).

%% The answer to an upgrade, an informational response and the trailers
%% are checked as the headers of a response are.
upgrade_injection_test() ->
    Req = nova_req:set_resp_header(~"x-a", ~"a\r\nb", ?REQ()),
    {ok, Answered, #{}} = nova_req:upgrade_websocket(Req, #{}, nova_ws_handler, ws_state, #{}),
    ?assertEqual(true, nova_req:response_begun(Answered)),
    ?assertEqual([{reply, 500, [{~"content-length", ~"0"}], <<>>}], nova_fake_server:sent()).

upgrade_headers_test() ->
    Req = nova_req:set_resp_header(~"x-a", ~"1", ?REQ()),
    {ok, _Req, #{}} = nova_req:upgrade_websocket(Req, #{}, nova_ws_handler, ws_state, #{}),
    ?assertEqual([{upgrade_websocket, nova_ws_handler, ws_state, #{}, []}], nova_fake_server:sent()).

inform_injection_test() ->
    ok = nova_req:inform(103, #{~"link" => ~"</a>\r\nx-b: 1"}, ?REQ()),
    ?assertEqual([], nova_fake_server:sent()).

trailers_injection_test() ->
    Req = nova_req:stream_reply(200, #{}, ?REQ()),
    ok = nova_req:stream_trailers(#{~"x-end" => ~"1\nx-b: 1"}, Req),
    ?assertMatch([{stream_reply, 200, _}, {stream_body, <<>>, fin}], nova_fake_server:sent()).

second_reply_test() ->
    Req0 = ?REQ(),
    Req1 = nova_req:reply(200, Req0),
    Req2 = nova_req:reply(500, Req1),
    ?assertEqual(true, maps:get(has_sent_resp, Req2)),
    ?assertMatch([{reply, 200, _, _}], nova_fake_server:sent()).

second_reply_stale_req_test() ->
    Req0 = ?REQ(),
    _ = nova_req:reply(200, Req0),
    ?assertEqual(true, nova_req:response_begun(Req0)),
    Req = nova_req:reply(500, Req0),
    ?assertEqual(true, maps:get(has_sent_resp, Req)),
    ?assertMatch([{reply, 200, _, _}], nova_fake_server:sent()).

stream_test() ->
    Req0 = nova_req:set_resp_cookie(~"one", ~"1", ?REQ()),
    Req = nova_req:stream_reply(200, #{~"content-type" => ~"text/event-stream"}, Req0),
    ?assertEqual(headers, maps:get(has_sent_resp, Req)),
    ok = nova_req:stream_body(<<>>, nofin, Req),
    ok = nova_req:stream_body(~"data: 1\n\n", nofin, Req),
    ok = nova_req:stream_trailers(#{~"x-end" => ~"1"}, Req),
    [{stream_reply, 200, Headers}, {stream_body, ~"data: 1\n\n", nofin}, {stream_trailers, [{~"x-end", ~"1"}]}] =
        nova_fake_server:sent(),
    ?assertEqual(~"text/event-stream", proplists:get_value(~"content-type", Headers)),
    ?assertEqual(undefined, proplists:get_value(~"content-length", Headers)),
    ?assertEqual(~"one=1", iolist_to_binary(proplists:get_value(~"set-cookie", Headers))).

bodyless_stream_reply_test() ->
    Req = nova_req:stream_reply(204, ?REQ()),
    ?assertEqual(true, maps:get(has_sent_resp, Req)),
    ?assertMatch([{reply, 204, _, <<>>}], nova_fake_server:sent()).

head_stream_body_test() ->
    Req = nova_req:stream_reply(200, ?REQ(#{method => ~"HEAD"})),
    ok = nova_req:stream_body(~"data", fin, Req),
    ?assertMatch([{stream_reply, 200, _}], nova_fake_server:sent()).

inform_test() ->
    Req0 = ?REQ(),
    ok = nova_req:inform(103, #{~"link" => ~"</style.css>; rel=preload"}, Req0),
    Req = nova_req:reply(200, Req0),
    ok = nova_req:inform(103, Req),
    ?assertMatch([{inform, 103, [{~"link", _}]}, {reply, 200, _, _}], nova_fake_server:sent()).

read_body_test() ->
    Req0 = ?REQ(#{body_chunks => [~"ab", ~"cd"]}),
    {more, ~"ab", Req1} = nova_req:read_body(Req0),
    ?assertMatch({ok, ~"cd", _}, nova_req:read_body(Req1)).

read_urlencoded_body_test() ->
    ?assertMatch({ok, [{~"a", ~"1"}, {~"b", ~"x y"}], _},
                 nova_req:read_urlencoded_body(?REQ(#{body_chunks => [~"a=1&b=x+y"]}))),
    ?assertExit({request_error, urlencoded_body, _},
                nova_req:read_urlencoded_body(?REQ(#{body_chunks => [~"a=%zz"]}))),
    ?assertExit({request_error, payload_too_large, _},
                nova_req:read_urlencoded_body(?REQ(#{body_chunks => [~"a=12345", ~"6"]}), #{length => 4})),
    ?assertExit({request_error, timeout, _},
                nova_req:read_urlencoded_body(?REQ(#{body_chunks => [~"a=1", ~"2"]}))).

multipart_test() ->
    Body = <<"--xyz\r\ncontent-disposition: form-data; name=\"a\"\r\n\r\none\r\n"
             "--xyz\r\ncontent-disposition: form-data; name=\"b\"\r\n\r\ntwo\r\n--xyz--\r\n">>,
    %% Every way of cutting the body in two reads the same parts.
    [?assertEqual([{~"form-data; name=\"a\"", ~"one"}, {~"form-data; name=\"b\"", ~"two"}],
                  parts(multipart_req([binary:part(Body, 0, Cut), binary:part(Body, Cut, byte_size(Body) - Cut)])))
     || Cut <- lists:seq(1, byte_size(Body) - 1)],
    ok.

multipart_errors_test() ->
    ?assertExit({request_error, {multipart, content_type}, _},
                nova_req:read_part(?REQ(#{headers => #{~"content-type" => ~"text/plain"}}))),
    ?assertExit({request_error, {multipart, boundary}, _},
                nova_req:read_part(?REQ(#{headers => #{~"content-type" => ~"multipart/form-data"}}))),
    ?assertExit({request_error, {multipart, headers}, _}, nova_req:read_part(multipart_req([<<>>]))),
    ?assertExit({request_error, {multipart, headers}, _},
                nova_req:read_part(multipart_req([<<"--xyz\r\n", (binary:copy(~"a", 4096))/binary>>, ~"b"]))),
    ?assertExit({request_error, {multipart, headers}, _},
                nova_req:read_part(multipart_req([~"--xyz\r\nx-a: 1\r\nx-a: 2\r\n\r\nbody\r\n--xyz--\r\n"]))).

multipart_req(Chunks) ->
    ?REQ(#{headers => #{~"content-type" => ~"multipart/form-data; boundary=xyz"}, body_chunks => Chunks}).

parts(Req0) ->
    case nova_req:read_part(Req0) of
        {ok, #{~"content-disposition" := Disposition}, Req1} ->
            {Body, Req} = part_body(Req1, <<>>),
            [{Disposition, Body} | parts(Req)];
        {done, _Req} ->
            []
    end.

part_body(Req0, Acc) ->
    case nova_req:read_part_body(Req0) of
        {ok, Data, Req} -> {<<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> part_body(Req, <<Acc/binary, Data/binary>>)
    end.
