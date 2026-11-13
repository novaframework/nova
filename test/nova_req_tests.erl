-module(nova_req_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% nova_req on a request served by nova_fake_server: the functions that
%% touch the connection reach the request's server, and the ones that read
%% or set the map work as cowboy_req's do.
%%====================================================================

req() ->
    req(#{}).

req(Results) ->
    #{nova_server => nova_fake_server,
      owner => self(),
      fake_results => Results,
      method => <<"POST">>,
      version => 'HTTP/1.1',
      scheme => <<"http">>,
      host => <<"localhost">>,
      port => 8080,
      path => <<"/">>,
      qs => <<"a=1&b">>,
      headers => #{<<"cookie">> => <<"x=1; y=2">>},
      peer => {{127, 0, 0, 1}, 5555},
      has_body => true,
      body_length => 3}.

called() ->
    receive {nova_fake_server, Call} -> Call after 0 -> none end.

%%--------------------------------------------------------------------
%% The server in the request
%%--------------------------------------------------------------------

reply_sends_the_set_body_test() ->
    Req0 = nova_req:set_resp_body(<<"hi">>, req()),
    Req = nova_req:reply(201, #{<<"x-a">> => <<"1">>}, Req0),
    ?assertEqual({reply, 201, #{<<"x-a">> => <<"1">>}, <<"hi">>}, called()),
    ?assertNot(maps:is_key(resp_body, Req)).

reply_without_a_body_test() ->
    _ = nova_req:reply(204, req()),
    ?assertEqual({reply, 204, #{}, <<>>}, called()).

inform_and_streams_test() ->
    Req0 = req(),
    ok = nova_req:inform(103, Req0),
    ?assertEqual({inform, 103, #{}}, called()),
    Req = nova_req:stream_reply(200, Req0),
    ?assertEqual({stream_reply, 200, #{}}, called()),
    ok = nova_req:stream_body(<<"a">>, nofin, Req),
    ?assertEqual({stream_body, <<"a">>, nofin}, called()),
    ok = nova_req:stream_events(#{data => <<"one">>}, nofin, Req),
    ?assertEqual({stream_body, <<"data: one\n\n">>, nofin}, called()),
    ok = nova_req:stream_events([#{data => <<"two">>}], fin, Req),
    ?assertEqual({stream_body, <<"data: two\n\n">>, fin}, called()),
    ok = nova_req:stream_trailers(#{<<"x-sum">> => <<"1">>}, Req),
    ?assertEqual({stream_trailers, #{<<"x-sum">> => <<"1">>}}, called()).

read_body_test() ->
    Req = req(#{read_body => {ok, <<"abc">>}}),
    ?assertMatch({ok, <<"abc">>, _}, nova_req:read_body(Req)),
    ?assertEqual({read_body, #{}}, called()).

read_parts_test() ->
    Req = req(#{read_part => {ok, #{<<"x-a">> => <<"1">>}}, read_part_body => {more, <<"p">>}}),
    ?assertMatch({ok, #{<<"x-a">> := <<"1">>}, _}, nova_req:read_part(Req)),
    ?assertEqual({read_part, #{length => 64000, period => 5000}}, called()),
    ?assertMatch({more, <<"p">>, _}, nova_req:read_part_body(Req)),
    ?assertEqual({read_part_body, #{}}, called()),
    ?assertMatch({done, _}, nova_req:read_part(req(#{read_part => done}), #{length => 10})),
    ?assertEqual({read_part, #{length => 10}}, called()).

%%--------------------------------------------------------------------
%% read_urlencoded_body/1,2 and read_and_match_urlencoded_body/2,3
%%--------------------------------------------------------------------

urlencoded_body_test() ->
    Req = req(#{read_body => {ok, <<"a=1&b">>}}),
    ?assertMatch({ok, [{<<"a">>, <<"1">>}, {<<"b">>, true}], _}, nova_req:read_urlencoded_body(Req)),
    ?assertEqual({read_body, #{length => 64000, period => 5000}}, called()).

urlencoded_body_merges_the_defaults_test() ->
    Req = req(#{read_body => {ok, <<"a=1">>}}),
    {ok, _, _} = nova_req:read_urlencoded_body(Req, #{length => 10}),
    ?assertEqual({read_body, #{length => 10, period => 5000}}, called()).

urlencoded_body_max_keys_test() ->
    Req = req(#{read_body => {ok, <<"a=1&b=2&c=3">>}}),
    ?assertExit({request_error, urlencoded_body, _},
                nova_req:read_urlencoded_body(Req, #{max_keys => 2})).

malformed_urlencoded_body_test() ->
    Req = req(#{read_body => {ok, <<"a=%zz">>}}),
    ?assertExit({request_error, urlencoded_body, _}, nova_req:read_urlencoded_body(Req)).

urlencoded_body_not_received_in_time_test() ->
    Req = req(#{read_body => {more, <<"a=1">>}}),
    ?assertExit({request_error, timeout, _}, nova_req:read_urlencoded_body(Req)).

urlencoded_body_too_large_test() ->
    Req = req(#{read_body => {more, <<"a=12345">>}}),
    ?assertExit({request_error, payload_too_large, _},
                nova_req:read_urlencoded_body(Req, #{length => 4})).

read_and_match_urlencoded_body_test() ->
    Req = req(#{read_body => {ok, <<"a=1&b=x">>}}),
    ?assertMatch({ok, #{a := 1}, _},
                 nova_req:read_and_match_urlencoded_body([{a, int}], Req)).

read_and_match_urlencoded_body_fails_test() ->
    Req = req(#{read_body => {ok, <<"a=x">>}}),
    ?assertExit({request_error, {read_and_match_urlencoded_body, _}, _},
                nova_req:read_and_match_urlencoded_body([{a, int}], Req, #{})).

%%--------------------------------------------------------------------
%% Reading and setting the map
%%--------------------------------------------------------------------

bindings_test() ->
    Req = (req())#{bindings => #{<<"id">> => <<"7">>}},
    ?assertEqual(<<"7">>, nova_req:binding(<<"id">>, Req)),
    ?assertEqual(undefined, nova_req:binding(<<"other">>, Req)),
    ?assertEqual(none, nova_req:binding(<<"other">>, Req, none)),
    ?assertEqual(#{<<"id">> => <<"7">>}, nova_req:bindings(Req)).

no_bindings_test() ->
    ?assertEqual(none, nova_req:binding(<<"id">>, req(), none)),
    ?assertEqual(#{}, nova_req:bindings(req())).

query_and_cookies_test() ->
    Req = req(),
    ?assertEqual([{<<"a">>, <<"1">>}, {<<"b">>, true}], nova_req:parse_qs(Req, #{})),
    ?assertEqual(#{a => <<"1">>}, nova_req:match_qs([a], Req, #{})),
    ?assertEqual([{<<"x">>, <<"1">>}, {<<"y">>, <<"2">>}], nova_req:parse_cookies(Req, #{})),
    ?assertEqual(#{x => <<"1">>}, nova_req:match_cookies([x], Req, #{})),
    ?assertEqual([{<<"y">>, <<"2">>}], nova_req:parse_cookies(nova_req:filter_cookies([<<"y">>], Req))).

response_headers_test() ->
    Req = nova_req:set_resp_header(<<"x-a">>, <<"1">>, req()),
    ?assertEqual(<<"1">>, nova_req:resp_header(<<"x-a">>, Req)),
    ?assertEqual(none, nova_req:resp_header(<<"x-b">>, Req, none)),
    ?assertEqual(#{<<"x-a">> => <<"1">>}, nova_req:resp_headers(Req)).
