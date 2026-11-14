%%% A response that has begun is not answered a second time: not when the
%%% handler or the controller returns the request it streamed with, and not
%%% when it crashes after the first bytes went out.
-module(nova_streamed_response_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         handler_that_streamed_is_not_answered_again/1,
         controller_that_streamed_is_not_answered_again/1,
         crash_after_the_response_began_keeps_what_was_sent/1,
         next_request_is_served_after_a_crash_in_a_stream/1
        ]).

-export([handle_stream/3]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     handler_that_streamed_is_not_answered_again,
     controller_that_streamed_is_not_answered_again,
     crash_after_the_response_began_keeps_what_was_sent,
     next_request_is_served_after_a_crash_in_a_stream
    ].

init_per_suite(Config) ->
    Host = [#{prefix => "",
              routes => [
                         {"/handler", fun nova_streamed_response_pages:handler/1, #{methods => [get]}},
                         {"/controller", fun nova_streamed_response_pages:controller/1, #{methods => [get]}},
                         {"/crash", fun nova_streamed_response_pages:crash/1, #{methods => [get]}},
                         {"/plain", fun nova_streamed_response_pages:plain/1, #{methods => [get]}}
                        ]}],
    Port = nova_test_listener:start(#{host => Host}),
    ok = nova_handlers:register_handler(test_streamed, fun ?MODULE:handle_stream/3),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

handle_stream({test_streamed, Then}, _Callback, Req0) ->
    Req = cowboy_req:stream_reply(200, #{~"content-type" => ~"text/plain"}, Req0),
    ok = cowboy_req:stream_body(~"first-chunk", nofin, Req),
    case Then of
        finish ->
            ok = cowboy_req:stream_body(~"last-chunk", fin, Req),
            {ok, Req};
        crash ->
            error(crashed_in_the_stream)
    end.

handler_that_streamed_is_not_answered_again(Config) ->
    {{200, Body}, Events} = nova_test_log:collect(fun() -> nova_test_listener:get(?config(port, Config), "/handler") end),
    ?assertNotEqual(nomatch, binary:match(Body, ~"first-chunk")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"last-chunk")),
    ?assertEqual([], at_error_level(Events)).

controller_that_streamed_is_not_answered_again(Config) ->
    {{200, Body}, Events} =
        nova_test_log:collect(fun() -> nova_test_listener:get(?config(port, Config), "/controller") end),
    ?assertNotEqual(nomatch, binary:match(Body, ~"first-chunk")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"last-chunk")),
    ?assertEqual([], at_error_level(Events)).

%% The crash is logged, and the 500 page it would have had is not sent into
%% the response that is under way.
crash_after_the_response_began_keeps_what_was_sent(Config) ->
    {{200, Body}, Events} = nova_test_log:collect(fun() -> nova_test_listener:get(?config(port, Config), "/crash") end),
    ?assertNotEqual(nomatch, binary:match(Body, ~"first-chunk")),
    ?assertEqual(nomatch, binary:match(Body, ~"500")),
    ?assertMatch([{error, {report, #{msg := ~"Controller crashed", reason := crashed_in_the_stream}}}],
                 at_error_level(Events)).

%% Both requests travel on one connection, so the second one is only served
%% when the first left the connection process alive.
next_request_is_served_after_a_crash_in_a_stream(Config) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config), [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, ~"GET /crash HTTP/1.1\r\nhost: localhost\r\n\r\n"),
        _Crashed = receive_until(Socket, ~"0\r\n\r\n", <<>>),
        ok = gen_tcp:send(Socket, ~"GET /plain HTTP/1.1\r\nhost: localhost\r\n\r\n"),
        ?assertMatch(<<"HTTP/1.1 200", _/binary>>, receive_until(Socket, ~"still-here", <<>>))
    after
        gen_tcp:close(Socket)
    end.

receive_until(Socket, Expected, Acc) ->
    case binary:match(Acc, Expected) of
        nomatch ->
            {ok, <<Data/binary>>} = gen_tcp:recv(Socket, 0, 5000),
            receive_until(Socket, Expected, <<Acc/binary, Data/binary>>);
        _Found ->
            Acc
    end.

at_error_level(Events) ->
    [Event || {error, _Msg} = Event <- Events].
