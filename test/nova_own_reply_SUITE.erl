%%% A controller, plugin or security callback that answers the request with
%%% Cowboy and then returns as if it had not is answered once: what it
%%% returned is not sent, the connection goes on to serve the next request,
%%% and one warning gives the status of the response that was dropped.
-module(nova_own_reply_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         ok_return_after_an_own_reply/1,
         view_return_after_an_own_reply/1,
         json_return_after_an_own_reply/1,
         status_return_after_an_own_reply/1,
         redirect_return_after_an_own_reply/1,
         fallback_controller_after_an_own_reply/1,
         fallback_controller_that_replies_itself/1,
         crash_after_an_own_reply/1,
         thrown_status_after_an_own_reply/1,
         post_request_reply_after_an_own_reply/1,
         denial_after_an_own_reply/1,
         denial_with_a_body_after_an_own_reply/1,
         redirect_of_a_security_callback_after_an_own_reply/1,
         pre_request_reply_after_an_own_reply/1,
         pre_request_crash_after_an_own_reply/1,
         pipelined_request_after_a_second_answer/1,
         second_answer_over_http2/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PAGES, nova_own_reply_pages).

all() ->
    [
     ok_return_after_an_own_reply,
     view_return_after_an_own_reply,
     json_return_after_an_own_reply,
     status_return_after_an_own_reply,
     redirect_return_after_an_own_reply,
     fallback_controller_after_an_own_reply,
     fallback_controller_that_replies_itself,
     crash_after_an_own_reply,
     thrown_status_after_an_own_reply,
     post_request_reply_after_an_own_reply,
     denial_after_an_own_reply,
     denial_with_a_body_after_an_own_reply,
     redirect_of_a_security_callback_after_an_own_reply,
     pre_request_reply_after_an_own_reply,
     pre_request_crash_after_an_own_reply,
     pipelined_request_after_a_second_answer,
     second_answer_over_http2
    ].

init_per_suite(Config) ->
    Host = [#{prefix => "",
              plugins => [],
              routes => [
                         {"/ok", fun ?PAGES:ok/1, #{methods => [get]}},
                         {"/view", fun ?PAGES:view/1, #{methods => [get]}},
                         {"/json", fun ?PAGES:json/1, #{methods => [get]}},
                         {"/status", fun ?PAGES:status/1, #{methods => [get]}},
                         {"/redirect", fun ?PAGES:redirect/1, #{methods => [get]}},
                         {"/unsupported", fun ?PAGES:unsupported/1, #{methods => [get]}},
                         {"/to-fallback", fun ?PAGES:to_fallback/1, #{methods => [get]}},
                         {"/crash", fun ?PAGES:crash/1, #{methods => [get]}},
                         {"/thrown", fun ?PAGES:thrown/1, #{methods => [get]}},
                         {"/plain", fun ?PAGES:plain/1, #{methods => [get]}},
                         {404, fun ?PAGES:not_found/1, #{}}
                        ]},
            #{prefix => "/plugin",
              plugins => [{post_request, ?PAGES, #{}}],
              routes => [{"/page", fun ?PAGES:before_plugin/1, #{methods => [get]}}]},
            #{prefix => "/pre-stop",
              plugins => [{pre_request, ?PAGES, #{then => stop}}],
              routes => [{"/page", fun ?PAGES:plain/1, #{methods => [get]}}]},
            #{prefix => "/pre-crash",
              plugins => [{pre_request, ?PAGES, #{then => crash}}],
              routes => [{"/page", fun ?PAGES:plain/1, #{methods => [get]}}]},
            #{prefix => "/denied",
              plugins => [],
              security => fun ?PAGES:denies/1,
              routes => [{"/page", fun ?PAGES:plain/1, #{methods => [get]}}]},
            #{prefix => "/denied-with-a-body",
              plugins => [],
              security => fun ?PAGES:denies_with_a_body/1,
              routes => [{"/page", fun ?PAGES:plain/1, #{methods => [get]}}]},
            #{prefix => "/redirected",
              plugins => [],
              security => fun ?PAGES:redirects/1,
              routes => [{"/page", fun ?PAGES:plain/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

ok_return_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 200}], answered_once(Config, "/ok")).

view_return_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 200}], answered_once(Config, "/view")).

json_return_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 200}], answered_once(Config, "/json")).

status_return_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 201}], answered_once(Config, "/status")).

redirect_return_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 302}], answered_once(Config, "/redirect")).

%% The controller answered and returned something only its fallback
%% controller understands.
fallback_controller_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 409}], answered_once(Config, "/unsupported")).

fallback_controller_that_replies_itself(Config) ->
    ?assertMatch([#{status := 409}], answered_once(Config, "/to-fallback")).

%% The 500 page is not sent after the answer of the controller.
crash_after_an_own_reply(Config) ->
    ?assertMatch([#{msg := ~"The response had begun when the controller failed, so no error page is sent"}],
                 answered_once(Config, "/crash")).

%% Nor is the 404 page the controller asks for by throwing.
thrown_status_after_an_own_reply(Config) ->
    ?assertMatch([#{msg := ~"The response had begun when the controller failed, so no error page is sent"}],
                 answered_once(Config, "/thrown")).

%% The plugin hands back a request that does not say it was answered, so
%% two responses are dropped. The warning is for the first.
post_request_reply_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 200}], answered_once(Config, "/plugin/page")).

%% A security callback that answers the request and then refuses it.
denial_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 401}], answered_once(Config, "/denied/page")).

denial_with_a_body_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 403}], answered_once(Config, "/denied-with-a-body/page")).

redirect_of_a_security_callback_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 302}], answered_once(Config, "/redirected/page")).

pre_request_reply_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 503}], answered_once(Config, "/pre-stop/page")).

pre_request_crash_after_an_own_reply(Config) ->
    ?assertMatch([#{status := 500}], answered_once(Config, "/pre-crash/page")).

%% The second request is already on the wire when the first is answered twice.
pipelined_request_after_a_second_answer(Config) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config), [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, [~"GET /denied/page HTTP/1.1\r\nhost: localhost\r\n\r\n",
                                   ~"GET /plain HTTP/1.1\r\nhost: localhost\r\n\r\n"]),
        Received = receive_until(Socket, ~"still-here", <<>>),
        ?assertEqual(2, length(binary:matches(Received, ~"HTTP/1.1 200")))
    after
        gen_tcp:close(Socket)
    end.

%% Two streams of one HTTP/2 connection, the first of them answered twice.
second_answer_over_http2(Config) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config), [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, [~"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
        {Denied, Encoder} = cow_hpack:encode(h2_get(~"/denied/page")),
        {Plain, _Encoder} = cow_hpack:encode(h2_get(~"/plain"), Encoder),
        Requests = [cow_http2:headers(1, fin, Denied), cow_http2:headers(3, fin, Plain)],
        ?assertEqual(#{1 => 200, 3 => 200}, h2_statuses(Socket, <<>>, Requests, cow_hpack:init(), #{}))
    after
        gen_tcp:close(Socket)
    end.

h2_get(Path) ->
    [{~":method", ~"GET"}, {~":scheme", ~"http"}, {~":path", Path}, {~":authority", ~"localhost"}].

%% The status of streams 1 and 3, or how each ended without one.
h2_statuses(_Socket, _Buffer, _Requests, _Decoder, Statuses) when map_size(Statuses) =:= 2 ->
    Statuses;
h2_statuses(Socket, Buffer, Requests, Decoder, Statuses) ->
    case cow_http2:parse(Buffer) of
        {ok, {settings, _Settings}, Rest} ->
            ok = gen_tcp:send(Socket, [cow_http2:settings_ack(), Requests]),
            h2_statuses(Socket, Rest, Requests, Decoder, Statuses);
        {ok, {headers, StreamID, _IsFin, head_fin, HeaderBlock}, Rest} ->
            {Headers, Decoder1} = cow_hpack:decode(HeaderBlock, Decoder),
            {~":status", <<Status/binary>>} = lists:keyfind(~":status", 1, Headers),
            h2_statuses(Socket, Rest, Requests, Decoder1, Statuses#{StreamID => binary_to_integer(Status)});
        {ok, {rst_stream, StreamID, Reason}, Rest} ->
            h2_statuses(Socket, Rest, Requests, Decoder, Statuses#{StreamID => {reset, Reason}});
        {ok, {goaway, _LastStreamID, Reason, _Debug}, _Rest} ->
            {goaway, Reason, Statuses};
        {ok, _OtherFrame, Rest} ->
            h2_statuses(Socket, Rest, Requests, Decoder, Statuses);
        more ->
            case gen_tcp:recv(Socket, 0, 5000) of
                {ok, <<Data/binary>>} -> h2_statuses(Socket, <<Buffer/binary, Data/binary>>, Requests, Decoder, Statuses);
                {error, Reason} -> {closed, Reason, Statuses}
            end
    end.

%% Asks for Path and then for /plain on one connection, which is only served
%% when the first request left the connection process alive. Returns the
%% reports logged at warning level.
answered_once(Config, Path) ->
    nova_once:forget({nova_stream_h, dropped}),
    nova_once:forget({nova_req, dropped}),
    {Received, Events} = nova_test_log:collect(fun() -> two_requests(?config(port, Config), Path) end),
    ?assertEqual(2, length(binary:matches(Received, ~"HTTP/1.1 "))),
    ?assertEqual(2, length(binary:matches(Received, ~"HTTP/1.1 200"))),
    ?assertEqual(nomatch, binary:match(Received, ~"from the")),
    [Report || {warning, {report, Report}} <- Events].

two_requests(Port, Path) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, ["GET ", Path, " HTTP/1.1\r\nhost: localhost\r\n\r\n"]),
        First = receive_until(Socket, ~"own-reply", <<>>),
        ok = gen_tcp:send(Socket, ~"GET /plain HTTP/1.1\r\nhost: localhost\r\n\r\n"),
        receive_until(Socket, ~"still-here", First)
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
