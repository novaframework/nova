%%% What only Roadrunner serves: HTTP/3 next to HTTP/1.1 and HTTP/2 on a
%%% listener with TLS, which it tells clients of.
-module(nova_roadrunner_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         http3_alt_svc/1,
         no_alt_svc/1,
         roadrunner_name/1,
         websocket_needs_http1/1,
         logger_metadata/1,
         http_versions/1,
         crashed_stream/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).

all() ->
    [
     http3_alt_svc,
     no_alt_svc,
     roadrunner_name,
     websocket_needs_http1,
     logger_metadata,
     http_versions,
     crashed_stream
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

http3_alt_svc(_Config) ->
    Port = start(#{tls => nova_test_listener:tls(), protocols => [http1, http2, http3]}),
    {200, Headers, Body} = nova_test_listener:https_get(Port, "/where"),
    ?assertEqual(~"web http1", Body),
    ?assertNotEqual(nomatch, binary:match(maps:get(~"alt-svc", Headers), <<"h3=\":", (integer_to_binary(Port))/binary>>)),
    ?assertEqual(Port, roadrunner_listener:quic_port(nova_roadrunner:roadrunner_name(web))),
    ?assertEqual({error, eaddrinuse}, gen_udp:open(Port, [{ip, {0, 0, 0, 0}}])).

no_alt_svc(_Config) ->
    Port = start(#{tls => nova_test_listener:tls(), protocols => [http1, http2]}),
    {200, Headers, _Body} = nova_test_listener:https_get(Port, "/where"),
    ?assertNot(is_map_key(~"alt-svc", Headers)),
    {ok, Socket} = gen_udp:open(Port, [{ip, {0, 0, 0, 0}}]),
    ok = gen_udp:close(Socket).

roadrunner_name(_Config) ->
    Port = start(#{}),
    ?assertEqual(Port, roadrunner_listener:port(nova_roadrunner:roadrunner_name(web))),
    ?assert(lists:member(nova_listener_web, roadrunner:listeners())).

%% Roadrunner takes WebSockets over HTTP/1.1, so a WebSocket route on a
%% listener without it could never be reached.
websocket_needs_http1(_Config) ->
    Port = nova_test_listener:free_port(),
    Socket = {"/ws", nova_ws_lifecycle_pages, #{protocol => ws}},
    Start = fun(Protocols, Routes) ->
                    nova_test_listener:try_start(#{server => nova_roadrunner,
                                                   port => Port,
                                                   listeners => #{web => #{port => Port, protocols => Protocols}},
                                                   host => [#{routes => Routes}]})
            end,
    ?assertMatch({error, {nova_roadrunner,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {websocket_route_not_served, "/ws", web}}}, _}}},
                 Start([http2], [?WHERE, Socket])),
    ok = nova_test_listener:stop(),
    ?assertEqual({ok, Port}, Start([http2], [?WHERE])),
    ok = nova_test_listener:stop(),
    ?assertEqual({ok, Port}, Start([http1], [?WHERE, Socket])).

%% The requests of a connection run in its process, one after the other.
logger_metadata(_Config) ->
    Port = nova_test_listener:free_port(),
    Metadata = {"/metadata", fun nova_listeners_pages:metadata/1, #{methods => [get]}},
    {ok, Port} = nova_test_listener:try_start(#{server => nova_roadrunner,
                                                port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [Metadata]}]}),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    try
        [?assertEqual(nomatch, binary:match(kept_get(Socket, "/metadata"), ~"alice")) || _Request <- [1, 2, 3]]
    after
        gen_tcp:close(Socket)
    end.

%% With TLS a listener serves both versions on its port.
http_versions(_Config) ->
    Port = nova_test_listener:free_port(),
    Routes = [{Path, fun nova_listeners_pages:where/1, #{methods => [get], http_versions => Versions}}
              || {Path, Versions} <- [{"/one", [http1]}, {"/two", [http2]}]],
    {ok, Port} = nova_test_listener:try_start(#{server => nova_roadrunner,
                                                port => Port,
                                                listeners => #{web => #{port => Port,
                                                                        tls => nova_test_listener:tls(),
                                                                        protocols => [http1, http2]}},
                                                host => [#{routes => Routes}]}),
    ?assertMatch({200, _, ~"web http1"}, nova_test_listener:https_get(Port, "/one")),
    ?assertEqual(505, nova_test_listener:h2s_get(Port, ~"/one")),
    ?assertMatch({426, #{~"upgrade" := ~"HTTP/2.0"}, _}, nova_test_listener:https_get(Port, "/two")),
    ?assertEqual(200, nova_test_listener:h2s_get(Port, ~"/two")).

%% The controller is gone, so the response it began is ended for it and the
%% client is not left waiting for the rest.
crashed_stream(_Config) ->
    Port = nova_test_listener:free_port(),
    Crash = {"/crash", fun nova_streamed_response_pages:crash/1, #{methods => [get]}},
    {ok, Port} = nova_test_listener:try_start(#{server => nova_roadrunner,
                                                port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [Crash]}]}),
    ok = nova_handlers:register_handler(test_streamed, fun nova_streamed_response_SUITE:handle_stream/3),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    try
        ok = gen_tcp:send(Socket, ~"GET /crash HTTP/1.1\r\nhost: localhost\r\n\r\n"),
        Response = until_closed(Socket, <<>>),
        ?assertMatch(<<"HTTP/1.1 200", _/binary>>, Response),
        ?assertNotEqual(nomatch, binary:match(Response, ~"first-chunk")),
        ?assertEqual(~"0\r\n\r\n", binary:part(Response, byte_size(Response), -5))
    after
        gen_tcp:close(Socket)
    end.

until_closed(Socket, Received) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, More} when is_binary(More) -> until_closed(Socket, <<Received/binary, More/binary>>);
        {error, closed} -> Received
    end.

%% The body of a response on a connection that stays open.
kept_get(Socket, Path) ->
    ok = gen_tcp:send(Socket, ["GET ", Path, " HTTP/1.1\r\nhost: localhost\r\n\r\n"]),
    {Head, Rest} = until_body(Socket, <<>>),
    {match, [Length]} = re:run(Head, ~"content-length: (\\d+)", [caseless, {capture, all_but_first, binary}]),
    rest_of_body(Socket, Rest, binary_to_integer(Length)).

until_body(Socket, Received) ->
    case binary:split(Received, ~"\r\n\r\n") of
        [Head, Rest] ->
            {Head, Rest};
        [_Incomplete] ->
            until_body(Socket, <<Received/binary, (received(Socket))/binary>>)
    end.

rest_of_body(_Socket, Body, Length) when byte_size(Body) >= Length ->
    Body;
rest_of_body(Socket, Body, Length) ->
    rest_of_body(Socket, <<Body/binary, (received(Socket))/binary>>, Length).

received(Socket) ->
    {ok, More} = gen_tcp:recv(Socket, 0, 5000),
    true = is_binary(More),
    More.

start(Settings) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{server => nova_roadrunner,
                                                port => Port,
                                                listeners => #{web => Settings#{port => Port}},
                                                host => [#{routes => [?WHERE]}]}),
    Port.
