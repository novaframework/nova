%%% What only Roadrunner serves: HTTP/3 next to HTTP/1.1 and HTTP/2 on a
%%% listener with TLS, which it tells clients of.
-module(nova_roadrunner_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         tls_listener_serves_http3_and_says_so/1,
         listener_without_http3_does_not_offer_it/1,
         listener_is_known_to_roadrunner_by_its_name/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).

all() ->
    [
     tls_listener_serves_http3_and_says_so,
     listener_without_http3_does_not_offer_it,
     listener_is_known_to_roadrunner_by_its_name
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

tls_listener_serves_http3_and_says_so(_Config) ->
    Port = start(#{tls => tls(), protocols => [http1, http2, http3]}),
    {200, Headers, Body} = https_get(Port, "/where"),
    ?assertEqual(~"web http1", Body),
    ?assertNotEqual(nomatch, binary:match(maps:get(~"alt-svc", Headers), <<"h3=\":", (integer_to_binary(Port))/binary>>)),
    ?assertEqual(Port, roadrunner_listener:quic_port(nova_roadrunner:roadrunner_name(web))),
    ?assertEqual({error, eaddrinuse}, gen_udp:open(Port, [{ip, {0, 0, 0, 0}}])).

listener_without_http3_does_not_offer_it(_Config) ->
    Port = start(#{tls => tls(), protocols => [http1, http2]}),
    {200, Headers, _Body} = https_get(Port, "/where"),
    ?assertNot(is_map_key(~"alt-svc", Headers)),
    {ok, Socket} = gen_udp:open(Port, [{ip, {0, 0, 0, 0}}]),
    ok = gen_udp:close(Socket).

listener_is_known_to_roadrunner_by_its_name(_Config) ->
    Port = start(#{}),
    ?assertEqual(Port, roadrunner_listener:port(nova_roadrunner:roadrunner_name(web))),
    ?assertEqual([nova_listener_web], roadrunner:listeners()).

start(Settings) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{server => nova_roadrunner,
                                                port => Port,
                                                listeners => #{web => Settings#{port => Port}},
                                                host => [#{routes => [?WHERE]}]}),
    Port.

%% A certificate made for the run, with its key.
tls() ->
    Key = [{key, {namedCurve, secp256r1}}, {digest, sha256}],
    #{server_config := Config} =
        public_key:pkix_test_data(#{server_chain => #{root => Key, peer => Key},
                                    client_chain => #{root => Key, peer => Key}}),
    Config.

https_get(Port, Path) ->
    {ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {verify, verify_none},
                                                   {alpn_advertised_protocols, [~"http/1.1"]}], 5000),
    try
        ok = ssl:send(Socket, ["GET ", Path, " HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"]),
        Response = until_closed(Socket, <<>>),
        [Head, Body] = binary:split(Response, ~"\r\n\r\n"),
        [<<"HTTP/1.1 ", Status:3/binary, _/binary>> | Lines] = binary:split(Head, ~"\r\n", [global]),
        {binary_to_integer(Status),
         #{string:lowercase(Name) => string:trim(Value) || Line <- Lines, [Name, Value] <- [binary:split(Line, ~":")]},
         Body}
    after
        ssl:close(Socket)
    end.

until_closed(Socket, Acc) ->
    case ssl:recv(Socket, 0, 5000) of
        {ok, Data} -> until_closed(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.
