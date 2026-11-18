%%% What only Roadrunner serves: HTTP/3 next to HTTP/1.1 and HTTP/2 on a
%%% listener with TLS, which it tells clients of.
-module(nova_roadrunner_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         tls_listener_serves_http3_and_says_so/1,
         listener_without_http3_does_not_offer_it/1,
         listener_is_known_to_roadrunner_by_its_name/1,
         websocket_route_needs_a_listener_with_http1/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).

all() ->
    [
     tls_listener_serves_http3_and_says_so,
     listener_without_http3_does_not_offer_it,
     listener_is_known_to_roadrunner_by_its_name,
     websocket_route_needs_a_listener_with_http1
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

tls_listener_serves_http3_and_says_so(_Config) ->
    Port = start(#{tls => nova_test_listener:tls(), protocols => [http1, http2, http3]}),
    {200, Headers, Body} = nova_test_listener:https_get(Port, "/where"),
    ?assertEqual(~"web http1", Body),
    ?assertNotEqual(nomatch, binary:match(maps:get(~"alt-svc", Headers), <<"h3=\":", (integer_to_binary(Port))/binary>>)),
    ?assertEqual(Port, roadrunner_listener:quic_port(nova_roadrunner:roadrunner_name(web))),
    ?assertEqual({error, eaddrinuse}, gen_udp:open(Port, [{ip, {0, 0, 0, 0}}])).

listener_without_http3_does_not_offer_it(_Config) ->
    Port = start(#{tls => nova_test_listener:tls(), protocols => [http1, http2]}),
    {200, Headers, _Body} = nova_test_listener:https_get(Port, "/where"),
    ?assertNot(is_map_key(~"alt-svc", Headers)),
    {ok, Socket} = gen_udp:open(Port, [{ip, {0, 0, 0, 0}}]),
    ok = gen_udp:close(Socket).

listener_is_known_to_roadrunner_by_its_name(_Config) ->
    Port = start(#{}),
    ?assertEqual(Port, roadrunner_listener:port(nova_roadrunner:roadrunner_name(web))),
    ?assertEqual([nova_listener_web], roadrunner:listeners()).

%% Roadrunner takes WebSockets over HTTP/1.1, so a WebSocket route on a
%% listener without it could never be reached.
websocket_route_needs_a_listener_with_http1(_Config) ->
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

start(Settings) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{server => nova_roadrunner,
                                                port => Port,
                                                listeners => #{web => Settings#{port => Port}},
                                                host => [#{routes => [?WHERE]}]}),
    Port.
