%%% What a websocket controller and nova_ws_origin_plugin decide about the
%%% upgrade request.
-module(nova_ws_upgrade_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([init_options_reach_the_connection/1,
         connection_without_options_keeps_the_defaults/1,
         other_origin_is_refused/1,
         same_host_origin_is_accepted/1,
         same_host_is_compared_without_case/1,
         missing_origin_is_accepted/1,
         listed_origin_is_accepted/1,
         unlisted_origin_is_refused/1,
         origin_plugin_leaves_other_requests_alone/1,
         route_without_the_plugin_accepts_any_origin/1,
         other_origin_is_refused_over_http2/1,
         same_host_origin_is_accepted_over_http2/1,
         same_host_needs_the_same_port_over_http2/1,
         missing_origin_is_accepted_over_http2/1,
         listed_origin_is_checked_over_http2/1,
         route_without_the_plugin_accepts_any_origin_over_http2/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(SMALL_FRAME, 16).

all() ->
    [init_options_reach_the_connection,
     connection_without_options_keeps_the_defaults,
     other_origin_is_refused,
     same_host_origin_is_accepted,
     same_host_is_compared_without_case,
     missing_origin_is_accepted,
     listed_origin_is_accepted,
     unlisted_origin_is_refused,
     origin_plugin_leaves_other_requests_alone,
     route_without_the_plugin_accepts_any_origin,
     other_origin_is_refused_over_http2,
     same_host_origin_is_accepted_over_http2,
     same_host_needs_the_same_port_over_http2,
     missing_origin_is_accepted_over_http2,
     listed_origin_is_checked_over_http2,
     route_without_the_plugin_accepts_any_origin_over_http2].

init_per_suite(Config) ->
    Routes = [#{prefix => "/plain",
                plugins => [],
                routes => [{"/ws", nova_ws_upgrade_pages, #{protocol => ws}}]},
              #{prefix => "/small",
                plugins => [],
                routes => [{"/ws", nova_ws_upgrade_pages, #{protocol => ws}}]},
              #{prefix => "/same-host",
                plugins => [{pre_request, nova_ws_origin_plugin, #{}}],
                routes => [{"/ws", nova_ws_upgrade_pages, #{protocol => ws}},
                           {"/page", fun nova_ws_upgrade_pages:page/1, #{methods => [get]}}]},
              #{prefix => "/listed",
                plugins => [{pre_request, nova_ws_origin_plugin,
                             #{allowed_origins => [~"https://app.example.com", "https://other.example.com"]}}],
                routes => [{"/ws", nova_ws_upgrade_pages, #{protocol => ws}}]}],
    [{port, nova_test_listener:start(#{host => Routes, cowboy_options => #{enable_connect_protocol => true}})}
     | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_options_reach_the_connection(Config) ->
    Port = ?config(port, Config),
    Fits = binary:copy(~"a", ?SMALL_FRAME),
    ?assertEqual([Fits], nova_test_listener:websocket_texts(Port, "/small/ws", Fits, 1)),
    ?assertEqual([closed],
                 nova_test_listener:websocket_texts(Port, "/small/ws", binary:copy(~"a", ?SMALL_FRAME + 1), 1)).

connection_without_options_keeps_the_defaults(Config) ->
    TooBigForSmall = binary:copy(~"a", ?SMALL_FRAME + 1),
    ?assertEqual([TooBigForSmall],
                 nova_test_listener:websocket_texts(?config(port, Config), "/plain/ws", TooBigForSmall, 1)).

other_origin_is_refused(Config) ->
    ?assertMatch(<<"HTTP/1.1 403", _/binary>>, handshake(Config, "/same-host/ws", "https://evil.example")).

same_host_origin_is_accepted(Config) ->
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>, handshake(Config, "/same-host/ws", "http://localhost")).

same_host_is_compared_without_case(Config) ->
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>, handshake(Config, "/same-host/ws", "HTTP://LocalHost")).

missing_origin_is_accepted(Config) ->
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/same-host/ws")).

listed_origin_is_accepted(Config) ->
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>, handshake(Config, "/listed/ws", "https://app.example.com")),
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>, handshake(Config, "/listed/ws", "https://other.example.com")).

unlisted_origin_is_refused(Config) ->
    ?assertMatch(<<"HTTP/1.1 403", _/binary>>, handshake(Config, "/listed/ws", "http://localhost")),
    ?assertMatch(<<"HTTP/1.1 403", _/binary>>, handshake(Config, "/listed/ws", "https://app.example.com.evil.example")),
    ?assertMatch(<<"HTTP/1.1 403", _/binary>>, handshake(Config, "/listed/ws", "null")).

origin_plugin_leaves_other_requests_alone(Config) ->
    ?assertEqual({200, ~"page"},
                 nova_test_listener:get(?config(port, Config), "/same-host/page",
                                        [{"origin", "https://evil.example"}])).

route_without_the_plugin_accepts_any_origin(Config) ->
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>, handshake(Config, "/plain/ws", "https://evil.example")).

other_origin_is_refused_over_http2(Config) ->
    ?assertEqual(403, connect(Config, "/same-host/ws", authority(Config), [{~"origin", ~"https://evil.example"}])).

same_host_origin_is_accepted_over_http2(Config) ->
    Authority = authority(Config),
    ?assertEqual(200, connect(Config, "/same-host/ws", Authority, [{~"origin", <<"http://", Authority/binary>>}])),
    ?assertEqual(200, connect(Config, "/same-host/ws", Authority, [{~"origin", <<"HTTP://LocalHost:", (port(Config))/binary>>}])),
    ?assertEqual(200, connect(Config, "/same-host/ws", ~"localhost", [{~"origin", ~"http://localhost"}])),
    ?assertEqual(200, connect(Config, "/same-host/ws", ~"localhost:443", [{~"origin", ~"https://localhost"}])).

same_host_needs_the_same_port_over_http2(Config) ->
    ?assertEqual(403, connect(Config, "/same-host/ws", authority(Config), [{~"origin", ~"http://localhost"}])),
    ?assertEqual(403, connect(Config, "/same-host/ws", authority(Config), [{~"origin", ~"http://localhost:1"}])),
    ?assertEqual(403, connect(Config, "/same-host/ws", ~"localhost", [{~"origin", ~"https://localhost"}])),
    ?assertEqual(403, connect(Config, "/same-host/ws", ~"localhost", [{~"origin", ~"null"}])).

missing_origin_is_accepted_over_http2(Config) ->
    ?assertEqual(200, connect(Config, "/same-host/ws", authority(Config), [])).

listed_origin_is_checked_over_http2(Config) ->
    ?assertEqual(200, connect(Config, "/listed/ws", authority(Config), [{~"origin", ~"https://app.example.com"}])),
    ?assertEqual(403, connect(Config, "/listed/ws", authority(Config), [{~"origin", ~"https://evil.example"}])).

route_without_the_plugin_accepts_any_origin_over_http2(Config) ->
    ?assertEqual(200, connect(Config, "/plain/ws", authority(Config), [{~"origin", ~"https://evil.example"}])).

connect(Config, Path, Authority, Headers) ->
    nova_test_listener:h2_websocket_connect(?config(port, Config), Path, Authority, Headers).

authority(Config) ->
    <<"localhost:", (port(Config))/binary>>.

port(Config) ->
    integer_to_binary(?config(port, Config)).

handshake(Config, Path, Origin) ->
    nova_test_listener:websocket_handshake(?config(port, Config), Path, [{"origin", Origin}]).
