%%% The request shapes nova_ws_upgrade_SUITE cannot send over its listener.
-module(nova_ws_origin_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVIL, ~"https://evil.example").

pre_request(Req, Options) ->
    nova_ws_origin_plugin:pre_request(Req, #{}, Options, undefined).

http3_connect(Headers) ->
    #{version => 'HTTP/3', method => ~"CONNECT", protocol => ~"websocket", scheme => ~"https",
      host => ~"example.com", port => 443, path => ~"/ws", headers => Headers}.

http3_upgrade_test_() ->
    [{"another origin is refused",
      ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined},
                    pre_request(http3_connect(#{~"origin" => ?EVIL}), #{}))},
     {"the origin of the request is accepted",
      ?_assertMatch({ok, _Req, undefined},
                    pre_request(http3_connect(#{~"origin" => ~"https://example.com"}), #{}))},
     {"the same host on another port is refused",
      ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined},
                    pre_request(http3_connect(#{~"origin" => ~"https://example.com:8443"}), #{}))},
     {"an origin that is not a host is refused",
      ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined},
                    pre_request(http3_connect(#{~"origin" => ~"https://exa mple.com"}), #{}))},
     {"a CONNECT for another protocol is left alone",
      ?_assertMatch({ok, _Req, undefined},
                    pre_request((http3_connect(#{~"origin" => ?EVIL}))#{protocol => ~"connect-udp"}, #{}))}].

unparseable_upgrade_is_checked_test() ->
    Req = #{version => 'HTTP/1.1', method => ~"GET", host => ~"localhost", port => 80, path => ~"/ws",
            headers => #{~"host" => ~"localhost", ~"connection" => ~"upgrade, \"", ~"upgrade" => ~"websocket",
                         ~"origin" => ?EVIL}},
    ?assertExit({request_error, _Header, _Text}, cowboy_websocket:is_upgrade_request(Req)),
    ?assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, pre_request(Req, #{})),
    ?assertMatch({ok, _Req, undefined}, pre_request(Req, #{allowed_origins => [?EVIL]})).
