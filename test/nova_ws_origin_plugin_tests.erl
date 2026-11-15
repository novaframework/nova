%%% The request shapes nova_ws_upgrade_SUITE cannot send over its listener.
-module(nova_ws_origin_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVIL, ~"https://evil.example").

pre_request(Req, Options) ->
    nova_ws_origin_plugin:pre_security(Req, #{}, Options, undefined).

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

same_origin_test_() ->
    SameOrigin = #{allowed_origins => same_origin},
    Https = fun(Origin) -> pre_request(http3_connect(#{~"origin" => Origin}), SameOrigin) end,
    OverTls = fun(Origin) ->
                      pre_request(#{version => 'HTTP/1.1', method => ~"GET", scheme => ~"https",
                                    host => ~"example.com", port => 443, path => ~"/ws",
                                    headers => #{~"host" => ~"example.com", ~"connection" => ~"upgrade",
                                                 ~"upgrade" => ~"websocket", ~"origin" => Origin}},
                                  SameOrigin)
              end,
    [{"the origin of an https request is accepted",
      [?_assertMatch({ok, _Req, undefined}, Https(~"https://example.com")),
       ?_assertMatch({ok, _Req, undefined}, Https(~"https://example.com:443")),
       ?_assertMatch({ok, _Req, undefined}, Https(~"HTTPS://Example.COM")),
       ?_assertMatch({ok, _Req, undefined}, OverTls(~"https://example.com")),
       ?_assertMatch({ok, _Req, undefined}, OverTls(~"https://example.com:443"))]},
     {"a page that was loaded without TLS is refused",
      [?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"http://example.com")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"http://example.com:443")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, OverTls(~"http://example.com")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, OverTls(~"http://example.com:443"))]},
     {"same_host accepts that page over HTTP/1.1",
      ?_assertMatch({ok, _Req, undefined},
                    pre_request(#{version => 'HTTP/1.1', method => ~"GET", scheme => ~"https",
                                  host => ~"example.com", port => 443, path => ~"/ws",
                                  headers => #{~"host" => ~"example.com", ~"connection" => ~"upgrade",
                                               ~"upgrade" => ~"websocket", ~"origin" => ~"http://example.com"}},
                                #{}))},
     {"another port or host is refused",
      [?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"https://example.com:8443")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"https://evil.example")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"https://exa mple.com")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"example.com")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"null"))]},
     {"a request without an origin is accepted",
      ?_assertMatch({ok, _Req, undefined}, pre_request(http3_connect(#{}), SameOrigin))}].

unparseable_upgrade() ->
    #{version => 'HTTP/1.1', method => ~"GET", host => ~"localhost", port => 80, path => ~"/ws",
      headers => #{~"host" => ~"localhost", ~"connection" => ~"upgrade, \"", ~"upgrade" => ~"websocket",
                   ~"origin" => ?EVIL}}.

unparseable_upgrade_is_checked_test() ->
    Req = unparseable_upgrade(),
    ?assertExit({request_error, _Header, _Text}, cowboy_websocket:is_upgrade_request(Req)),
    ?assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, pre_request(Req, #{})),
    ?assertMatch({ok, _Req, undefined}, pre_request(Req, #{allowed_origins => [?EVIL]})).
