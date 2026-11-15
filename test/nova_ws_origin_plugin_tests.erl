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
    SameOrigin = #{trusted_origins => same_origin},
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
     {"same_host refuses that page too, and accepts it on a listener without TLS",
      [?_assertMatch({Scheme, {Answer, _}},
                     {Scheme,
                      begin
                          Answered = pre_request(#{version => 'HTTP/1.1', method => ~"GET", scheme => Scheme,
                                                   host => ~"example.com", port => 443, path => ~"/ws",
                                                   headers => #{~"host" => ~"example.com",
                                                                ~"connection" => ~"upgrade",
                                                                ~"upgrade" => ~"websocket",
                                                                ~"origin" => ~"http://example.com"}},
                                                 #{}),
                          {element(1, Answered), Answered}
                      end})
       || {Scheme, Answer} <- [{~"https", stop}, {~"http", ok}]]},
     {"another port or host is refused",
      [?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"https://example.com:8443")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"https://evil.example")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"https://exa mple.com")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"example.com")),
       ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Https(~"null"))]},
     {"a request without an origin is accepted",
      ?_assertMatch({ok, _Req, undefined}, pre_request(http3_connect(#{}), SameOrigin))}].

%% What a proxy that sends `$host:$server_port` makes of the upgrade.
host_header_with_the_port_of_the_scheme_test_() ->
    Upgrade = fun(Host, Origin) ->
                      pre_request(#{version => 'HTTP/1.1', method => ~"GET", scheme => ~"http",
                                    host => ~"example.com", port => 8080, path => ~"/ws",
                                    headers => #{~"host" => Host, ~"connection" => ~"upgrade",
                                                 ~"upgrade" => ~"websocket", ~"origin" => Origin}},
                                  #{})
              end,
    [?_assertMatch({ok, _Req, undefined}, Upgrade(~"example.com:443", ~"https://example.com")),
     ?_assertMatch({ok, _Req, undefined}, Upgrade(~"Example.com:80", ~"http://example.com")),
     ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Upgrade(~"example.com:443", ~"http://example.com")),
     ?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, Upgrade(~"example.com:443", ~"https://evil.example"))].

unparseable_upgrade() ->
    #{version => 'HTTP/1.1', method => ~"GET", host => ~"localhost", port => 80, path => ~"/ws",
      headers => #{~"host" => ~"localhost", ~"connection" => ~"upgrade, \"", ~"upgrade" => ~"websocket",
                   ~"origin" => ?EVIL}}.

unparseable_upgrade_is_checked_test() ->
    Req = unparseable_upgrade(),
    ?assertExit({request_error, _Header, _Text}, cowboy_websocket:is_upgrade_request(Req)),
    ?assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, pre_request(Req, #{})),
    ?assertMatch({ok, _Req, undefined}, pre_request(Req, #{trusted_origins => [?EVIL]})).

any_origin_test() ->
    ?assertMatch({ok, _Req, undefined}, pre_request(http3_connect(#{~"origin" => ?EVIL}), #{trusted_origins => any})).

plugin_without_origins_of_its_own_test_() ->
    Accepted = http3_connect(#{~"origin" => ~"https://only.example.com"}),
    {setup,
     fun() -> nova_test_settings:set(trusted_origins, [~"https://only.example.com"]) end,
     fun(_) -> nova_test_settings:unset(trusted_origins) end,
     [?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined},
                    pre_request(http3_connect(#{~"origin" => ~"https://example.com"}), #{})),
      ?_assertEqual({ok, Accepted, undefined}, pre_request(Accepted, #{})),
      ?_assertMatch({ok, _Req, undefined},
                    pre_request(http3_connect(#{~"origin" => ~"https://example.com"}),
                                #{trusted_origins => same_host}))]}.

%% Nova lists the plugin first on every websocket route, where it checks
%% every request and not only the ones that ask for an upgrade.
request_to_a_websocket_route_that_is_no_upgrade_test_() ->
    Websocket = #{cowboy_handler => nova_ws_handler},
    [?_assertMatch({stop, {reply, 403, _Body}, _Req, undefined}, get_from_another_origin(Websocket, #{})),
     ?_assertMatch({ok, _Req, undefined}, get_from_another_origin(Websocket, #{trusted_origins => [?EVIL]})),
     ?_assertMatch({ok, _Req, undefined}, get_from_another_origin(#{cowboy_handler => nova_handler}, #{})),
     ?_assertMatch({ok, _Req, undefined}, get_from_another_origin(#{}, #{}))].

get_from_another_origin(Env, Options) ->
    Get = #{version => 'HTTP/1.1', method => ~"GET", scheme => ~"http", host => ~"example.com", port => 80,
            path => ~"/ws", headers => #{~"host" => ~"example.com", ~"origin" => ?EVIL}},
    pre_security(Get, Env, Options).

pre_security(Req, Env, Options) ->
    nova_ws_origin_plugin:pre_security(Req, Env, Options, undefined).
