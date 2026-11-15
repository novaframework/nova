%%% nova_origin:is_allowed/3 on what a client can put in `Origin` and `Host`,
%%% clause by clause.
-module(nova_origin_tests).

-include_lib("eunit/include/eunit.hrl").

%% An HTTP/1.1 request, which names where it was sent in the `Host` header.
with_host(Host) ->
    with_host(Host, ~"https").

with_host(Host, Scheme) ->
    #{version => 'HTTP/1.1', method => ~"GET", scheme => Scheme, host => ~"unused.example", port => 1,
      path => ~"/", headers => #{~"host" => Host}}.

%% An HTTP/2 request, which has no `Host` header.
to_authority(Scheme, Host, Port) ->
    #{version => 'HTTP/2', method => ~"GET", scheme => Scheme, host => Host, port => Port, path => ~"/",
      headers => #{}}.

allowed(Origin, Req, Allowed) ->
    try nova_origin:is_allowed(Origin, Req, Allowed)
    catch Class:Reason -> {raised, Class, Reason}
    end.

no_origin_and_any_test_() ->
    [?_assert(allowed(undefined, with_host(~"example.com"), Allowed))
     || Allowed <- [same_host, same_origin, any, [], [~"https://example.com"]]] ++
        [?_assert(allowed(Origin, with_host(~"example.com"), any))
         || Origin <- [~"https://evil.example", ~"null", ~"", ~"://"]].

same_host_test_() ->
    Accepted = [{~"https://example.com", ~"example.com"},
                {~"https://example.com:8443", ~"example.com:8443"},
                {~"https://[::1]", ~"[::1]"},
                {~"https://[::1]:8080", ~"[::1]:8080"},
                {~"https://[2001:DB8::1]:8080", ~"[2001:db8::1]:8080"},
                {~"https://example.com.", ~"example.com."}],
    Refused = [{~"https://evil.example", ~"example.com"},
               {~"https://example.com.evil.example", ~"example.com"},
               {~"https://example.com:8443", ~"example.com"},
               {~"https://example.com", ~"example.com:8443"},
               {~"https://example.com:8443", ~"example.com:9443"},
               {~"https://example.com.", ~"example.com"},
               {~"https://example.com", ~"example.com."},
               {~"https://[::1]", ~"[::2]"},
               {~"https://[0:0:0:0:0:0:0:1]", ~"[::1]"},
               {~"https://x@example.com", ~"example.com"},
               {~"https://example.com/", ~"example.com"},
               {~"https://example.com, https://example.com", ~"example.com"}],
    [[?_assertEqual({Origin, Host, true}, {Origin, Host, allowed(Origin, with_host(Host), same_host)})
      || {Origin, Host} <- Accepted],
     [?_assertEqual({Origin, Host, false}, {Origin, Host, allowed(Origin, with_host(Host), same_host)})
      || {Origin, Host} <- Refused]].

%% A page that was not loaded over TLS can be written by anybody on the way
%% to the browser, so it does not speak for the host to a listener that has
%% TLS. A listener without TLS cannot tell what the browser used: a proxy in
%% front of it may have taken the TLS off, and no header of the request is
%% trusted to say so. It goes by host and port alone.
same_host_of_a_listener_with_tls_needs_an_origin_with_tls_test_() ->
    Refused = [{~"http://example.com", ~"example.com"},
               {~"HTTP://example.com", ~"example.com"},
               {~"http://example.com:443", ~"example.com:443"},
               {~"http://example.com:8443", ~"example.com:8443"},
               {~"http://[::1]:8080", ~"[::1]:8080"},
               {~"ws://example.com", ~"example.com"},
               {~"wss://example.com", ~"example.com"}],
    Accepted = [{~"http://example.com", ~"example.com"},
                {~"https://example.com", ~"example.com"},
                {~"http://[2001:DB8::1]:8080", ~"[2001:db8::1]:8080"},
                {~"https://example.com:8443", ~"example.com:8443"},
                {~"ws://example.com", ~"example.com"}],
    [[?_assertEqual({Origin, Host, false}, {Origin, Host, allowed(Origin, with_host(Host), same_host)})
      || {Origin, Host} <- Refused],
     [?_assertEqual({Origin, Host, true}, {Origin, Host, allowed(Origin, with_host(Host, ~"http"), same_host)})
      || {Origin, Host} <- Accepted],
     ?_assertNot(allowed(~"http://example.com:443", to_authority(~"https", ~"example.com", 443), same_host)),
     ?_assert(allowed(~"http://example.com", to_authority(~"http", ~"example.com", 80), same_host))].

%% A proxy that is told to send `$host:$server_port` names the port a
%% browser leaves out of the origin.
same_host_with_the_port_of_the_scheme_test_() ->
    [?_assert(allowed(~"https://example.com", with_host(~"example.com:443"), same_host)),
     ?_assert(allowed(~"http://example.com", with_host(~"example.com:80", ~"http"), same_host)),
     ?_assert(allowed(~"https://example.com:443", with_host(~"example.com"), same_host)),
     ?_assert(allowed(~"https://example.com:443", with_host(~"example.com:443"), same_host)),
     ?_assert(allowed(~"http://example.com:80", with_host(~"example.com", ~"http"), same_host)),
     ?_assert(allowed(~"https://[::1]", with_host(~"[::1]:443"), same_host)),
     ?_assertNot(allowed(~"http://example.com", with_host(~"example.com:443"), same_host)),
     ?_assertNot(allowed(~"https://example.com", with_host(~"example.com:80"), same_host)),
     ?_assertNot(allowed(~"https://example.com", with_host(~"example.com:8443"), same_host)),
     ?_assertNot(allowed(~"ws://example.com", with_host(~"example.com:80"), same_host)),
     ?_assertNot(allowed(~"https://evil.example", with_host(~"example.com:443"), same_host))].

same_host_without_case_test_() ->
    [?_assert(allowed(Origin, with_host(Host), same_host))
     || {Origin, Host} <- [{~"https://example.com", ~"Example.COM"},
                           {~"HTTPS://EXAMPLE.COM", ~"example.com"},
                           {~"https://Example.com", ~"EXAMPLE.com:443"},
                           {~"https://[2001:db8::A]", ~"[2001:DB8::a]"}]].

same_host_of_a_request_without_a_host_header_test_() ->
    [?_assert(allowed(~"https://example.com", to_authority(~"https", ~"example.com", 443), same_host)),
     ?_assert(allowed(~"https://example.com:8443", to_authority(~"https", ~"example.com", 8443), same_host)),
     ?_assert(allowed(~"https://[::1]:8443", to_authority(~"https", ~"[::1]", 8443), same_host)),
     ?_assertNot(allowed(~"https://example.com", to_authority(~"http", ~"example.com", 80), same_host)),
     ?_assertNot(allowed(~"https://example.com", to_authority(~"https", ~"example.com", 8443), same_host)),
     ?_assertNot(allowed(~"https://evil.example", to_authority(~"https", ~"example.com", 443), same_host))].

same_origin_test_() ->
    Req = to_authority(~"https", ~"example.com", 443),
    [[?_assert(allowed(Origin, Req, same_origin))
      || Origin <- [~"https://example.com", ~"https://example.com:443", ~"HTTPS://Example.COM"]],
     [?_assertEqual({Origin, false}, {Origin, allowed(Origin, Req, same_origin)})
      || Origin <- [~"http://example.com", ~"http://example.com:443", ~"https://example.com:8443",
                    ~"https://example.com.", ~"https://evil.example", ~"wss://example.com"]],
     ?_assert(allowed(~"https://[::1]:8443", to_authority(~"https", ~"[::1]", 8443), same_origin)),
     ?_assertNot(allowed(~"https://[::1]", to_authority(~"https", ~"[::1]", 8443), same_origin))].

%% What is not an origin is refused by every rule that looks at it, and
%% nothing a client sends makes one of them raise.
malformed_origin_test_() ->
    Origins = [~"", ~"null", ~"example.com", ~"https:", ~"https:/", ~"https://", ~"://", ~"://example.com",
               ~"https://:443", ~"https://example.com:", ~"https://example.com:x", ~"https://example.com:-1",
               ~"https://example.com:999999", ~"https://exa mple.com", ~"https://[", ~"https://[::1",
               ~"https://]", ~"https://[::1]x", ~"https://[::1]:", ~"https://[zz]", ~"https://%",
               ~"https://example.com://example.com", ~"https://\0example.com", <<255, 254>>,
               binary:copy(~"a", 70000)],
    Reqs = [with_host(~"example.com"), with_host(~""), to_authority(~"https", ~"example.com", 443),
            to_authority(~"https", ~"", 443)],
    [?_assertEqual({Origin, Allowed, false}, {Origin, Allowed, allowed(Origin, Req, Allowed)})
     || Origin <- Origins, Req <- Reqs, Allowed <- [same_host, same_origin, [~"https://example.com"], []]].

malformed_host_test_() ->
    Hosts = [~"", ~":443", ~"example.com:", ~"example.com:x", ~"example.com:999999", ~"exa mple.com", ~"[",
             ~"[::1", ~"[::1]x", ~"example.com/", ~"x@example.com", <<0>>, <<255>>],
    [?_assertEqual({Origin, Host, false}, {Origin, Host, allowed(Origin, with_host(Host), same_host)})
     || Host <- Hosts,
        Origin <- [~"https://example.com", <<"https://", Host/binary>>, ~"https://", ~"https://:443"]].

request_without_what_cowboy_gives_it_test_() ->
    [?_assertNot(allowed(~"https://example.com", #{headers => #{}}, Allowed)) || Allowed <- [same_host, same_origin]].

listed_test_() ->
    Req = with_host(~"example.com"),
    [?_assert(allowed(~"https://app.example.com", Req, [~"https://app.example.com"])),
     ?_assert(allowed(~"HTTPS://App.Example.com", Req, [~"https://other.example.com", ~"https://APP.example.com"])),
     ?_assertNot(allowed(~"https://app.example.com", Req, ["https://app.example.com"])),
     ?_assertNot(allowed(~"https://example.com", Req, [~"https://app.example.com"])),
     ?_assertNot(allowed(~"https://app.example.com:443", Req, [~"https://app.example.com"])),
     ?_assertNot(allowed(~"https://example.com", Req, []))].

%% The setting and the options of the plugins are checked before a request
%% arrives. A value that got past that refuses, and does not raise.
allowed_that_is_none_of_the_values_test_() ->
    Req = with_host(~"example.com"),
    [?_assertEqual({Allowed, false}, {Allowed, allowed(~"https://example.com", Req, Allowed)})
     || Allowed <- [same_hosts, ~"https://example.com", [a], [same_host], [1.5], [[a]], #{}, {same_host},
                    nova_test_helper:improper_list([~"https://other.example"], ~"https://another.example"), 443]] ++
        [?_assert(allowed(~"https://example.com", Req,
                          nova_test_helper:improper_list([a, ~"https://example.com"], b)))].
