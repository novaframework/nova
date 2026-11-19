-module(nova_redact_tests).

-include_lib("eunit/include/eunit.hrl").

proplist_test() ->
    ?assertEqual([{certfile, "cert.pem"}, {key, redacted}, {password, redacted}],
                 nova_redact:tls_options([{certfile, "cert.pem"}, {key, {rsa, ~"der"}}, {password, "secret"}])).

map_test() ->
    ?assertEqual(#{port => 8443, socket_opts => #{password => redacted, certs_keys => redacted}},
                 nova_redact:tls_options(#{port => 8443,
                                           socket_opts => #{password => "secret", certs_keys => [#{key => ~"der"}]}})).

nested_test() ->
    Reason = {shutdown, {failed_to_start_child, ranch_acceptors_sup,
                         {child, undefined, ref, {ranch_listener_sup, start_link,
                                                  [ref, ranch_ssl,
                                                   #{socket_opts => [{port, 0}, {key, {rsa, ~"a private key"}}]}]}}}},
    Redacted = nova_redact:tls_options(Reason),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~0p", [Redacted])), ~"a private key")),
    ?assertMatch({shutdown, {failed_to_start_child, ranch_acceptors_sup, {child, undefined, ref, _}}}, Redacted).

improper_test() ->
    ?assertEqual([{certfile, "cert.pem"}, {key, redacted} | {password, redacted}],
                 nova_redact:tls_options([{certfile, "cert.pem"}, {key, {rsa, ~"der"}} | {password, "secret"}])).

untouched_test() ->
    ?assertEqual([eaddrinuse, 1, ~"key", [a | b]], nova_redact:tls_options([eaddrinuse, 1, ~"key", [a | b]])).

stacktrace_test() ->
    Location = [{file, "nova_sup.erl"}, {line, 1}],
    ?assertEqual([{cowboy, start_tls, 3, []}, {nova_sup, bind_tls, 5, Location}],
                 nova_redact:stacktrace([{cowboy, start_tls, [ref, [{password, "secret"}], #{}], []},
                                         {nova_sup, bind_tls, 5, Location}])).
