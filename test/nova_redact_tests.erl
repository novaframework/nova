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

-define(SECRET, ~"session_id=do-not-log-me").

%%====================================================================
%% nova_redact
%%====================================================================

req() ->
    (nova_test_helper:mock_req(~"POST", ~"/login"))#{headers => #{~"cookie" => ?SECRET}}.

request_map_is_reduced_to_method_and_path_test() ->
    ?assertEqual(#{method => ~"POST", path => ~"/login", redacted => cowboy_req},
                 nova_redact:term(req())).

nested_request_is_redacted_test() ->
    Reason = {case_clause, {stop, req()}},
    ?assertEqual({case_clause, {stop, #{method => ~"POST", path => ~"/login", redacted => cowboy_req}}},
                 nova_redact:term(Reason)),
    ?assertEqual([a, #{req => #{method => ~"POST", path => ~"/login", redacted => cowboy_req}}],
                 nova_redact:term([a, #{req => req()}])).

other_terms_are_untouched_test() ->
    Term = {error, [1, 2], #{a => <<"b">>}, "str"},
    ?assertEqual(Term, nova_redact:term(Term)).

sensitive_keys_are_redacted_in_maps_and_proplists_test() ->
    Term = {error, #{~"Authorization" => ~"Bearer x", nested => #{password => ~"pw", user => ~"u"}},
            [{~"token", ~"t"}, {"Set-Cookie", ~"c"}, {name, ~"n"}]},
    ?assertEqual({error, #{~"Authorization" => redacted, nested => #{password => redacted, user => ~"u"}},
                  [{~"token", redacted}, {"Set-Cookie", redacted}, {name, ~"n"}]},
                 nova_redact:term(Term)).

partial_websocket_request_qs_is_redacted_test() ->
    PartialReq = #{method => ~"GET", path => ~"/ws", qs => ~"password=SECRETPW", peer => {{127, 0, 0, 1}, 1}},
    ?assertEqual(PartialReq#{qs => redacted}, nova_redact:term(PartialReq)).

key_containing_a_sensitive_fragment_is_redacted_test() ->
    ?assertEqual(#{~"access_token" => redacted, ~"password_confirmation" => redacted,
                   ~"client_secret" => redacted, ~"user" => d},
                 nova_redact:term(#{~"access_token" => a, ~"password_confirmation" => b,
                                    ~"client_secret" => c, ~"user" => d})),
    ?assertEqual([{refresh_token, redacted}, {id_token, redacted}, {~"new_password", redacted},
                  {"passwd", redacted}, {~"Proxy-Authorization", redacted}, {resp_cookies, redacted},
                  {credentials, redacted}, {nova_session_id, redacted}, {name, ~"n"}],
                 nova_redact:term([{refresh_token, t}, {id_token, t}, {~"new_password", p},
                                   {"passwd", p}, {~"Proxy-Authorization", a}, {resp_cookies, c},
                                   {credentials, c}, {nova_session_id, s}, {name, ~"n"}])).

key_is_compared_without_case_and_separators_test() ->
    ?assertEqual(#{~"X-Api-Key" => redacted, apiKey => redacted, "API_KEY" => redacted,
                   ~"Private-Key" => redacted, clientSecret => redacted, ~"api" => 1, key => 2},
                 nova_redact:term(#{~"X-Api-Key" => k, apiKey => k, "API_KEY" => k,
                                    ~"Private-Key" => k, clientSecret => k, ~"api" => 1, key => 2})).

whole_name_keys_do_not_match_as_fragments_test() ->
    ?assertEqual(#{otp => redacted, ~"QS" => redacted, otp_release => ~"29", ~"faqs" => 1},
                 nova_redact:term(#{otp => ~"123456", ~"QS" => ~"a=b", otp_release => ~"29", ~"faqs" => 1})).

tuple_outside_a_list_is_matched_on_the_whole_name_test() ->
    Untouched = [{error, {invalid_token, expired}},
                 {nova_session, unsupported_session_used},
                 {badmatch, {session_not_found, ~"id"}}],
    [?assertEqual(Term, nova_redact:term(Term)) || Term <- Untouched],
    ?assertEqual({badmatch, {token, redacted}}, nova_redact:term({badmatch, {token, ~"t"}})),
    ?assertEqual({password, redacted}, nova_redact:term({password, ~"pw"})),
    ?assertEqual({session_id, redacted}, nova_redact:term({session_id, ~"id"})),
    ?assertEqual({~"Set-Cookie", redacted}, nova_redact:term({~"Set-Cookie", ~"c"})),
    ?assertEqual({error, [{invalid_token, redacted}]}, nova_redact:term({error, [{invalid_token, ~"t"}]})).

only_the_first_64_bytes_of_a_key_are_compared_test() ->
    Padding = binary:copy(~"a", 64),
    Early = <<"token", Padding/binary>>,
    Late = <<Padding/binary, "token">>,
    ?assertEqual(#{Early => redacted, Late => 1, binary_to_list(Early) => redacted, binary_to_list(Late) => 2},
                 nova_redact:term(#{Early => 0, Late => 1, binary_to_list(Early) => 0, binary_to_list(Late) => 2})).

key_that_is_not_text_is_left_alone_test() ->
    Term = #{<<255, 254>> => 1, {token, a} => 2, 42 => 3, improper_list(token, improper) => 4, [{a, b}] => 5},
    ?assertEqual(Term, nova_redact:term(Term)),
    ?assertEqual(#{<<255, "token">> => redacted}, nova_redact:term(#{<<255, "token">> => 1})).

%% eqwalizer does not accept an improper list literal.
-spec improper_list(term(), dynamic()) -> dynamic().
improper_list(Head, Tail) ->
    [Head | Tail].

configured_redact_keys_are_added_test() ->
    application:set_env(nova, redact_keys, [card, ~"X-Tenant", "ssn"]),
    try
        ?assertEqual(#{~"credit_card_number" => redacted, ~"x-tenant" => redacted, ~"SSN" => redacted,
                       ~"API_KEY" => redacted, other => 1},
                     nova_redact:term(#{~"credit_card_number" => ~"4111", ~"x-tenant" => ~"t", ~"SSN" => ~"s",
                                        ~"API_KEY" => ~"k", other => 1})),
        ?assertEqual({card, redacted}, nova_redact:term({card, ~"4111"}))
    after
        application:unset_env(nova, redact_keys)
    end.

unusable_redact_keys_are_ignored_test() ->
    Term = #{user => ~"u", ~"" => 1, id => 2},
    [begin
         application:set_env(nova, redact_keys, Configured),
         try
             ?assertEqual(Term#{password => redacted}, nova_redact:term(Term#{password => ~"pw"}))
         after
             application:unset_env(nova, redact_keys)
         end
     end || Configured <- [[~"", '_', "-", 42, {a, b}], not_a_list, improper_list(improper, tail)]].
