-module(nova_redact_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

%% Logger handler callback: forwards every event to the test process.
-export([log/2]).

log(#{msg := Msg}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Msg},
    ok.

-define(SECRET, ~"session_id=do-not-log-me").

%%====================================================================
%% nova_redact
%%====================================================================

req() ->
    (nova_test_helper:mock_req(~"POST", ~"/login"))#{headers => #{~"cookie" => ?SECRET}}.

request_map_is_reduced_to_method_and_path_test() ->
    ?assertEqual(#{method => ~"POST", path => ~"/login", redacted => nova_req},
                 nova_redact:term(req())).

nested_request_is_redacted_test() ->
    Reason = {case_clause, {stop, req()}},
    ?assertEqual({case_clause, {stop, #{method => ~"POST", path => ~"/login", redacted => nova_req}}},
                 nova_redact:term(Reason)),
    ?assertEqual([a, #{req => #{method => ~"POST", path => ~"/login", redacted => nova_req}}],
                 nova_redact:term([a, #{req => req()}])).

other_terms_are_untouched_test() ->
    Term = {error, [1, 2], #{a => <<"b">>}, "str"},
    ?assertEqual(Term, nova_redact:term(Term)).

stacktrace_arguments_are_dropped_test() ->
    Stacktrace = [{my_controller, index, [req()], [{file, "x.erl"}, {line, 3}]},
                  {nova_handler, execute, 2, []}],
    ?assertEqual([{my_controller, index, 1, [{file, "x.erl"}, {line, 3}]},
                  {nova_handler, execute, 2, []}],
                 nova_redact:stacktrace(Stacktrace)).

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

format_stacktrace_drops_arguments_test() ->
    [Frame] = nova:format_stacktrace([{my_controller, index, [req()], []}]),
    ?assertEqual(1, maps:get(arity, Frame)).

%%====================================================================
%% Crash logs written by nova_handler
%%====================================================================

controller_crash_log_has_no_request_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             capture_logs(),
             Callback = fun(#{this_key_is_missing := _}) -> ok end,
             Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
             {ok, Req1, _Env} = nova_handler:execute(req(), Env),
             ?assertEqual(500, maps:get(resp_status_code, Req1)),
             assert_not_leaked(maps:get(crash_info, Req1)),
             assert_logged_without_request()
     end}.

badmatch_on_request_log_has_no_request_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             capture_logs(),
             Callback = fun(Req) -> ok = Req end,
             Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
             {ok, Req1, _Env} = nova_handler:execute(req(), Env),
             assert_not_leaked(maps:get(crash_info, Req1)),
             assert_logged_without_request()
     end}.

query_string_behind_a_badmatch_is_not_logged_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             capture_logs(),
             Callback = fun(Req) -> [] = nova_req:parse_qs(Req) end,
             Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
             Req0 = (nova_test_helper:mock_req(~"GET", ~"/login"))#{qs => ~"password=SECRETPW"},
             {ok, Req1, _Env} = nova_handler:execute(Req0, Env),
             Logs = logged(),
             ?assertNotEqual([], Logs),
             Text = iolist_to_binary(io_lib:format("~p", [{Logs, maps:get(crash_info, Req1)}])),
             ?assertEqual(nomatch, binary:match(Text, ~"SECRETPW"))
     end}.

unformattable_frame_is_logged_without_request_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             capture_logs(),
             ?assertEqual([], nova_error_controller:format_stacktrace([{not_a_frame, req()}])),
             Logs = logged(),
             ?assertMatch([{report, #{msg := ~"Could not format stacktrace line"}}], Logs),
             assert_not_leaked(Logs)
     end}.

dev_crash_page_shows_frames_and_log_has_no_cookie_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             application:set_env(nova, render_error_pages, true),
             ServerError = #nova_handler_value{app = nova, callback = fun nova_error_controller:server_error/1},
             {ok, Dispatch} = nova_routing_trie:insert('_', 500, '_', ServerError, nova_routing_trie:new(#{})),
             persistent_term:put(nova_dispatch, Dispatch),
             capture_logs(),
             Callback = fun(#{this_key_is_missing := _}) -> ok end,
             Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
             Req0 = nova_test_helper:with_header(~"accept", ~"application/json", req()),
             {ok, Req1, _Env} = nova_handler:execute(Req0, Env),
             ?assertEqual(500, maps:get(resp_status_code, Req1)),
             {Body, Modules} = error_page(Req1),
             ?assert(lists:member(~"nova_redact_tests", Modules)),
             assert_not_leaked(Body),
             assert_logged_without_request()
     end}.

%% The JSON error page and the module of each stacktrace frame on it.
error_page(#{resp_body := Body}) when is_binary(Body); is_list(Body) ->
    Page = iolist_to_binary(Body),
    case json:decode(Page) of
        #{~"stacktrace" := Frames} when is_list(Frames) ->
            {Page, [Module || #{~"module" := Module} <- Frames]}
    end.

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, render_error_pages, false),
    persistent_term:put(nova_dispatch, nova_routing_trie:new(#{})),
    persistent_term:put(nova_use_stacktrace, true),
    {ok, Pid} = nova_handlers:start_link(),
    meck:new(nova_req, [passthrough, no_link]),
    meck:expect(nova_req, reply, fun nova_test_helper:reply/2),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    logger:remove_handler(nova_redact_tests),
    meck:unload(nova_req),
    gen_server:stop(Pid),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_use_stacktrace),
    application:unset_env(nova, render_error_pages),
    nova_test_helper:cleanup_nova_env(Prev).

%% eunit runs the test body in another process than setup/0.
capture_logs() ->
    ok = logger:add_handler(nova_redact_tests, ?MODULE, #{config => #{test_pid => self()}}).

assert_logged_without_request() ->
    Logs = logged(),
    ?assertNotEqual([], Logs),
    assert_not_leaked(Logs).

logged() ->
    receive
        {logged, Msg} -> [Msg | logged()]
    after 0 ->
            []
    end.

assert_not_leaked(Term) ->
    Text = iolist_to_binary(io_lib:format("~p", [Term])),
    ?assertEqual(nomatch, binary:match(Text, ~"do-not-log-me")).
