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

configured_redact_keys_are_added_test() ->
    application:set_env(nova, redact_keys, [api_key, ~"X-Api-Key"]),
    try
        ?assertEqual(#{~"x-api-key" => redacted, ~"API_KEY" => redacted, other => 1},
                     nova_redact:term(#{~"x-api-key" => ~"k", ~"API_KEY" => ~"k", other => 1}))
    after
        application:unset_env(nova, redact_keys)
    end.

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
             Callback = fun(Req) -> [] = cowboy_req:parse_qs(Req) end,
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
             Body = iolist_to_binary(maps:get(resp_body, Req1)),
             #{~"stacktrace" := Frames} = json:decode(Body),
             ?assert(lists:member(~"nova_redact_tests", [Module || #{~"module" := Module} <- Frames])),
             assert_not_leaked(Body),
             assert_logged_without_request()
     end}.

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, render_error_pages, false),
    persistent_term:put(nova_dispatch, nova_routing_trie:new(#{})),
    persistent_term:put(nova_use_stacktrace, true),
    {ok, Pid} = nova_handlers:start_link(),
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, reply, fun(Status, Req) -> Req#{resp_status_code => Status} end),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    logger:remove_handler(nova_redact_tests),
    meck:unload(cowboy_req),
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
