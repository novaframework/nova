-module(nova_redact_tests).
-include_lib("eunit/include/eunit.hrl").

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
