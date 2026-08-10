-module(nova_error_controller_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> nova_test_helper:setup_nova_env() end).
-define(CLEANUP, fun(Prev) -> nova_test_helper:cleanup_nova_env(Prev) end).

%%====================================================================
%% status_code/1
%%====================================================================

status_code_from_req_test() ->
    Req = #{resp_status_code => 404},
    ?assertEqual({status, 404}, nova_error_controller:status_code(Req)).

status_code_default_test() ->
    ?assertEqual({status, 200}, nova_error_controller:status_code(#{})).

%%====================================================================
%% format_stacktrace/1
%%====================================================================

format_stacktrace_empty_test() ->
    ?assertEqual([], nova_error_controller:format_stacktrace([])).

format_stacktrace_single_entry_test() ->
    Entry = {my_mod, my_fun, 2, [{file, "my_mod.erl"}, {line, 42}]},
    [Formatted] = nova_error_controller:format_stacktrace([Entry]),
    ?assertEqual(<<"my_mod">>, maps:get(module, Formatted)),
    ?assertEqual(<<"my_fun">>, maps:get(function, Formatted)),
    ?assertEqual(<<"my_mod.erl">>, maps:get(file, Formatted)),
    ?assertEqual(42, maps:get(line, Formatted)).

format_stacktrace_no_file_line_test() ->
    Entry = {some_mod, some_fun, 1, []},
    [Formatted] = nova_error_controller:format_stacktrace([Entry]),
    ?assertEqual(undefined, maps:get(file, Formatted)),
    ?assertEqual(undefined, maps:get(line, Formatted)).

format_stacktrace_multiple_entries_test() ->
    Entries = [
        {mod_a, fun_a, 1, [{file, "a.erl"}, {line, 10}]},
        {mod_b, fun_b, 0, [{file, "b.erl"}, {line, 20}]}
    ],
    Result = nova_error_controller:format_stacktrace(Entries),
    ?assertEqual(2, length(Result)).

format_stacktrace_not_enabled_test() ->
    ?assertEqual([], nova_error_controller:format_stacktrace(not_enabled)).

%%====================================================================
%% format_arity/1
%%====================================================================

format_arity_integer_test() ->
    ?assertEqual(3, nova_error_controller:format_arity(3)).

format_arity_function_test() ->
    F = fun() -> ok end,
    ?assertEqual(<<"fun">>, nova_error_controller:format_arity(F)).

%%====================================================================
%% not_found/1
%%====================================================================

not_found_json_accept_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"GET">>, <<"/missing">>),
         Req1 = nova_test_helper:with_header(<<"accept">>, <<"application/json">>, Req),
         {status, 404, Headers, Body} = nova_error_controller:not_found(Req1),
         ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)),
         ?assert(is_binary(Body))
     end}.

%% When no accept header, defaults to JSON
not_found_no_accept_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"GET">>, <<"/missing">>),
         {status, 404, Headers, _Body} = nova_error_controller:not_found(Req),
         ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers))
     end}.
