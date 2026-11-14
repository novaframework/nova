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

format_arity_drops_arguments_test() ->
    ?assertEqual(2, nova_error_controller:format_arity([#{headers => #{}}, secret])).

format_stacktrace_hides_arguments_test() ->
    Entry = {my_mod, my_fun, [#{~"cookie" => ~"session_id=abc"}], [{file, "my_mod.erl"}, {line, 1}]},
    [Formatted] = nova_error_controller:format_stacktrace([Entry]),
    ?assertEqual(1, maps:get(arity, Formatted)).

%% nova_handler hands over frames already formatted by nova:format_stacktrace/1.
format_stacktrace_accepts_formatted_frames_test() ->
    Frames = nova:format_stacktrace([{my_mod, my_fun, [a, b], [{file, "my_mod.erl"}, {line, 7}]}]),
    ?assertEqual([#{module => ~"my_mod", function => ~"my_fun", arity => 2,
                    file => ~"my_mod.erl", line => 7}],
                 nova_error_controller:format_stacktrace(Frames)).

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

%% The message used to be encoded as a list of character codes.
not_found_json_message_is_a_string_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(~"GET", ~"/missing"),
         {status, 404, _Headers, Body} = nova_error_controller:not_found(Req),
         ?assertEqual(#{~"message" => ~"Resource not found"}, json:decode(iolist_to_binary(Body)))
     end}.

not_found_json_with_parameters_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:with_header(~"accept", ~"application/json; charset=utf-8",
                                            nova_test_helper:mock_req(~"GET", ~"/missing")),
         {status, 404, Headers, _Body} = nova_error_controller:not_found(Req),
         ?assertEqual(~"application/json", maps:get(~"content-type", Headers))
     end}.

%% nova_handler's crash payload has a status code but no title or message.
server_error_has_title_and_message_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:with_header(~"accept", ~"application/json",
                                            nova_test_helper:mock_req(~"GET", ~"/")),
         CrashInfo = #{status_code => 500, class => error, reason => boom},
         {status, 500, _Headers, Body} = nova_error_controller:server_error(Req#{crash_info => CrashInfo}),
         #{~"title" := Title, ~"status" := Status, ~"message" := Message} =
             json:decode(iolist_to_binary(Body)),
         ?assertEqual(~"500 Internal Server Error", Title),
         ?assertEqual(~"Internal Server Error", Status),
         ?assertNotEqual(~"undefined", Message)
     end}.

server_error_html_for_browsers_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Accept = ~"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
         Req = nova_test_helper:with_header(~"accept", Accept, nova_test_helper:mock_req(~"GET", ~"/")),
         CrashInfo = #{status_code => 500, class => error, reason => boom},
         {status, 500, Headers, Body} = nova_error_controller:server_error(Req#{crash_info => CrashInfo}),
         ?assertEqual(~"text/html", maps:get(~"content-type", Headers)),
         ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Body), ~"500 Internal Server Error"))
     end}.

%%====================================================================
%% Crash details are escaped and only shown in dev
%%====================================================================

%% The payload nova_handler builds when a controller returns
%% {error, #{x => <<"<script>x</script>">>}} and there is no fallback.
unsupported_return_crash_info() ->
    Response = {error, #{x => ~"<script>x</script>"}},
    #{status_code => 500,
      status => ~"Problems with controller",
      title => ~"Controller returned unsupported data",
      extra_msg => list_to_binary(io_lib:format("Controller returned unsupported data: ~p", [Response]))}.

browser_req() ->
    nova_test_helper:with_header(~"accept", ~"text/html,application/xhtml+xml;q=0.9",
                                 nova_test_helper:mock_req(~"GET", ~"/")).

json_req() ->
    nova_test_helper:with_header(~"accept", ~"application/json", nova_test_helper:mock_req(~"GET", ~"/")).

extra_msg_is_escaped_in_dev_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = (browser_req())#{crash_info => unsupported_return_crash_info()},
         {status, 500, _Headers, Body0} = nova_error_controller:server_error(Req),
         Body = iolist_to_binary(Body0),
         ?assertEqual(nomatch, binary:match(Body, ~"<script>x")),
         ?assertNotEqual(nomatch, binary:match(Body, ~"&lt;script&gt;x"))
     end}.

extra_msg_is_hidden_outside_dev_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         application:set_env(nova, environment, prod),
         try
             CrashInfo = unsupported_return_crash_info(),
             {status, 500, _, Html} =
                 nova_error_controller:server_error((browser_req())#{crash_info => CrashInfo}),
             ?assertEqual(nomatch, binary:match(iolist_to_binary(Html), ~"script&gt;")),
             {status, 500, _, Json} =
                 nova_error_controller:server_error((json_req())#{crash_info => CrashInfo}),
             Decoded = json:decode(iolist_to_binary(Json)),
             ?assertMatch(#{~"title" := _}, Decoded),
             ?assertNot(is_map_key(~"extra_msg", Decoded)),
             ?assertNot(is_map_key(~"stacktrace", Decoded))
         after
             application:set_env(nova, environment, dev)
         end
     end}.

%% The dev 500 for a crash used to encode its texts as charlists, which JSON
%% turned into arrays of numbers.
crash_details_are_strings_in_dev_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         CrashInfo = #{class => error, reason => boom, stacktrace => []},
         {status, 500, _, Json} = nova_error_controller:server_error((json_req())#{crash_info => CrashInfo}),
         #{~"status" := Status, ~"extra_msg" := ExtraMsg} = json:decode(iolist_to_binary(Json)),
         ?assertEqual(~"Internal Server Error", Status),
         ?assertEqual(~"Class: error\nReason: boom", ExtraMsg)
     end}.

%% nova:get_environment/0 reads an unset environment as dev, but crash details
%% must not reach clients unless dev was chosen explicitly.
extra_msg_is_hidden_when_environment_is_unset_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         application:unset_env(nova, environment),
         try
             CrashInfo = unsupported_return_crash_info(),
             {status, 500, _, Json} =
                 nova_error_controller:server_error((json_req())#{crash_info => CrashInfo}),
             #{} = Decoded = json:decode(iolist_to_binary(Json)),
             ?assertNot(is_map_key(~"extra_msg", Decoded)),
             ?assertNot(is_map_key(~"stacktrace", Decoded)),
             ?assertEqual({status, 500},
                          nova_error_controller:server_error(
                            (json_req())#{crash_info => #{class => error, reason => secret}}))
         after
             application:set_env(nova, environment, dev)
         end
     end}.

accept_media_types_are_case_insensitive_test() ->
    ?assertEqual(html, nova_error_controller:preferred_type(~" Text/HTML ;q=0.9")).

%% Cowboy joins repeated accept headers; parsing must stay linear.
huge_accept_header_is_cheap_test() ->
    Accept = binary:copy(~",", 400000),
    {Micros, none} = timer:tc(fun() -> nova_error_controller:preferred_type(Accept) end),
    ?assert(Micros < 100000).

media_range_of_only_whitespace_is_empty_test() ->
    ?assertEqual(<<>>, nova_error_controller:trim_ows(~" \t ")),
    ?assertEqual(none, nova_error_controller:preferred_type(~" , \t")).
