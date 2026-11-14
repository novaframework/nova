-module(nova_error_controller_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun nova_test_helper:setup_nova_env/0).
-define(CLEANUP, fun nova_test_helper:cleanup_nova_env/1).

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
    persistent_term:erase({nova_error_controller, stacktrace_hint}),
    {Frames, Events} = nova_test_log:collect(fun() -> nova_error_controller:format_stacktrace(not_enabled) end),
    ?assertEqual([], Frames),
    [{warning, {report, #{msg := <<Message/binary>>}}}] = Events,
    ?assertNotEqual(nomatch, binary:match(Message, ~"nova:use_stacktrace(true)")),
    ?assertNotEqual(nomatch, binary:match(Message, ~"{use_stacktrace, true}")).

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

%% A thrown {500, Reason} leaves the reason as crash_info, which need not be a
%% map, and a 500 page can be asked for with no crash_info at all.
server_error_without_a_crash_map_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:with_header(~"accept", ~"application/json",
                                            nova_test_helper:mock_req(~"GET", ~"/")),
         [begin
              {status, 500, _Headers, Body} = nova_error_controller:server_error(Req0),
              ?assertMatch(#{~"title" := ~"500 Internal Server Error"}, json:decode(iolist_to_binary(Body)))
          end || Req0 <- [Req, Req#{crash_info => not_a_map}]]
     end}.

%% Without an accept header the 500 page is JSON, as the 404 page is.
server_error_no_accept_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(~"GET", ~"/"),
         [begin
              {status, 500, Headers, Body} = nova_error_controller:server_error(Req0),
              ?assertEqual(~"application/json", maps:get(~"content-type", Headers)),
              ?assertMatch(#{~"title" := ~"500 Internal Server Error"}, json:decode(iolist_to_binary(Body)))
          end || Req0 <- [Req, Req#{crash_info => #{status_code => 500, class => error, reason => boom}}]]
     end}.

%% An accept header that asks for neither JSON nor HTML gets no body, on
%% both pages.
unknown_accept_has_no_body_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:with_header(~"accept", ~"image/png", nova_test_helper:mock_req(~"GET", ~"/")),
         ?assertMatch({status, 404, _, <<>>}, nova_error_controller:not_found(Req)),
         ?assertMatch({status, 500, _, <<>>}, nova_error_controller:server_error(Req))
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
             {status, 500, _, Crash} =
                 nova_error_controller:server_error((json_req())#{crash_info => #{class => error, reason => secret}}),
             ?assertEqual(nomatch, binary:match(iolist_to_binary(Crash), ~"secret"))
         after
             application:set_env(nova, environment, dev)
         end
     end}.

%% A crash in a plugin leaves a class and a reason and no status code.
%% Outside dev its page is the generic one, in the form the client accepts.
crash_page_outside_dev_follows_the_accept_header_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         application:set_env(nova, environment, prod),
         try
             CrashInfo = #{class => error, reason => secret, stacktrace => [{secret_mod, secret_fun, 1, []}]},
             {status, 500, JsonHeaders, Json} =
                 nova_error_controller:server_error((json_req())#{crash_info => CrashInfo}),
             ?assertEqual(~"application/json", maps:get(~"content-type", JsonHeaders)),
             ?assertEqual(#{~"status" => ~"Internal Server Error",
                            ~"title" => ~"500 Internal Server Error",
                            ~"message" => ~"Something internal crashed. Please take a look!"},
                          json:decode(iolist_to_binary(Json))),
             {status, 500, NoAcceptHeaders, _} =
                 nova_error_controller:server_error((nova_test_helper:mock_req(~"GET", ~"/"))#{crash_info => CrashInfo}),
             ?assertEqual(~"application/json", maps:get(~"content-type", NoAcceptHeaders)),
             {status, 500, HtmlHeaders, Html} =
                 nova_error_controller:server_error((browser_req())#{crash_info => CrashInfo}),
             ?assertEqual(~"text/html", maps:get(~"content-type", HtmlHeaders)),
             ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Html), ~"500 Internal Server Error")),
             ?assertEqual(nomatch, binary:match(iolist_to_binary(Html), ~"secret")),
             Png = nova_test_helper:with_header(~"accept", ~"image/png", nova_test_helper:mock_req(~"GET", ~"/")),
             ?assertMatch({status, 500, _, <<>>}, nova_error_controller:server_error(Png#{crash_info => CrashInfo}))
         after
             application:set_env(nova, environment, dev)
         end
     end}.

%% render_error_pages turns the HTML page off. A client that asks for JSON,
%% or for nothing in particular, still gets the JSON body.
server_error_without_error_pages_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         application:set_env(nova, render_error_pages, false),
         try
             CrashInfo = #{status_code => 500, class => error, reason => boom},
             [begin
                  {status, 500, Headers, Body} = nova_error_controller:server_error(Req#{crash_info => CrashInfo}),
                  ?assertEqual(~"application/json", maps:get(~"content-type", Headers)),
                  ?assertMatch(#{~"title" := ~"500 Internal Server Error"}, json:decode(iolist_to_binary(Body)))
              end || Req <- [json_req(), nova_test_helper:mock_req(~"GET", ~"/")]],
             ?assertEqual({status, 500}, nova_error_controller:server_error((browser_req())#{crash_info => CrashInfo}))
         after
             application:unset_env(nova, render_error_pages)
         end
     end}.

%% nova_handler takes the stacktrace of a crash only when stacktraces are
%% on. In dev the page of a crash without one says how to turn them on, the
%% first time.
crash_without_a_stacktrace_names_the_setting_in_dev_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Warnings = fun(CrashInfo) ->
                            {_Page, Events} =
                                nova_test_log:collect(
                                  fun() -> nova_error_controller:server_error((json_req())#{crash_info => CrashInfo}) end),
                            [Message || {warning, {report, #{msg := Message}}} <- Events]
                    end,
         Crash = #{status_code => 500, class => error, reason => boom},
         persistent_term:erase({nova_error_controller, stacktrace_hint}),
         ?assertEqual([], Warnings(Crash#{stacktrace => []})),
         ?assertEqual([], Warnings(#{status_code => 500})),
         application:set_env(nova, environment, prod),
         try
             ?assertEqual([], Warnings(Crash))
         after
             application:set_env(nova, environment, dev)
         end,
         [<<Message/binary>>] = Warnings(Crash),
         ?assertNotEqual(nomatch, binary:match(Message, ~"nova:use_stacktrace(true)")),
         ?assertEqual([], Warnings(Crash))
     end}.

%% The pages are negotiated as RFC 9110 12.5.1 has it: the most specific
%% range that covers a type gives its quality, the type with the higher
%% quality wins, and of two the client likes the same the one it names first.
%% JSON wins when one range covers both.
accept_header_chooses_the_body_test_() ->
    Chosen = [{undefined, json},
              {~"*/*", json},
              {~"text/html", html},
              {~"application/json", json},
              {~"application/json; charset=utf-8", json},
              {~"text/*", html},
              {~"application/*", json},
              {~"application/*;q=0.5, text/html", html},
              {~"text/html;q=0, */*", json},
              {~"application/json;q=0, */*", html},
              {~"text/html, application/json", html},
              {~"application/json, text/html", json},
              {~"text/html;q=0.5, application/json;q=0.6", json},
              {~"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", html},
              {~" Text/HTML ;q=0.9", html},
              {~"image/png", none},
              {~"*/*;q=0", none},
              {~"text/html;q=0", none},
              {~" , \t", none},
              {~"text/html;q=abc", json},
              {~"/", json}],
    [{iolist_to_binary(io_lib:format("~p", [Accept])),
      ?_assertEqual(Type, nova_error_controller:response_type(accepting(Accept)))}
     || {Accept, Type} <- Chosen].

accept_header_chooses_the_body_of_both_pages_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = accepting(~"*/*"),
         {status, 404, NotFound, _} = nova_error_controller:not_found(Req),
         {status, 500, ServerError, _} = nova_error_controller:server_error(Req),
         ?assertEqual(~"application/json", maps:get(~"content-type", NotFound)),
         ?assertEqual(~"application/json", maps:get(~"content-type", ServerError))
     end}.

%% Cowboy joins repeated accept headers, up to some hundred kilobytes, so
%% reading them must stay linear.
huge_accept_header_is_cheap_test_() ->
    [?_test(begin
                {Micros, Type} = timer:tc(fun() -> nova_error_controller:response_type(accepting(Accept)) end),
                ?assertEqual(Expected, Type),
                ?assert(Micros < Limit)
            end)
     || {Accept, Expected, Limit} <- [{binary:copy(~",", 400000), none, 100000},
                                      {binary:copy(~"image/png;q=0.5, ", 25000), none, 1000000},
                                      {<<(binary:copy(~"image/png;q=0.5, ", 25000))/binary, "text/html">>, html,
                                       1000000}]].

accepting(undefined) ->
    nova_test_helper:mock_req(~"GET", ~"/");
accepting(Accept) ->
    nova_test_helper:with_header(~"accept", Accept, nova_test_helper:mock_req(~"GET", ~"/")).
