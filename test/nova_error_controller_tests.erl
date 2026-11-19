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
         ?assertMatch(#{~"message" := _}, json:decode(iolist_to_binary(Body)))
     end}.

%% When no accept header, defaults to JSON
not_found_no_accept_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"GET">>, <<"/missing">>),
         {status, 404, Headers, _Body} = nova_error_controller:not_found(Req),
         ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers))
     end}.

server_error_test_() ->
    Req = (nova_test_helper:mock_req(~"GET", ~"/crash"))#{crash_info => #{class => error, reason => badarg}},
    {foreach,
     fun() -> application:get_env(nova, environment) end,
     fun({ok, Environment}) -> application:set_env(nova, environment, Environment);
        (undefined) -> application:unset_env(nova, environment)
     end,
     [fun() ->
              application:unset_env(nova, environment),
              ?assertEqual({status, 500}, nova_error_controller:server_error(Req))
      end,
      fun() ->
              application:set_env(nova, environment, dev),
              ?assertMatch({status, 500, #{}, _Body}, nova_error_controller:server_error(Req))
      end]}.

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

%%====================================================================
%% Crash details are escaped and only shown in dev
%%====================================================================

browser_req() ->
    nova_test_helper:with_header(~"accept", ~"text/html,application/xhtml+xml;q=0.9",
                                 nova_test_helper:mock_req(~"GET", ~"/")).

json_req() ->
    nova_test_helper:with_header(~"accept", ~"application/json", nova_test_helper:mock_req(~"GET", ~"/")).

%% render_error_pages turns the pages of Nova off for every client, whatever
%% form it asks for.
no_page_without_error_pages_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         application:set_env(nova, render_error_pages, false),
         try
             CrashInfo = #{status_code => 500, class => error, reason => boom},
             Png = nova_test_helper:with_header(~"accept", ~"image/png", nova_test_helper:mock_req(~"GET", ~"/")),
             [begin
                  ?assertEqual({status, 500}, nova_error_controller:server_error(Req#{crash_info => CrashInfo})),
                  ?assertEqual({status, 404}, nova_error_controller:not_found(Req))
              end || Req <- [json_req(), nova_test_helper:mock_req(~"GET", ~"/"), browser_req(), Png]]
         after
             application:unset_env(nova, render_error_pages)
         end
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
