%%% Input a client got wrong is answered with a 4xx and logged as a warning,
%%% not with a 500 and a crash report.
-module(nova_request_error_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([malformed_query_string_is_a_400/1,
         query_string_with_too_many_keys_is_a_400/1,
         form_with_too_many_fields_is_a_413/1,
         malformed_form_is_a_400/1,
         well_formed_input_reaches_the_controller/1,
         parse_qs_false_leaves_the_query_string_alone/1,
         request_error_in_a_controller_is_a_400/1,
         body_over_the_length_a_controller_reads_is_a_413/1,
         body_a_controller_waits_too_long_for_is_a_408/1,
         request_error_in_a_plugin_is_a_400/1,
         request_error_in_a_plugin_passes_when_the_input_is_right/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PAGES, nova_request_error_pages).
-define(FORM, {"content-type", "application/x-www-form-urlencoded"}).

all() ->
    [malformed_query_string_is_a_400,
     query_string_with_too_many_keys_is_a_400,
     form_with_too_many_fields_is_a_413,
     malformed_form_is_a_400,
     well_formed_input_reaches_the_controller,
     parse_qs_false_leaves_the_query_string_alone,
     request_error_in_a_controller_is_a_400,
     body_over_the_length_a_controller_reads_is_a_413,
     body_a_controller_waits_too_long_for_is_a_408,
     request_error_in_a_plugin_is_a_400,
     request_error_in_a_plugin_passes_when_the_input_is_right].

init_per_suite(Config) ->
    Routes = [#{prefix => "/parsed",
                plugins => [{pre_request, nova_request_plugin, #{parse_qs => true, read_urlencoded_body => true}}],
                routes => [{"/echo", fun ?PAGES:echo/1, #{methods => [get, post]}}]},
              #{prefix => "/unparsed",
                plugins => [{pre_request, nova_request_plugin, #{parse_qs => false}}],
                routes => [{"/echo", fun ?PAGES:echo/1, #{methods => [get]}}]},
              #{prefix => "/controller",
                plugins => [],
                routes => [{"/qs", fun ?PAGES:parse_qs/1, #{methods => [get]}},
                           {"/short-body", fun ?PAGES:short_body/1, #{methods => [post]}},
                           {"/slow-body", fun ?PAGES:slow_body/1, #{methods => [post]}}]},
              #{prefix => "/plugin",
                plugins => [{pre_request, ?PAGES, #{}}],
                routes => [{"/echo", fun ?PAGES:echo/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

malformed_query_string_is_a_400(Config) ->
    ?assertEqual({400, <<>>}, quietly(fun() -> get(Config, "/parsed/echo?a=%zz") end)).

query_string_with_too_many_keys_is_a_400(Config) ->
    ?assertEqual({400, <<>>}, quietly(fun() -> get(Config, ["/parsed/echo?", fields(101)]) end)).

form_with_too_many_fields_is_a_413(Config) ->
    ?assertEqual({413, <<>>}, quietly(fun() -> post(Config, "/parsed/echo", fields(101)) end)).

malformed_form_is_a_400(Config) ->
    ?assertEqual({400, <<>>}, quietly(fun() -> post(Config, "/parsed/echo", "a=%zz") end)).

well_formed_input_reaches_the_controller(Config) ->
    {200, Body} = post(Config, "/parsed/echo?page=2", fields(100)),
    #{~"parsed_qs" := #{~"page" := ~"2"}, ~"params" := Params} = json:decode(Body),
    ?assertEqual(100, map_size(Params)).

parse_qs_false_leaves_the_query_string_alone(Config) ->
    {{200, Body}, Events} = nova_test_log:collect(fun() -> get(Config, "/unparsed/echo?a=%zz") end),
    ?assertMatch(#{~"parsed_qs" := null}, json:decode(Body)),
    ?assertEqual([], nova_test_log:errors(Events)).

request_error_in_a_controller_is_a_400(Config) ->
    ?assertEqual({400, <<>>}, quietly(fun() -> get(Config, "/controller/qs?a=%zz") end)),
    ?assertMatch({200, _}, get(Config, "/controller/qs?a=b")).

body_over_the_length_a_controller_reads_is_a_413(Config) ->
    ?assertMatch(<<"HTTP/1.1 413", _/binary>>,
                 quietly(fun() -> unfinished_post(Config, "/controller/short-body", "name=a+longer+value") end)),
    ?assertMatch({200, _}, post(Config, "/controller/short-body", "a=b")).

body_a_controller_waits_too_long_for_is_a_408(Config) ->
    ?assertMatch(<<"HTTP/1.1 408", _/binary>>,
                 quietly(fun() -> unfinished_post(Config, "/controller/slow-body", "a=b") end)),
    ?assertMatch({200, _}, post(Config, "/controller/slow-body", "a=b")).

request_error_in_a_plugin_is_a_400(Config) ->
    ?assertEqual({400, <<>>}, quietly(fun() -> get(Config, "/plugin/echo?id=seven") end)),
    ?assertEqual({400, <<>>}, quietly(fun() -> get(Config, "/plugin/echo") end)).

request_error_in_a_plugin_passes_when_the_input_is_right(Config) ->
    ?assertMatch({200, _}, get(Config, "/plugin/echo?id=7")).

get(Config, Path) ->
    nova_test_listener:get(?config(port, Config), Path).

post(Config, Path, Body) ->
    {Status, _Headers, RespBody} = nova_test_listener:response(?config(port, Config), "POST", Path, [?FORM], Body),
    {Status, RespBody}.

%% Announces a longer body than it sends and waits for the answer.
unfinished_post(Config, Path, Sent) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config), [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, ["POST ", Path, " HTTP/1.1\r\n",
                                   "host: localhost\r\n",
                                   "content-type: application/x-www-form-urlencoded\r\n",
                                   "content-length: 1000\r\n\r\n",
                                   Sent]),
        {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
        Response
    after
        gen_tcp:close(Socket)
    end.

fields(Count) ->
    lists:join("&", [["f", integer_to_list(N), "=v"] || N <- lists:seq(1, Count)]).

%% Runs a request that must be logged as a warning and nothing worse.
quietly(Request) ->
    {Response, Events} = nova_test_log:collect(Request),
    ?assertEqual([], nova_test_log:errors(Events)),
    ?assertMatch([_ | _], [Report || {warning, {report, Report}} <- Events]),
    Response.
