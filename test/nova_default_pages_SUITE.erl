%%% The 404 and the 500 page Nova answers with when the application has none
%%% of its own: generic outside dev, and in the form the client accepts.
-module(nova_default_pages_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
         plugin_crash_is_json_for_a_client_that_accepts_json/1,
         plugin_crash_is_json_without_an_accept_header/1,
         plugin_crash_is_html_for_a_browser/1,
         crash_is_json_with_error_pages_off/1,
         crash_has_no_html_page_with_error_pages_off/1,
         any_type_is_answered_with_json/1,
         unreadable_accept_header_is_answered_with_json/1,
         excluded_json_is_answered_with_html/1,
         neither_json_nor_html_has_no_body/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PAGES, nova_default_pages_pages).
-define(BROWSER, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8").

all() ->
    [
     plugin_crash_is_json_for_a_client_that_accepts_json,
     plugin_crash_is_json_without_an_accept_header,
     plugin_crash_is_html_for_a_browser,
     crash_is_json_with_error_pages_off,
     crash_has_no_html_page_with_error_pages_off,
     any_type_is_answered_with_json,
     unreadable_accept_header_is_answered_with_json,
     excluded_json_is_answered_with_html,
     neither_json_nor_html_has_no_body
    ].

init_per_suite(Config) ->
    Host = [#{prefix => "/plugin",
              plugins => [{pre_request, ?PAGES, #{}}],
              routes => [{"/page", fun ?PAGES:page/1, #{methods => [get]}}]},
            #{prefix => "/guarded",
              plugins => [],
              security => fun ?PAGES:refuse/1,
              routes => [{"/page", fun ?PAGES:page/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(Case, Config) when Case =:= crash_is_json_with_error_pages_off;
                                     Case =:= crash_has_no_html_page_with_error_pages_off ->
    application:set_env(nova, render_error_pages, false),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    application:unset_env(nova, render_error_pages).

plugin_crash_is_json_for_a_client_that_accepts_json(Config) ->
    ?assertEqual(generic_500(), json_page(500, get(Config, "/plugin/page", "application/json"))).

plugin_crash_is_json_without_an_accept_header(Config) ->
    ?assertEqual(generic_500(), json_page(500, nova_test_listener:response(?config(port, Config), "GET",
                                                                           "/plugin/page", []))).

plugin_crash_is_html_for_a_browser(Config) ->
    {500, #{~"content-type" := ~"text/html"}, Body} = get(Config, "/plugin/page", ?BROWSER),
    ?assertNotEqual(nomatch, binary:match(Body, ~"500 Internal Server Error")),
    ?assertEqual(nomatch, binary:match(Body, ~"secretly")).

%% render_error_pages is about the HTML page.
crash_is_json_with_error_pages_off(Config) ->
    ?assertEqual(generic_500(), json_page(500, get(Config, "/plugin/page", "application/json"))),
    ?assertEqual(generic_500(), json_page(500, get(Config, "/guarded/page", "application/json"))).

crash_has_no_html_page_with_error_pages_off(Config) ->
    ?assertMatch({500, _, <<>>}, get(Config, "/plugin/page", ?BROWSER)),
    ?assertMatch({500, _, <<>>}, get(Config, "/guarded/page", ?BROWSER)).

%% What curl and many HTTP clients send.
any_type_is_answered_with_json(Config) ->
    ?assertEqual(#{~"message" => ~"Resource not found"}, json_page(404, get(Config, "/missing", "*/*"))),
    ?assertEqual(generic_500(), json_page(500, get(Config, "/plugin/page", "*/*"))).

unreadable_accept_header_is_answered_with_json(Config) ->
    ?assertEqual(#{~"message" => ~"Resource not found"}, json_page(404, get(Config, "/missing", "text/html;q=abc"))).

excluded_json_is_answered_with_html(Config) ->
    {404, #{~"content-type" := ~"text/html"}, Body} = get(Config, "/missing", "application/json;q=0, */*"),
    ?assertNotEqual(nomatch, binary:match(Body, ~"404 Not found")).

neither_json_nor_html_has_no_body(Config) ->
    ?assertMatch({404, _, <<>>}, get(Config, "/missing", "image/png")),
    ?assertMatch({500, _, <<>>}, get(Config, "/plugin/page", "image/png, */*;q=0")).

get(Config, Path, Accept) ->
    nova_test_listener:response(?config(port, Config), "GET", Path, [{"accept", Accept}]).

json_page(Status, {Status, #{~"content-type" := ~"application/json"}, Body}) ->
    json:decode(Body).

generic_500() ->
    #{~"status" => ~"Internal Server Error",
      ~"title" => ~"500 Internal Server Error",
      ~"message" => ~"Something internal crashed. Please take a look!"}.
