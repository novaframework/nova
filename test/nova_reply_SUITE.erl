%%% What Cowboy refuses to send does not turn a response into a 500.
-module(nova_reply_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([body_of_a_204_is_left_out/1,
         body_of_a_304_is_left_out/1,
         json_with_a_204_is_left_out/1,
         reply_from_a_post_request_plugin_comes_too_late/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PAGES, nova_reply_pages).

all() ->
    [body_of_a_204_is_left_out,
     body_of_a_304_is_left_out,
     json_with_a_204_is_left_out,
     reply_from_a_post_request_plugin_comes_too_late].

init_per_suite(Config) ->
    Routes = [#{prefix => "/empty",
                plugins => [],
                routes => [{"/204", fun ?PAGES:no_content_with_a_body/1, #{methods => [get]}},
                           {"/304", fun ?PAGES:not_modified_with_a_body/1, #{methods => [get]}},
                           {"/json", fun ?PAGES:json_no_content/1, #{methods => [delete]}}]},
              #{prefix => "/late",
                plugins => [{post_request, ?PAGES, #{}}],
                routes => [{"/page", fun ?PAGES:page/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

body_of_a_204_is_left_out(Config) ->
    ?assertEqual({204, <<>>}, quietly(fun() -> nova_test_listener:get(?config(port, Config), "/empty/204") end)).

body_of_a_304_is_left_out(Config) ->
    {{304, Headers, Body}, Events} =
        nova_test_log:collect(fun() -> nova_test_listener:response(?config(port, Config), "GET", "/empty/304", []) end),
    ?assertEqual(<<>>, Body),
    ?assertEqual(~"\"v1\"", maps:get(~"etag", Headers)),
    ?assertEqual([], nova_test_log:errors(Events)).

json_with_a_204_is_left_out(Config) ->
    ?assertEqual({204, <<>>},
                 quietly(fun() -> nova_test_listener:request(?config(port, Config), "DELETE", "/empty/json", []) end)).

%% The controller has answered by the time a post_request plugin runs.
reply_from_a_post_request_plugin_comes_too_late(Config) ->
    ?assertEqual({200, ~"page"}, quietly(fun() -> nova_test_listener:get(?config(port, Config), "/late/page") end)).

quietly(Request) ->
    {Response, Events} = nova_test_log:collect(Request),
    ?assertEqual([], nova_test_log:errors(Events)),
    Response.
