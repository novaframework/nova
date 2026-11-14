%%% nova_cors_plugin behind the router, over a real listener.
-module(nova_cors_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([request_gets_the_cors_headers/1,
         preflight_is_answered_by_the_plugin/1,
         route_without_the_plugin_gets_no_cors_headers/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [request_gets_the_cors_headers,
     preflight_is_answered_by_the_plugin,
     route_without_the_plugin_gets_no_cors_headers].

init_per_suite(Config) ->
    Routes = [#{prefix => "/cors",
                plugins => [{pre_request, nova_cors_plugin, #{allow_origins => ~"https://app.example.com"}}],
                routes => [{"/page", fun nova_cors_pages:page/1, #{}}]},
              #{prefix => "/plain",
                plugins => [],
                routes => [{"/page", fun nova_cors_pages:page/1, #{}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

request_gets_the_cors_headers(Config) ->
    {200, Headers, Body} = nova_test_listener:response(?config(port, Config), "GET", "/cors/page",
                                                       [{"origin", "https://app.example.com"}]),
    ?assertEqual(~"controller saw GET", Body),
    ?assertEqual(~"https://app.example.com", maps:get(~"access-control-allow-origin", Headers)),
    ?assert(is_map_key(~"access-control-allow-headers", Headers)),
    ?assert(is_map_key(~"access-control-allow-methods", Headers)).

preflight_is_answered_by_the_plugin(Config) ->
    {204, Headers, Body} = nova_test_listener:response(?config(port, Config), "OPTIONS", "/cors/page",
                                                       [{"origin", "https://app.example.com"},
                                                        {"access-control-request-method", "POST"}]),
    ?assertEqual(<<>>, Body),
    ?assertEqual(~"https://app.example.com", maps:get(~"access-control-allow-origin", Headers)).

route_without_the_plugin_gets_no_cors_headers(Config) ->
    {200, Headers, _Body} = nova_test_listener:response(?config(port, Config), "GET", "/plain/page", []),
    ?assertNot(is_map_key(~"access-control-allow-origin", Headers)).
