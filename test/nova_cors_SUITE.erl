%%% nova_cors_plugin behind the router, over a real listener.
-module(nova_cors_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([request_gets_the_cors_headers/1,
         preflight_is_answered_by_the_plugin/1,
         route_without_the_plugin_gets_no_cors_headers/1,
         preflight_for_a_method_restricted_route_is_answered/1,
         preflight_from_another_origin_gets_no_allow_origin/1,
         preflight_for_a_method_the_route_lacks_is_a_405/1,
         preflight_without_the_plugin_is_a_405/1,
         plain_options_on_a_method_restricted_route_is_a_405/1,
         wrong_method_does_not_run_the_plugins/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [request_gets_the_cors_headers,
     preflight_is_answered_by_the_plugin,
     route_without_the_plugin_gets_no_cors_headers,
     preflight_for_a_method_restricted_route_is_answered,
     preflight_from_another_origin_gets_no_allow_origin,
     preflight_for_a_method_the_route_lacks_is_a_405,
     preflight_without_the_plugin_is_a_405,
     plain_options_on_a_method_restricted_route_is_a_405,
     wrong_method_does_not_run_the_plugins].

init_per_suite(Config) ->
    Routes = [#{prefix => "/cors",
                plugins => [{pre_request, nova_cors_plugin, #{allow_origins => ~"https://app.example.com"}}],
                routes => [{"/page", fun nova_cors_pages:page/1, #{}},
                           {"/post-only", fun nova_cors_pages:page/1, #{methods => [post]}}]},
              #{prefix => "/plain",
                plugins => [],
                routes => [{"/page", fun nova_cors_pages:page/1, #{}},
                           {"/post-only", fun nova_cors_pages:page/1, #{methods => [post]}}]}],
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

preflight_for_a_method_restricted_route_is_answered(Config) ->
    {204, Headers, Body} = preflight(Config, "/cors/post-only", "https://app.example.com", "POST"),
    ?assertEqual(<<>>, Body),
    ?assertEqual(~"https://app.example.com", maps:get(~"access-control-allow-origin", Headers)),
    ?assert(is_map_key(~"access-control-allow-methods", Headers)),
    ?assertNot(is_map_key(~"access-control-allow-credentials", Headers)).

preflight_from_another_origin_gets_no_allow_origin(Config) ->
    {204, Headers, _Body} = preflight(Config, "/cors/post-only", "https://evil.example.com", "POST"),
    ?assertNot(is_map_key(~"access-control-allow-origin", Headers)).

preflight_for_a_method_the_route_lacks_is_a_405(Config) ->
    {405, Headers, _Body} = preflight(Config, "/cors/post-only", "https://app.example.com", "DELETE"),
    ?assertEqual(~"POST", maps:get(~"allow", Headers)),
    ?assertNot(is_map_key(~"access-control-allow-origin", Headers)).

preflight_without_the_plugin_is_a_405(Config) ->
    {405, Headers, _Body} = preflight(Config, "/plain/post-only", "https://app.example.com", "POST"),
    ?assertEqual(~"POST", maps:get(~"allow", Headers)),
    ?assertNot(is_map_key(~"access-control-allow-origin", Headers)).

plain_options_on_a_method_restricted_route_is_a_405(Config) ->
    {405, Headers, _Body} = nova_test_listener:response(?config(port, Config), "OPTIONS", "/cors/post-only",
                                                        [{"origin", "https://app.example.com"}]),
    ?assertNot(is_map_key(~"access-control-allow-origin", Headers)).

wrong_method_does_not_run_the_plugins(Config) ->
    {405, Headers, _Body} = nova_test_listener:response(?config(port, Config), "GET", "/cors/post-only",
                                                        [{"origin", "https://app.example.com"}]),
    ?assertNot(is_map_key(~"access-control-allow-origin", Headers)).

preflight(Config, Path, Origin, Method) ->
    nova_test_listener:response(?config(port, Config), "OPTIONS", Path,
                                [{"origin", Origin}, {"access-control-request-method", Method}]).
