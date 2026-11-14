%%% Where the application that owns the listener and an application it
%%% mounts declare the same route, the owner's is the one served.
-module(nova_mounted_precedence_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         host_route_wins/1,
         host_security_stays/1,
         host_status_page_wins/1,
         mounted_routes_without_a_clash_are_served/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     host_route_wins,
     host_security_stays,
     host_status_page_wins,
     mounted_routes_without_a_clash_are_served
    ].

init_per_suite(Config) ->
    Host = [
            #{routes => [{"/login", fun nova_mounted_precedence_pages:host_page/1, #{methods => [get]}},
                         {404, fun nova_mounted_precedence_pages:host_not_found/1, #{}}]},
            #{prefix => "/admin",
              security => fun nova_mounted_precedence_pages:deny/1,
              routes => [{"/", fun nova_mounted_precedence_pages:host_page/1, #{methods => [get]}}]}
           ],
    Mounted = [
               #{routes => [{"/login", fun nova_mounted_precedence_pages:mounted_page/1, #{methods => [get]}},
                            {"/admin", fun nova_mounted_precedence_pages:mounted_page/1, #{methods => [get]}},
                            {"/mounted-only", fun nova_mounted_precedence_pages:mounted_page/1, #{methods => [get]}},
                            {404, fun nova_mounted_precedence_pages:mounted_not_found/1, #{}}]}
              ],
    Port = nova_test_listener:start(#{host => Host, mounted => Mounted, mounted_options => #{}}),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

host_route_wins(Config) ->
    ?assertEqual({200, ~"host"}, nova_test_listener:get(?config(port, Config), "/login")).

host_security_stays(Config) ->
    ?assertMatch({401, _}, nova_test_listener:get(?config(port, Config), "/admin")).

host_status_page_wins(Config) ->
    ?assertEqual({404, ~"host 404"}, nova_test_listener:get(?config(port, Config), "/no-such-route")).

mounted_routes_without_a_clash_are_served(Config) ->
    ?assertEqual({200, ~"mounted"}, nova_test_listener:get(?config(port, Config), "/mounted-only")).
