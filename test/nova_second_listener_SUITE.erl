%%% An application on a listener of its own gets its own status pages, and
%%% the router answers for the listener a request came in on.
-module(nova_second_listener_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         crash_renders_the_listeners_own_page/1,
         thrown_status_renders_the_listeners_own_page/1,
         not_found_renders_the_listeners_own_page/1,
         bootstrap_listener_keeps_its_pages/1,
         lookup_by_dispatch_key/1,
         dispatch_key_of_a_request/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     crash_renders_the_listeners_own_page,
     thrown_status_renders_the_listeners_own_page,
     not_found_renders_the_listeners_own_page,
     bootstrap_listener_keeps_its_pages,
     lookup_by_dispatch_key,
     dispatch_key_of_a_request
    ].

init_per_suite(Config) ->
    First = [#{routes => [{"/crash", fun nova_second_listener_pages:crash/1, #{methods => [get]}},
                          {"/key", fun nova_second_listener_pages:key/1, #{methods => [get]}},
                          {500, fun nova_second_listener_pages:first_error/1, #{}}]}],
    Second = [#{routes => [{"/crash", fun nova_second_listener_pages:crash/1, #{methods => [get]}},
                           {"/missing", fun nova_second_listener_pages:missing/1, #{methods => [get]}},
                           {"/key", fun nova_second_listener_pages:key/1, #{methods => [get]}},
                           {"/only-second", fun nova_second_listener_pages:key/1, #{methods => [get]}},
                           {500, fun nova_second_listener_pages:second_error/1, #{}},
                           {404, fun nova_second_listener_pages:second_not_found/1, #{}}]}],
    FirstPort = nova_test_listener:start(#{host => First, mounted => Second, nova_apps => []}),
    SecondPort = nova_test_listener:free_port(),
    {ok, nova_test_sub_app, _Host, SecondPort} =
        nova_sup:add_application(nova_test_sub_app, #{port => SecondPort}),
    [{first_port, FirstPort}, {second_port, SecondPort} | Config].

end_per_suite(_Config) ->
    nova_sup:remove_application(nova_test_sub_app),
    nova_test_listener:stop().

crash_renders_the_listeners_own_page(Config) ->
    ?assertEqual({500, ~"second 500"}, nova_test_listener:get(?config(second_port, Config), "/crash")).

thrown_status_renders_the_listeners_own_page(Config) ->
    ?assertEqual({404, ~"second 404"}, nova_test_listener:get(?config(second_port, Config), "/missing")).

not_found_renders_the_listeners_own_page(Config) ->
    ?assertEqual({404, ~"second 404"}, nova_test_listener:get(?config(second_port, Config), "/no-such-route")).

bootstrap_listener_keeps_its_pages(Config) ->
    Port = ?config(first_port, Config),
    ?assertEqual({500, ~"first 500"}, nova_test_listener:get(Port, "/crash")),
    {Status, Body} = nova_test_listener:get(Port, "/no-such-route"),
    ?assertEqual(404, Status),
    ?assertNotEqual(~"second 404", Body).

lookup_by_dispatch_key(Config) ->
    SecondKey = {nova_dispatch, nova_test_sub_app, ?config(second_port, Config)},
    ?assertMatch({ok, _, _}, nova_router:lookup_url('_', ~"/only-second", ~"GET", SecondKey)),
    ?assertEqual({error, not_found}, nova_router:lookup_url('_', ~"/only-second", ~"GET", nova_dispatch)),
    ?assertEqual({error, not_found}, nova_router:lookup_url('_', ~"/only-second", ~"GET")).

dispatch_key_of_a_request(Config) ->
    SecondPort = ?config(second_port, Config),
    Expected = iolist_to_binary(io_lib:format("~0p", [{nova_dispatch, nova_test_sub_app, SecondPort}])),
    ?assertEqual({200, ~"nova_dispatch"}, nova_test_listener:get(?config(first_port, Config), "/key")),
    ?assertEqual({200, Expected}, nova_test_listener:get(SecondPort, "/key")).
