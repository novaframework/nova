%%% The listeners of the `listeners` configuration: each has a name, the
%%% HTTP versions it serves and the applications it serves them for, and a
%%% node that asks for what its server cannot do does not start.
-module(nova_listeners_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         each_listener_serves_its_own_applications/1,
         a_listener_serves_the_versions_it_names/1,
         listeners_are_known_by_name/1,
         an_application_is_added_to_a_listener_by_name/1,
         a_version_the_server_does_not_have_fails_the_boot/1,
         a_misconfigured_listener_fails_the_boot/1,
         a_listener_that_cannot_start_leaves_none_behind/1,
         stopping_the_server_package_stops_the_listeners/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).

all() ->
    [
     each_listener_serves_its_own_applications,
     a_listener_serves_the_versions_it_names,
     listeners_are_known_by_name,
     an_application_is_added_to_a_listener_by_name,
     a_version_the_server_does_not_have_fails_the_boot,
     a_misconfigured_listener_fails_the_boot,
     a_listener_that_cannot_start_leaves_none_behind,
     stopping_the_server_package_stops_the_listeners
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

each_listener_serves_its_own_applications(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual({200, ~"public http1"}, nova_test_listener:get(Public, "/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Admin, "/only-public")),
    ?assertMatch({200, _}, nova_test_listener:get(Public, "/only-public")).

a_listener_serves_the_versions_it_names(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual(200, nova_test_listener:h2_get(Public, ~"/where")),
    ?assertNotEqual(200, catch nova_test_listener:h2_get(Admin, ~"/where")).

listeners_are_known_by_name(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual([admin, public], lists:sort(nova_sup:listeners())),
    ?assertEqual([#{app => nova_test_app, host => {0, 0, 0, 0}, port => Public, listener => public},
                  #{app => nova_test_sub_app, host => {127, 0, 0, 1}, port => Admin, listener => admin}],
                 lists:sort(nova_sup:get_started_applications())).

an_application_is_added_to_a_listener_by_name(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => []}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/where")),
    ?assertEqual({error, {no_such_listener, admin}},
                 nova_sup:add_application(nova_test_sub_app, #{listener => admin})),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/where")),
    ?assertEqual([web], nova_sup:listeners()).

a_version_the_server_does_not_have_fails_the_boot(_Config) ->
    Port = nova_test_listener:free_port(),
    Listeners = #{web => #{port => Port, tls => #{certfile => "cert.pem"}, protocols => [http1, http3]}},
    ?assertMatch({error, {nova_cowboy,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {listener_protocols_not_supported, web, [http3], nova_cowboy}}}, _}}},
                 nova_test_listener:try_start(#{port => Port, listeners => Listeners})),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)).

a_misconfigured_listener_fails_the_boot(_Config) ->
    Port = nova_test_listener:free_port(),
    ?assertMatch({error, {nova_cowboy,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {unknown_listener_settings, web, [use_ssl]}}}, _}}},
                 nova_test_listener:try_start(#{port => Port, listeners => #{web => #{port => Port, use_ssl => true}}})).

a_listener_that_cannot_start_leaves_none_behind(_Config) ->
    First = nova_test_listener:free_port(),
    {ok, Taken} = gen_tcp:listen(0, [{ip, {0, 0, 0, 0}}]),
    {ok, Second} = inet:port(Taken),
    try
        Listeners = #{a => #{port => First, applications => [nova_test_app]},
                      b => #{port => Second, applications => [nova_test_sub_app]}},
        ?assertMatch({error, {nova_cowboy,
                              {{shutdown, {failed_to_start_child, nova_listener, {listener_not_started, eaddrinuse}}},
                               _}}},
                     nova_test_listener:try_start(#{port => First, listeners => Listeners, nova_apps => []})),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, First, [], 1000))
    after
        gen_tcp:close(Taken)
    end.

stopping_the_server_package_stops_the_listeners(_Config) ->
    {Public, Admin} = two_listeners(),
    ok = application:stop(nova_test_app),
    ok = application:stop(nova_cowboy),
    ?assertEqual([], nova_sup:listeners()),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Public, [], 1000)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Admin, [], 1000)),
    ?assertNotEqual(undefined, whereis(nova_sup)).

two_listeners() ->
    Public = nova_test_listener:free_port(),
    Admin = nova_test_listener:free_port(),
    Listeners = #{public => #{port => Public,
                              protocols => [http1, http2],
                              applications => [nova_test_app]},
                  admin => #{ip => {127, 0, 0, 1},
                             port => Admin,
                             protocols => [http1],
                             applications => [nova_test_sub_app]}},
    Only = {"/only-public", fun nova_listeners_pages:where/1, #{methods => [get]}},
    {ok, Public} = nova_test_listener:try_start(#{port => Public,
                                                  listeners => Listeners,
                                                  host => [#{routes => [?WHERE, Only]}],
                                                  mounted => [#{routes => [?WHERE]}],
                                                  nova_apps => []}),
    {Public, Admin}.
