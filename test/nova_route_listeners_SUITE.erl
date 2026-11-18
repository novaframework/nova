%%% What a router says about where and how its routes are served: a route
%%% group names the listeners it is for, and a route or its group the HTTP
%%% versions it is served over. What could not be served fails the boot.
-module(nova_route_listeners_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         group_is_served_on_the_listeners_it_names/1,
         applications_of_a_group_are_served_where_it_is/1,
         group_for_a_listener_that_is_not_there_fails_the_boot/1,
         group_for_a_listener_without_its_application_fails_the_boot/1,
         route_is_served_over_the_versions_it_names/1,
         route_takes_the_versions_of_its_group/1,
         http1_is_told_which_version_to_come_back_with/1,
         versions_the_listener_lacks_fail_the_boot/1,
         versions_that_are_none_fail_the_boot/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).
-define(WHERE(Path, Versions),
        {Path, fun nova_listeners_pages:where/1, #{methods => [get], http_versions => Versions}}).

all() ->
    [
     group_is_served_on_the_listeners_it_names,
     applications_of_a_group_are_served_where_it_is,
     group_for_a_listener_that_is_not_there_fails_the_boot,
     group_for_a_listener_without_its_application_fails_the_boot,
     route_is_served_over_the_versions_it_names,
     route_takes_the_versions_of_its_group,
     http1_is_told_which_version_to_come_back_with,
     versions_the_listener_lacks_fail_the_boot,
     versions_that_are_none_fail_the_boot
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

group_is_served_on_the_listeners_it_names(_Config) ->
    {ok, Public, Admin} = two_listeners([nova_test_app],
                                        #{host => [#{routes => [?WHERE]},
                                                   #{prefix => "/admin", listener => admin, routes => [?WHERE]},
                                                   #{prefix => "/both", listener => [admin, public],
                                                     routes => [?WHERE]}]}),
    ?assertEqual({200, ~"public http1"}, nova_test_listener:get(Public, "/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Public, "/admin/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/admin/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/where")),
    ?assertEqual({200, ~"public http1"}, nova_test_listener:get(Public, "/both/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/both/where")).

applications_of_a_group_are_served_where_it_is(_Config) ->
    {ok, Public, Admin} = two_listeners([nova_test_app],
                                        #{host => [#{prefix => "/admin", listener => admin,
                                                     apps => [nova_test_sub_app]}],
                                          mounted => [#{routes => [?WHERE]}]}),
    ?assertMatch({404, _}, nova_test_listener:get(Public, "/admin/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/admin/where")).

group_for_a_listener_that_is_not_there_fails_the_boot(_Config) ->
    Routes = #{host => [#{routes => [?WHERE]}, #{prefix => "/admin", listener => amdin, routes => [?WHERE]}]},
    ?assertEqual({route_group_not_served, nova_test_app, "/admin", amdin},
                 refusal(two_listeners([nova_test_app], Routes))).

group_for_a_listener_without_its_application_fails_the_boot(_Config) ->
    Routes = #{host => [#{routes => [?WHERE]}, #{prefix => "/admin", listener => admin, routes => [?WHERE]}],
               mounted => [#{routes => [?WHERE]}]},
    ?assertEqual({route_group_not_served, nova_test_app, "/admin", admin},
                 refusal(two_listeners([nova_test_sub_app], Routes))).

%% Without TLS, so over the two versions Cowboy serves on one port.
route_is_served_over_the_versions_it_names(_Config) ->
    Port = one_listener(#{protocols => [http1, http2]},
                        [?WHERE("/one", [http1]), ?WHERE("/two", [http2]), ?WHERE("/both", [http1, http2]), ?WHERE]),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/one")),
    ?assertEqual(505, nova_test_listener:h2_get(Port, ~"/one")),
    ?assertMatch({426, _}, nova_test_listener:get(Port, "/two")),
    ?assertEqual(200, nova_test_listener:h2_get(Port, ~"/two")),
    [begin
         ?assertMatch({200, _}, nova_test_listener:get(Port, Path)),
         ?assertEqual(200, nova_test_listener:h2_get(Port, iolist_to_binary(Path)))
     end || Path <- ["/both", "/where"]].

route_takes_the_versions_of_its_group(_Config) ->
    Group = #{prefix => "/group", http_versions => [http2], routes => [?WHERE, ?WHERE("/own", [http1])]},
    Port = one_listener(#{protocols => [http1, http2]}, [Group]),
    ?assertMatch({426, _}, nova_test_listener:get(Port, "/group/where")),
    ?assertEqual(200, nova_test_listener:h2_get(Port, ~"/group/where")),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/group/own")),
    ?assertEqual(505, nova_test_listener:h2_get(Port, ~"/group/own")).

%% Of the versions a route names, those its listener serves.
http1_is_told_which_version_to_come_back_with(_Config) ->
    Port = one_listener(#{tls => nova_test_listener:tls(), protocols => [http1, http2]},
                        [?WHERE("/two", [http2, http3]), ?WHERE]),
    {Status, Headers, _Body} = nova_test_listener:https_get(Port, "/two"),
    ?assertEqual({426, ~"HTTP/2.0"}, {Status, maps:get(~"upgrade", Headers)}),
    ?assert(lists:member(~"upgrade", binary:split(string:lowercase(maps:get(~"connection", Headers)), ~", ",
                                                  [global]))),
    ?assertMatch({200, _, ~"web http1"}, nova_test_listener:https_get(Port, "/where")).

versions_the_listener_lacks_fail_the_boot(_Config) ->
    ?assertEqual({route_http_versions_not_served, "/two", [http2, http3], web},
                 refusal(try_one_listener(#{protocols => [http1]}, [?WHERE("/two", [http2, http3])]))).

versions_that_are_none_fail_the_boot(_Config) ->
    [?assertEqual({invalid_http_versions, "/where", Versions},
                  refusal(try_one_listener(#{}, [?WHERE("/where", Versions)])))
     || Versions <- [[], [spdy], http2, [http1, ~"http2"]]].

one_listener(Settings, Routes) ->
    {ok, Port} = try_one_listener(Settings, Routes),
    Port.

try_one_listener(Settings, Routes) ->
    Port = nova_test_listener:free_port(),
    Groups = [case Route of
                  #{} -> Route;
                  _ -> #{routes => [Route]}
              end || Route <- Routes],
    Result = nova_test_listener:try_start(#{port => Port,
                                            listeners => #{web => Settings#{port => Port}},
                                            host => Groups,
                                            nova_apps => []}),
    refused_binds_nothing(Result, Port).

%% `public` serves nova_test_app, and `admin` the applications given.
two_listeners(AdminApplications, Routes) ->
    Public = nova_test_listener:free_port(),
    Admin = nova_test_listener:free_port(),
    Listeners = #{public => #{port => Public, applications => [nova_test_app]},
                  admin => #{ip => {127, 0, 0, 1}, port => Admin, applications => AdminApplications}},
    case refused_binds_nothing(nova_test_listener:try_start(Routes#{port => Public,
                                                                    listeners => Listeners,
                                                                    nova_apps => []}),
                               Public) of
        {ok, Public} -> {ok, Public, Admin};
        {error, Reason} -> {error, Reason}
    end.

refused_binds_nothing({error, _} = Refused, Port) ->
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)),
    Refused;
refused_binds_nothing(Started, _Port) ->
    Started.

refusal({error, {_Server, {{shutdown, {failed_to_start_child, nova_listener, Reason}}, _Start}}}) ->
    Reason.
