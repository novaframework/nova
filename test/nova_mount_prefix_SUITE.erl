%%% The prefix in a nova_apps entry and the prefix of a route group in the
%%% mounted application add up, and the mounted application can look up
%%% where it was mounted.
-module(nova_mount_prefix_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         mount_and_group_prefixes_concatenate/1,
         mount_prefix_is_discoverable/1,
         unmounted_application_reports_the_root/1
        ]).

-export([page/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     mount_and_group_prefixes_concatenate,
     mount_prefix_is_discoverable,
     unmounted_application_reports_the_root
    ].

init_per_suite(Config) ->
    Routes = [{"/page", fun ?MODULE:page/1, #{methods => [get]}}],
    Port = nova_test_listener:start(#{host => [#{prefix => "/host", routes => Routes}],
                                      mounted => [#{prefix => "/group", routes => Routes},
                                                  #{routes => Routes}],
                                      mounted_options => #{prefix => "/mounted"}}),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

page(_Req) ->
    {status, 200, #{}, ~"page"}.

mount_and_group_prefixes_concatenate(Config) ->
    Port = ?config(port, Config),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/mounted/group/page")),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/mounted/page")),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/host/page")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/group/page")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/page")).

mount_prefix_is_discoverable(_Config) ->
    ?assertEqual({nova_test_sub_app, "/mounted"}, lists:keyfind(nova_test_sub_app, 1, nova:get_apps())).

unmounted_application_reports_the_root(_Config) ->
    ?assertEqual({nova_test_app, "/"}, lists:keyfind(nova_test_app, 1, nova:get_apps())).
