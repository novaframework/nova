%%% What an application mounted by a mounted application takes over from
%%% the entry that mounted its parent: override_secure, and nothing else.
-module(nova_nested_app_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
         override_secure_covers_a_nested_application/1,
         override_secure_of_a_nested_application_is_added/1,
         override_secure_reaches_through_an_application_without_one/1,
         nested_application_without_an_override_around_it_keeps_its_own/1,
         first_entry_of_an_application_is_the_one_used/1,
         prefix_secure_and_plugins_are_not_inherited/1,
         override_secure_covers_the_apps_of_a_route_group/1,
         apps_of_a_route_group_inherit_the_mount_but_not_the_group/1,
         own_entry_of_the_bootstrap_application_carries_its_options/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PAGE, [{"/", fun nova_nested_app_pages:page/1, #{methods => [get]}}]).
-define(OUTER, fun nova_nested_app_pages:outer/1).
-define(INNER, fun nova_nested_app_pages:inner/1).
-define(GROUP, fun nova_nested_app_pages:group/1).

all() ->
    [
     override_secure_covers_a_nested_application,
     override_secure_of_a_nested_application_is_added,
     override_secure_reaches_through_an_application_without_one,
     nested_application_without_an_override_around_it_keeps_its_own,
     first_entry_of_an_application_is_the_one_used,
     prefix_secure_and_plugins_are_not_inherited,
     override_secure_covers_the_apps_of_a_route_group,
     apps_of_a_route_group_inherit_the_mount_but_not_the_group,
     own_entry_of_the_bootstrap_application_carries_its_options
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    ok = nova_nested_app_pages:report_calls_to_me(),
    Config.

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop(),
    nova_nested_app_pages:stop_reporting_calls().

%% The status and body of a request, and the callbacks that ran for it.
get(Port, Path) ->
    {Status, Body} = nova_test_listener:get(Port, Path),
    {Status, Body, nova_nested_app_pages:calls()}.

override_secure_covers_a_nested_application(_Config) ->
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", override_secure => ?OUTER},
                                      mounted => [#{prefix => "/own", routes => ?PAGE}],
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested => [#{prefix => "/nested", routes => ?PAGE},
                                                 #{prefix => "/nested-guarded", security => ?GROUP, routes => ?PAGE}]}),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/mounted/own/?outer=deny")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/nested/?outer=deny")),
    ?assertEqual({200, ~"none", [outer]}, get(Port, "/nested/")),
    ?assertEqual({200, ~"outer", [outer]}, get(Port, "/nested/?outer=data")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/nested-guarded/?outer=deny")),
    ?assertEqual({200, ~"none", [outer]}, get(Port, "/nested-guarded/?group=deny")).

override_secure_of_a_nested_application_is_added(_Config) ->
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", override_secure => ?OUTER},
                                      mounted_nova_apps => [{nova_test_nested_app, #{override_secure => ?INNER}}],
                                      nested => [#{prefix => "/nested", security => ?GROUP, routes => ?PAGE}]}),
    ?assertEqual({200, ~"none", [outer, inner]}, get(Port, "/nested/?group=deny")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/nested/?outer=deny")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/nested/?outer=deny&inner=deny")),
    ?assertEqual({403, ~"inner", [outer, inner]}, get(Port, "/nested/?inner=deny")),
    ?assertEqual({403, ~"inner", [outer, inner]}, get(Port, "/nested/?outer=data&inner=deny")),
    ?assertEqual({200, ~"outer", [outer, inner]}, get(Port, "/nested/?outer=data")),
    ?assertEqual({200, ~"inner", [outer, inner]}, get(Port, "/nested/?inner=data")),
    ?assertEqual({200, ~"inner", [outer, inner]}, get(Port, "/nested/?outer=data&inner=data")).

override_secure_reaches_through_an_application_without_one(_Config) ->
    Port = nova_test_listener:start(#{mounted_options => #{override_secure => ?OUTER},
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested_nova_apps => [{nova_test_deep_app, #{prefix => "/deep",
                                                                                  override_secure => ?INNER}}],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}],
                                      deep => [#{routes => ?PAGE}]}),
    ?assertEqual({200, ~"none", [outer]}, get(Port, "/nested/")),
    ?assertEqual({200, ~"none", [outer, inner]}, get(Port, "/deep/")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/deep/?outer=deny")),
    ?assertEqual({403, ~"inner", [outer, inner]}, get(Port, "/deep/?inner=deny")).

nested_application_without_an_override_around_it_keeps_its_own(_Config) ->
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", override_secure => false},
                                      mounted_nova_apps => [{nova_test_nested_app, #{override_secure => ?INNER}}],
                                      nested => [#{prefix => "/nested", security => ?GROUP, routes => ?PAGE}]}),
    ?assertEqual({200, ~"none", [inner]}, get(Port, "/nested/?group=deny")),
    ?assertEqual({403, ~"inner", [inner]}, get(Port, "/nested/?inner=deny")).

%% An application is compiled once, for the first entry that names it.
first_entry_of_an_application_is_the_one_used(_Config) ->
    Port = nova_test_listener:start(#{nova_apps => [nova_test_nested_app,
                                                    {nova_test_sub_app, #{override_secure => ?OUTER}}],
                                      mounted => [#{prefix => "/own", routes => ?PAGE}],
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}]}),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/own/?outer=deny")),
    ?assertEqual({200, ~"none", []}, get(Port, "/nested/?outer=deny")).

prefix_secure_and_plugins_are_not_inherited(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", secure => ?OUTER,
                                                           plugin_strategy => {override, Plugins}},
                                      mounted => [#{prefix => "/own", routes => ?PAGE}],
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}]}),
    ?assertMatch({403, #{~"x-nova-pre-request" := ~"1"}, ~"outer"}, response(Port, "/mounted/own/?outer=deny")),
    ?assertMatch({404, _Headers, _Body}, response(Port, "/mounted/nested/")),
    {200, Headers, ~"none"} = response(Port, "/nested/?outer=deny"),
    ?assertNot(is_map_key(~"x-nova-pre-request", Headers)),
    ?assertEqual([outer], nova_nested_app_pages:calls()).

override_secure_covers_the_apps_of_a_route_group(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", override_secure => ?OUTER,
                                                           plugin_strategy => {override, Plugins}},
                                      mounted => [#{prefix => "/plain", apps => [nova_test_nested_app]},
                                                  #{prefix => "/added",
                                                    apps => [{nova_test_deep_app, #{override_secure => ?INNER}}]}],
                                      nested => [#{prefix => "/nested", security => ?GROUP, routes => ?PAGE}],
                                      deep => [#{prefix => "/deep", security => ?GROUP, routes => ?PAGE}]}),
    ?assertMatch({200, #{~"x-nova-pre-request" := ~"1"}, ~"none"}, response(Port, "/mounted/plain/nested/?group=deny")),
    ?assertEqual([outer], nova_nested_app_pages:calls()),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/mounted/plain/nested/?outer=deny")),
    ?assertEqual({200, ~"none", [outer, inner]}, get(Port, "/mounted/added/deep/?group=deny")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/mounted/added/deep/?outer=deny&inner=deny")),
    ?assertEqual({403, ~"inner", [outer, inner]}, get(Port, "/mounted/added/deep/?inner=deny")).

%% The applications in the `apps` of a route group are compiled with the
%% options their parent was mounted with and the prefix of the group. The
%% `security` and `plugins` of the group are its own.
apps_of_a_route_group_inherit_the_mount_but_not_the_group(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", secure => ?OUTER},
                                      mounted => [#{prefix => "/group", security => ?GROUP, plugins => Plugins,
                                                    apps => [nova_test_nested_app,
                                                             {nova_test_deep_app, #{prefix => "/elsewhere"}}]}],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}],
                                      deep => [#{prefix => "/deep", routes => ?PAGE}]}),
    {200, Headers, ~"none"} = response(Port, "/mounted/group/nested/?group=deny"),
    ?assertNot(is_map_key(~"x-nova-pre-request", Headers)),
    ?assertEqual([outer], nova_nested_app_pages:calls()),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/mounted/group/nested/?outer=deny")),
    ?assertMatch({404, _Body, []}, get(Port, "/nested/")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/elsewhere/deep/?outer=deny")),
    ?assertEqual({200, ~"none", [outer]}, get(Port, "/elsewhere/deep/?group=deny")),
    ?assertMatch({404, _Body, []}, get(Port, "/mounted/group/deep/")).

%% The application that starts the listener has no entry that mounts it. One
%% it writes for itself in its own nova_apps gives it its options.
own_entry_of_the_bootstrap_application_carries_its_options(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    Port = nova_test_listener:start(#{nova_apps => [{nova_test_sub_app, #{prefix => "/mounted"}},
                                                    {nova_test_app, #{secure => ?OUTER,
                                                                      plugin_strategy => {override, Plugins}}}],
                                      host => [#{prefix => "/host", routes => ?PAGE}],
                                      mounted => [#{prefix => "/own", routes => ?PAGE}]}),
    ?assertMatch({200, #{~"x-nova-pre-request" := ~"1"}, ~"none"}, response(Port, "/host/")),
    ?assertEqual([outer], nova_nested_app_pages:calls()),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/host/?outer=deny")),
    {200, Headers, ~"none"} = response(Port, "/mounted/own/?outer=deny"),
    ?assertNot(is_map_key(~"x-nova-pre-request", Headers)),
    ?assertEqual([], nova_nested_app_pages:calls()).

response(Port, Path) ->
    nova_test_listener:response(Port, "GET", Path, []).
