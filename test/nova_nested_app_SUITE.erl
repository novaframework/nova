%%% What an application mounted by a mounted application takes over from
%%% the entry that mounted its parent: override_secure, and nothing else.
-module(nova_nested_app_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
         override_secure_covers_a_nested_application/1,
         override_secure_of_a_nested_application_is_added/1,
         override_secure_reaches_through_an_application_without_one/1,
         nested_application_without_an_override_around_it_keeps_its_own/1,
         application_mounted_twice_with_different_security_is_refused/1,
         application_mounted_twice_with_the_same_security_is_mounted_once/1,
         callback_written_as_a_fun_and_as_a_tuple_is_the_same_security/1,
         secure_covers_a_nested_application_but_prefix_and_plugins_do_not/1,
         secure_of_every_application_around_a_nested_one_has_to_allow/1,
         override_secure_replaces_the_secure_around_a_nested_application/1,
         override_secure_covers_the_apps_of_a_route_group/1,
         apps_of_a_route_group_inherit_the_mount_and_the_security_of_the_group/1,
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
     application_mounted_twice_with_different_security_is_refused,
     application_mounted_twice_with_the_same_security_is_mounted_once,
     callback_written_as_a_fun_and_as_a_tuple_is_the_same_security,
     secure_covers_a_nested_application_but_prefix_and_plugins_do_not,
     secure_of_every_application_around_a_nested_one_has_to_allow,
     override_secure_replaces_the_secure_around_a_nested_application,
     override_secure_covers_the_apps_of_a_route_group,
     apps_of_a_route_group_inherit_the_mount_and_the_security_of_the_group,
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

%% An application is compiled once, for the first entry that names it. Two
%% entries that ask for different callbacks leave it open which of them
%% protects the application, so Nova does not start.
application_mounted_twice_with_different_security_is_refused(_Config) ->
    Nested = [#{prefix => "/nested", routes => ?PAGE}],
    [?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                              {nova_app_mounted_twice, nova_test_nested_app}}}, _}}},
                  nova_test_listener:try_start(#{nova_apps => NovaApps,
                                                 mounted_nova_apps => [nova_test_nested_app],
                                                 nested => Nested}))
     || NovaApps <- [[nova_test_nested_app, {nova_test_sub_app, #{override_secure => ?OUTER}}],
                     [nova_test_nested_app, {nova_test_sub_app, #{secure => ?OUTER}}],
                     [nova_test_sub_app, {nova_test_nested_app, #{secure => ?OUTER}}]]].

%% The secure an entry inherits and the one it declares are the same wish.
application_mounted_twice_with_the_same_security_is_mounted_once(_Config) ->
    Port = nova_test_listener:start(#{nova_apps => [{nova_test_nested_app, #{prefix => "/first", secure => ?OUTER}},
                                                    {nova_test_sub_app, #{secure => ?OUTER}}],
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}]}),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/first/nested/?outer=deny")),
    ?assertMatch({404, _Body, []}, get(Port, "/nested/")).

callback_written_as_a_fun_and_as_a_tuple_is_the_same_security(_Config) ->
    Port = nova_test_listener:start(#{nova_apps => [{nova_test_nested_app,
                                                     #{prefix => "/first", secure => {nova_nested_app_pages, outer}}},
                                                    {nova_test_sub_app, #{secure => ?OUTER}}],
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}]}),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/first/nested/?outer=deny")).

secure_covers_a_nested_application_but_prefix_and_plugins_do_not(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", secure => ?OUTER,
                                                           plugin_strategy => {override, Plugins}},
                                      mounted => [#{prefix => "/own", routes => ?PAGE}],
                                      mounted_nova_apps => [nova_test_nested_app],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}]}),
    ?assertMatch({200, #{~"x-nova-pre-request" := ~"1"}, ~"none"}, response(Port, "/mounted/own/")),
    ?assertMatch({403, _, ~"outer"}, response(Port, "/mounted/own/?outer=deny")),
    ?assertMatch({404, _Headers, _Body}, response(Port, "/mounted/nested/")),
    {200, Headers, ~"none"} = response(Port, "/nested/"),
    ?assertNot(is_map_key(~"x-nova-pre-request", Headers)),
    ?assertMatch({403, _, ~"outer"}, response(Port, "/nested/?outer=deny")),
    ?assertEqual([outer, outer, outer, outer], nova_nested_app_pages:calls()).

%% Outermost first, and the security of the route group last.
secure_of_every_application_around_a_nested_one_has_to_allow(_Config) ->
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", secure => ?OUTER},
                                      mounted_nova_apps => [{nova_test_nested_app, #{secure => ?INNER}}],
                                      nested_nova_apps => [{nova_test_deep_app, #{prefix => "/deep"}}],
                                      nested => [#{prefix => "/nested", security => ?GROUP, routes => ?PAGE}],
                                      deep => [#{routes => ?PAGE}]}),
    ?assertEqual({200, ~"none", [outer, inner, group]}, get(Port, "/nested/")),
    ?assertEqual({403, ~"group", [outer, inner, group]}, get(Port, "/nested/?group=deny")),
    ?assertEqual({200, ~"none", [outer, inner]}, get(Port, "/deep/")),
    ?assertEqual({403, ~"inner", [outer, inner]}, get(Port, "/deep/?inner=deny")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/deep/?outer=deny&inner=deny")).

%% override_secure is the one way to replace a callback, an inherited secure
%% included.
override_secure_replaces_the_secure_around_a_nested_application(_Config) ->
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", secure => ?OUTER},
                                      mounted_nova_apps => [{nova_test_nested_app, #{override_secure => ?INNER}}],
                                      nested => [#{prefix => "/nested", security => ?GROUP, routes => ?PAGE}]}),
    ?assertEqual({200, ~"none", [inner]}, get(Port, "/nested/?outer=deny&group=deny")),
    ?assertEqual({403, ~"inner", [inner]}, get(Port, "/nested/?inner=deny")).

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
%% options their parent was mounted with and the prefix of the group, behind
%% the `security` of the group. The `plugins` of the group are its own.
apps_of_a_route_group_inherit_the_mount_and_the_security_of_the_group(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    Port = nova_test_listener:start(#{mounted_options => #{prefix => "/mounted", secure => ?OUTER},
                                      mounted => [#{prefix => "/group", security => ?GROUP, plugins => Plugins,
                                                    apps => [nova_test_nested_app,
                                                             {nova_test_deep_app, #{prefix => "/elsewhere"}}]}],
                                      nested => [#{prefix => "/nested", routes => ?PAGE}],
                                      deep => [#{prefix => "/deep", routes => ?PAGE}]}),
    {200, Headers, ~"none"} = response(Port, "/mounted/group/nested/"),
    ?assertNot(is_map_key(~"x-nova-pre-request", Headers)),
    ?assertEqual([outer, group], nova_nested_app_pages:calls()),
    ?assertEqual({403, ~"group", [outer, group]}, get(Port, "/mounted/group/nested/?group=deny")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/mounted/group/nested/?outer=deny")),
    ?assertMatch({404, _Body, []}, get(Port, "/nested/")),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/elsewhere/deep/?outer=deny")),
    ?assertEqual({403, ~"group", [outer, group]}, get(Port, "/elsewhere/deep/?group=deny")),
    ?assertMatch({404, _Body, []}, get(Port, "/mounted/group/deep/")).

%% The application that starts the listener has no entry that mounts it. One
%% it writes for itself in its own nova_apps gives it its options, and the
%% secure of that entry covers the applications it mounts.
own_entry_of_the_bootstrap_application_carries_its_options(_Config) ->
    Plugins = [{pre_request, nova_test_app_plugin, #{}}],
    {Port, Events} =
        nova_test_log:collect(
          fun() ->
              nova_test_listener:start(#{nova_apps => [{nova_test_sub_app, #{prefix => "/mounted"}},
                                                       {nova_test_app, #{secure => ?OUTER,
                                                                         plugin_strategy => {override, Plugins}}}],
                                         host => [#{prefix => "/host", routes => ?PAGE}],
                                         mounted => [#{prefix => "/own", routes => ?PAGE}]})
          end),
    ?assertMatch([#{app := nova_test_app, options := [plugin_strategy, secure]}],
                 [Report || {notice, {report, #{msg := ~"Application has an entry for itself in its nova_apps, and is compiled with its options"} = Report}} <- Events]),
    ?assertMatch({200, #{~"x-nova-pre-request" := ~"1"}, ~"none"}, response(Port, "/host/")),
    ?assertEqual([outer], nova_nested_app_pages:calls()),
    ?assertEqual({403, ~"outer", [outer]}, get(Port, "/host/?outer=deny")),
    {403, Headers, ~"outer"} = response(Port, "/mounted/own/?outer=deny"),
    ?assertNot(is_map_key(~"x-nova-pre-request", Headers)),
    ?assertEqual([outer], nova_nested_app_pages:calls()).

response(Port, Path) ->
    nova_test_listener:response(Port, "GET", Path, []).
