%%% Nova either starts with its listener serving requests or does not start.
-module(nova_boot_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         port_in_use_fails_the_boot/1,
         listener_starts_after_the_processes_it_needs/1,
         restarted_supervisor_serves_again/1,
         listener_of_another_owner_fails_the_boot/1,
         invalid_security_fails_the_boot/1,
         invalid_security_of_a_mounted_application_fails_the_boot/1,
         invalid_security_of_a_mounted_application_fails_the_boot_next_to_a_group_callback/1,
         invalid_security_replaced_by_an_override_fails_the_boot/1,
         misplaced_security_callback_fails_the_boot/1,
         misplaced_security_that_is_false_is_warned_about/1,
         pubsub_scope_is_restarted/1,
         restarted_pubsub_scope_has_no_members/1,
         pubsub_scope_started_elsewhere_is_left_alone/1,
         unset_environment_is_prod_and_said_once/1,
         set_environment_is_not_mentioned/1,
         unknown_non_canonical_paths_fails_the_boot/1,
         unknown_trusted_origins_fails_the_boot/1,
         unknown_trusted_origins_of_a_plugin_fails_the_boot/1,
         allowed_origins_of_a_plugin_fails_the_boot/1,
         session_store_for_no_session_fails_the_boot/1,
         plugins_before_security_that_names_no_plugin_fails_the_boot/1,
         plugins_before_security_of_a_route_group_that_names_none_of_its_plugins_fails_the_boot/1,
         setting_that_is_gone_fails_the_boot/1,
         plugins_after_security_of_a_route_group_fails_the_boot/1,
         csrf_settings_nothing_can_go_by_fail_the_boot/1,
         secret_that_fails_the_boot_is_not_told/1,
         secret_is_not_told_by_a_boot/1,
         csrf_plugin_without_sessions_fails_the_boot/1,
         csrf_tokens_kept_in_the_session_store_are_said_once/1,
         body_decoded_behind_a_security_callback_is_warned_about/1,
         security_reads_body_of_a_route_group_is_checked_by_the_boot/1,
         plugins_of_an_earlier_run_are_forgotten/1
        ]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     port_in_use_fails_the_boot,
     listener_starts_after_the_processes_it_needs,
     restarted_supervisor_serves_again,
     listener_of_another_owner_fails_the_boot,
     invalid_security_fails_the_boot,
     invalid_security_of_a_mounted_application_fails_the_boot,
     invalid_security_of_a_mounted_application_fails_the_boot_next_to_a_group_callback,
     invalid_security_replaced_by_an_override_fails_the_boot,
     misplaced_security_callback_fails_the_boot,
     misplaced_security_that_is_false_is_warned_about,
     pubsub_scope_is_restarted,
     restarted_pubsub_scope_has_no_members,
     pubsub_scope_started_elsewhere_is_left_alone,
     unset_environment_is_prod_and_said_once,
     set_environment_is_not_mentioned,
     unknown_non_canonical_paths_fails_the_boot,
     unknown_trusted_origins_fails_the_boot,
     unknown_trusted_origins_of_a_plugin_fails_the_boot,
     allowed_origins_of_a_plugin_fails_the_boot,
     session_store_for_no_session_fails_the_boot,
     plugins_before_security_that_names_no_plugin_fails_the_boot,
     plugins_before_security_of_a_route_group_that_names_none_of_its_plugins_fails_the_boot,
     setting_that_is_gone_fails_the_boot,
     plugins_after_security_of_a_route_group_fails_the_boot,
     csrf_settings_nothing_can_go_by_fail_the_boot,
     secret_that_fails_the_boot_is_not_told,
     secret_is_not_told_by_a_boot,
     csrf_plugin_without_sessions_fails_the_boot,
     csrf_tokens_kept_in_the_session_store_are_said_once,
     body_decoded_behind_a_security_callback_is_warned_about,
     security_reads_body_of_a_route_group_is_checked_by_the_boot,
     plugins_of_an_earlier_run_are_forgotten
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

port_in_use_fails_the_boot(_Config) ->
    {ok, Socket} = gen_tcp:listen(0, [{ip, {0, 0, 0, 0}}]),
    {ok, Port} = inet:port(Socket),
    try
        ?assertMatch({error, _}, nova_test_listener:try_start(#{port => Port})),
        ?assertEqual(undefined, whereis(nova_sup))
    after
        gen_tcp:close(Socket)
    end.

listener_starts_after_the_processes_it_needs(_Config) ->
    ok = nova_test_listener:configure(#{}),
    {ok, {_SupFlags, Children}} = nova_sup:init([]),
    Ids = [Id || #{id := Id} <- Children],
    ?assertEqual(nova_listener, lists:last(Ids)),
    ?assertEqual([], [nova_handlers, nova_plugin_manager, nova_session_ets] -- lists:droplast(Ids)),
    ?assertEqual([], nova_sup:listeners()).

restarted_supervisor_serves_again(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    Port = nova_test_listener:start(#{host => [#{routes => Routes}]}),
    Supervisor = running(nova_sup),
    Monitor = monitor(process, Supervisor),
    exit(Supervisor, kill),
    receive {'DOWN', Monitor, process, Supervisor, killed} -> ok end,
    ?assert(stopped(nova, 50)),
    ?assertMatch({ok, _}, application:ensure_all_started(nova)),
    ?assertEqual({200, ~"page"}, nova_test_listener:get(Port, "/")),
    ?assertEqual([nova_listener], nova_sup:listeners()).

listener_of_another_owner_fails_the_boot(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = cowboy:start_clear(nova_listener, [{port, Port}], #{env => #{dispatch => cowboy_router:compile([])}}),
    try
        ?assertMatch({error, _}, nova_test_listener:try_start(#{port => nova_test_listener:free_port()})),
        ?assertEqual(Port, ranch:get_port(nova_listener))
    after
        cowboy:stop_listener(nova_listener)
    end.

invalid_security_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener, {invalid_security, true}}}, _}}},
                 nova_test_listener:try_start(#{host => [#{security => true, routes => Routes}]})),
    ?assertEqual(undefined, whereis(nova_sup)).

invalid_security_of_a_mounted_application_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener, {invalid_security, yes}}}, _}}},
                 nova_test_listener:try_start(#{mounted => [#{routes => Routes}],
                                                mounted_options => #{prefix => "/mounted", secure => yes}})),
    ?assertEqual(undefined, whereis(nova_sup)).

invalid_security_of_a_mounted_application_fails_the_boot_next_to_a_group_callback(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener, {invalid_security, yes}}}, _}}},
                 nova_test_listener:try_start(#{mounted => [#{security => fun nova_boot_pages:allow/1,
                                                              routes => Routes}],
                                                mounted_options => #{prefix => "/mounted", secure => yes}})),
    ?assertEqual(undefined, whereis(nova_sup)).

%% override_secure replaces the callbacks, which have to be usable all the
%% same: the routes would be open the day the override is taken away.
invalid_security_replaced_by_an_override_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    Override = #{prefix => "/mounted", override_secure => fun nova_boot_pages:allow/1},
    ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener, {invalid_security, true}}}, _}}},
                 nova_test_listener:try_start(#{mounted => [#{security => true, routes => Routes}],
                                                mounted_options => Override})),
    ?assertEqual(undefined, whereis(nova_sup)),
    ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener, {invalid_security, yes}}}, _}}},
                 nova_test_listener:try_start(#{mounted => [#{routes => Routes}],
                                                mounted_options => Override#{secure => yes}})),
    ?assertEqual(undefined, whereis(nova_sup)).

%% A callback under a key that is not read where it is written would leave
%% the routes its author believes protected open.
misplaced_security_callback_fails_the_boot(_Config) ->
    Allow = fun nova_boot_pages:allow/1,
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    Route = fun(Key) -> [{"/page", fun nova_boot_pages:page/1, #{methods => [get], Key => Allow}}] end,
    [begin
         {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(Options) end),
         ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                  {misplaced_security, App, Key, Path}}}, _}}},
                      Result),
         ?assertMatch([#{app := App, key := Key, path := Path} | _], nova_test_log:errors(Events)),
         ?assertEqual(undefined, whereis(nova_sup))
     end || {App, Key, Path, Options}
                <- [{nova_test_sub_app, security, "/mounted",
                     #{mounted => [#{routes => Routes}],
                       mounted_options => #{prefix => "/mounted", security => Allow}}},
                    {nova_test_app, secure, "/admin",
                     #{host => [#{prefix => "/admin", secure => Allow, routes => Routes}]}},
                    {nova_test_app, security, "/admin/page",
                     #{host => [#{prefix => "/admin", routes => Route(security)}]}},
                    {nova_test_app, secure, "/admin/page",
                     #{host => [#{prefix => "/admin", routes => Route(secure)}]}}]].

misplaced_security_that_is_false_is_warned_about(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    {Port, Events} =
        nova_test_log:collect(fun() ->
                                      nova_test_listener:start(#{host => [#{prefix => "/admin", secure => false,
                                                                            routes => Routes}],
                                                                 mounted => [#{routes => Routes}],
                                                                 mounted_options => #{prefix => "/mounted",
                                                                                      security => false}})
                              end),
    ?assertEqual([{nova_test_app, secure, "/admin"}, {nova_test_sub_app, security, "/mounted"}],
                 lists:sort([{App, Key, Path}
                             || {warning, {report, #{app := App, key := Key, path := Path}}} <- Events])),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/admin/")),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/mounted/")).

pubsub_scope_is_restarted(_Config) ->
    _Port = nova_test_listener:start(#{}),
    Scope = pubsub_scope(),
    ?assert(is_pid(Scope)),
    Monitor = monitor(process, Scope),
    exit(Scope, kill),
    receive {'DOWN', Monitor, process, Scope, killed} -> ok end,
    ?assert(restarted(Scope, 50)),
    ok = nova_pubsub:join(restarted_channel),
    ?assertEqual([self()], nova_pubsub:get_members(restarted_channel)).

restarted_pubsub_scope_has_no_members(_Config) ->
    _Port = nova_test_listener:start(#{}),
    ok = nova_pubsub:join(emptied_channel),
    Scope = pubsub_scope(),
    Monitor = monitor(process, nova_scope),
    exit(Scope, kill),
    receive {'DOWN', Monitor, process, {nova_scope, _Node}, killed} -> ok end,
    ?assert(restarted(Scope, 50)),
    ?assertEqual([], nova_pubsub:get_members(emptied_channel)),
    ok = nova_pubsub:join(emptied_channel),
    ?assertEqual([self()], nova_pubsub:get_members(emptied_channel)).

pubsub_scope_started_elsewhere_is_left_alone(_Config) ->
    {ok, Scope} = pg:start(nova_scope),
    try
        _Port = nova_test_listener:start(#{}),
        ?assertEqual(undefined, pubsub_scope()),
        ok = nova_test_listener:stop(),
        ?assert(is_process_alive(Scope))
    after
        exit(Scope, kill)
    end.

%% An unset environment is prod wherever it is read: the routers are asked
%% for routes(prod), and nothing meant for development is on. A boot says so
%% once, for the developer who expected dev.
unset_environment_is_prod_and_said_once(_Config) ->
    ok = nova_test_listener:configure(#{}),
    application:set_env(nova_test_app, router_module, nova_boot_pages),
    application:unset_env(nova, environment),
    {{ok, _Started}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(nova_test_app) end),
    ?assertMatch([#{key := environment, read_as := prod}], environment_notices(Events)),
    ?assertEqual(prod, nova:get_environment()),
    ?assertNot(nova:is_dev_environment()),
    Port = maps:get(port, application:get_env(nova, cowboy_configuration, #{})),
    ?assertMatch({200, _}, nova_test_listener:get(Port, "/environment/prod")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/environment/dev")).

set_environment_is_not_mentioned(_Config) ->
    [begin
         ok = nova_test_listener:configure(#{}),
         application:set_env(nova_test_app, router_module, nova_boot_pages),
         application:set_env(nova, environment, Environment),
         {{ok, _Started}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(nova_test_app) end),
         ?assertEqual([], environment_notices(Events)),
         Port = maps:get(port, application:get_env(nova, cowboy_configuration, #{})),
         ?assertMatch({200, _}, nova_test_listener:get(Port, ["/environment/", atom_to_list(Environment)])),
         ok = nova_test_listener:stop()
     end || Environment <- [dev, prod]].

unknown_non_canonical_paths_fails_the_boot(_Config) ->
    ?assertEqual({invalid_setting, non_canonical_paths, allow}, stopped_by([{non_canonical_paths, allow}])).

unknown_trusted_origins_fails_the_boot(_Config) ->
    [?assertEqual({invalid_setting, trusted_origins, Value}, stopped_by([{trusted_origins, Value}]))
     || Value <- [none, ~"https://example.com", [same_host]]].

unknown_trusted_origins_of_a_plugin_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    [begin
         Group = #{prefix => "/forms", plugins => [{pre_request, Plugin, #{trusted_origins => same_hosts}}],
                   routes => Routes},
         {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Group]}) end),
         ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                  {invalid_setting, trusted_origins, same_hosts}}}, _}}},
                      Result),
         ?assertMatch([#{setting := trusted_origins, value := same_hosts, plugin := Plugin, app := nova_test_app,
                         prefix := "/forms"} | _],
                      nova_test_log:errors(Events)),
         ?assertEqual(undefined, whereis(nova_sup))
     end || Plugin <- [nova_csrf_plugin, nova_ws_origin_plugin]].

%% The option had the name of the setting, which was in no release.
allowed_origins_of_a_plugin_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    [begin
         Group = #{prefix => "/forms", plugins => [{pre_request, Plugin, #{allowed_origins => any}}],
                   routes => Routes},
         {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Group]}) end),
         ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                  {invalid_setting, allowed_origins,
                                                   {replaced_by, trusted_origins}}}}, _}}},
                      Result),
         ?assertMatch([#{setting := allowed_origins, replaced_by := trusted_origins, plugin := Plugin,
                         app := nova_test_app, prefix := "/forms"} | _],
                      nova_test_log:errors(Events)),
         ?assertEqual(undefined, whereis(nova_sup))
     end || Plugin <- [nova_csrf_plugin, nova_ws_origin_plugin]].

session_store_for_no_session_fails_the_boot(_Config) ->
    ?assertEqual({invalid_setting, max_sessions, 0}, stopped_by([{max_sessions, 0}])).

%% A name that is not a plugin would leave the order as it is, and the
%% security callback without the body its author listed the plugin for.
plugins_before_security_that_names_no_plugin_fails_the_boot(_Config) ->
    [?assertEqual({invalid_setting, plugins_before_security, Value}, stopped_by([{plugins_before_security, Value}]))
     || Value <- [[nova_reqeust_plugin], [all], nova_request_plugin]].

plugins_before_security_of_a_route_group_that_names_none_of_its_plugins_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    Request = {pre_request, nova_request_plugin, #{decode_json_body => true}},
    [begin
         Group = #{prefix => "/admin", plugins => [Request], plugins_before_security => Value, routes => Routes},
         {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Group]}) end),
         ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                  {invalid_setting, plugins_before_security, Value}}}, _}}},
                      Result),
         ?assertMatch([#{setting := plugins_before_security, value := Value, app := nova_test_app,
                         prefix := "/admin"} | _],
                      nova_test_log:errors(Events)),
         ?assertEqual(undefined, whereis(nova_sup))
     end || Value <- [[nova_reqeust_plugin], [all], [nova_csrf_plugin], nova_request_plugin]].

%% A setting that was in no release is not read any more. A configuration
%% that still has it is stopped and told which setting to use.
setting_that_is_gone_fails_the_boot(_Config) ->
    [?assertEqual({invalid_setting, Key, {replaced_by, Replacement}}, stopped_by([{Key, Value}]))
     || {Key, Value, Replacement} <- [{plugins_after_security, [nova_request_plugin], plugins_before_security},
                                      {allowed_origins, any, trusted_origins},
                                      {csrf_secret, binary:copy(~"s", 32), secret_key_base}]].

plugins_after_security_of_a_route_group_fails_the_boot(_Config) ->
    Group = #{prefix => "/admin", plugins => [{pre_request, nova_request_plugin, #{}}],
              plugins_after_security => [nova_request_plugin],
              routes => [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}]},
    {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Group]}) end),
    ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                             {invalid_setting, plugins_after_security,
                                              {replaced_by, plugins_before_security}}}}, _}}},
                 Result),
    ?assertMatch([#{setting := plugins_after_security, replaced_by := plugins_before_security,
                    app := nova_test_app, prefix := "/admin"} | _],
                 nova_test_log:errors(Events)),
    ?assertEqual(undefined, whereis(nova_sup)).

csrf_settings_nothing_can_go_by_fail_the_boot(_Config) ->
    [?assertEqual({Settings, Reason}, {Settings, stopped_by(Settings)})
     || {Settings, Reason} <- [{[{csrf_token, stateless}], {invalid_setting, secret_key_base, not_set}},
                               {[{csrf_token, signed}], {invalid_setting, csrf_token, signed}},
                               {[{secret_key_base, ~"short"}], {invalid_setting, secret_key_base, redacted}},
                               {[{csrf_token, session}, {secret_key_base, [~"short"]}],
                                {invalid_setting, secret_key_base, redacted}}]].

%% `[New | Old]` is what "put the new secret in front of the old one" looks
%% like to somebody who writes it as a cons. Neither that nor any other value
%% that is refused ends up in what the boot returns, logs or reports.
secret_that_fails_the_boot_is_not_told(_Config) ->
    Old = ~"the-secret-in-use-of-32-bytes-or-more",
    New = ~"the-secret-to-come-of-32-bytes-or-more",
    [begin
         application:set_env(nova, secret_key_base, Value),
         try
             {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{}) end),
             ?assertMatch({error, {nova, {{{invalid_setting, secret_key_base, redacted}, _Stacktrace}, _}}}, Result),
             ?assertEqual(undefined, whereis(nova_sup)),
             Told = iolist_to_binary(io_lib:format("~0p", [{Result, Events}])),
             [?assertEqual({Secret, nomatch}, {Secret, binary:match(Told, Secret)}) || Secret <- [Old, New]]
         after
             application:unset_env(nova, secret_key_base),
             nova_test_listener:stop()
         end
     end || Value <- [nova_test_helper:improper_list([New], Old), nova_test_helper:improper_list([New, Old], New),
                      {New, Old}, [[New, Old]], binary:part(Old, 0, 31)]].

%% The secret under the name it had before any release is in a boot that
%% stops, and under its own name in one that goes on. Neither says it.
secret_is_not_told_by_a_boot(_Config) ->
    Secret = ~"the-secret-in-use-of-32-bytes-or-more",
    Csrf = [#{plugins => [{pre_request, nova_csrf_plugin, #{}}],
              routes => [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}]}],
    [begin
         application:set_env(nova, Key, Secret),
         try
             {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => Csrf}) end),
             ?assertMatch({Key, {Outcome, _}}, {Key, Result}),
             Told = iolist_to_binary(io_lib:format("~0p", [{Result, Events}])),
             ?assertEqual({Key, nomatch}, {Key, binary:match(Told, Secret)})
         after
             application:unset_env(nova, Key),
             nova_test_listener:stop()
         end
     end || {Key, Outcome} <- [{csrf_secret, error}, {secret_key_base, ok}]].

%% The token of nova_csrf_plugin is kept in the session or signed for the
%% session cookie. Without sessions every request to its routes would fail.
csrf_plugin_without_sessions_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_boot_pages:page/1, #{methods => [get]}}],
    Csrf = #{prefix => "/forms", plugins => [{pre_request, nova_csrf_plugin, #{}}], routes => Routes},
    Secret = binary:copy(~"s", 32),
    [begin
         [application:set_env(nova, Key, Value) || {Key, Value} <- [{use_sessions, false} | Settings]],
         try
             {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Csrf]}) end),
             ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                      {invalid_setting, use_sessions,
                                                       {required_by, nova_csrf_plugin}}}}, _}}},
                          Result),
             ?assertMatch([#{setting := use_sessions, plugin := nova_csrf_plugin, app := nova_test_app,
                             prefix := "/forms"} | _],
                          nova_test_log:errors(Events)),
             ?assertEqual(undefined, whereis(nova_sup))
         after
             [application:unset_env(nova, Key) || {Key, _Value} <- [{use_sessions, false} | Settings]],
             nova_test_listener:stop()
         end
     end || Settings <- [[], [{csrf_token, session}], [{secret_key_base, Secret}]]],
    ?assertEqual({invalid_setting, csrf_token, {requires, use_sessions}},
                 stopped_by([{use_sessions, false}, {csrf_token, stateless}, {secret_key_base, Secret}])),
    application:set_env(nova, use_sessions, false),
    application:set_env(nova, secret_key_base, Secret),
    try
        Port = nova_test_listener:start(#{host => [#{prefix => "/pages", plugins => [], routes => Routes}]}),
        ?assertEqual({200, ~"page"}, nova_test_listener:get(Port, "/pages/"))
    after
        application:unset_env(nova, use_sessions),
        application:unset_env(nova, secret_key_base)
    end.

%% Without a secret the tokens of nova_csrf_plugin are kept in the session
%% store, as before there were signed ones. An application that has the
%% plugin is told once, unless it has said that this is what it wants.
csrf_tokens_kept_in_the_session_store_are_said_once(_Config) ->
    Csrf = [#{plugins => [{pre_request, nova_csrf_plugin, #{}}],
              routes => [{"/", fun nova_boot_pages:page/1, #{methods => [get]}},
                         {"/other", fun nova_boot_pages:page/1, #{methods => [get]}}]}],
    Secret = binary:copy(~"s", 32),
    [begin
         [application:set_env(nova, Key, Value) || {Key, Value} <- Settings],
         try
             {{ok, _Port}, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => Routes}) end),
             ?assertEqual({Settings, Notices},
                          {Settings, length([Report || {notice, {report, #{key := secret_key_base} = Report}} <- Events])})
         after
             application:unset_env(nova, csrf_token),
             application:unset_env(nova, secret_key_base),
             nova_test_listener:stop()
         end
     end || {Routes, Settings, Notices} <- [{Csrf, [], 1},
                                            {Csrf, [{csrf_token, session}], 0},
                                            {Csrf, [{secret_key_base, Secret}], 0},
                                            {Csrf, [{csrf_token, stateless}, {secret_key_base, Secret}], 0},
                                            {[], [], 0}]].

body_decoded_behind_a_security_callback_is_warned_about(_Config) ->
    Group = fun(Prefix, More) ->
                    More#{prefix => Prefix, security => fun nova_boot_pages:allow/1,
                          plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],
                          routes => [{"/", fun nova_boot_pages:page/1, #{methods => [post]}}]}
            end,
    Routes = [Group("/reads", #{}), Group("/listed", #{plugins_before_security => [nova_request_plugin]})],
    try
        {{ok, _Port}, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => Routes}) end),
        ?assertMatch([#{app := nova_test_app, prefix := "/reads", plugins := [nova_request_plugin],
                        router_file := _}],
                     [Report || {warning, {report, #{plugins := _} = Report}} <- Events])
    after
        nova_once:forget({nova_router, body_behind_security, nova_test_app, "/reads", [nova_request_plugin]})
    end.

%% A callback that is said to read the body, in a route group that decodes
%% it behind the callback, would find none: the node does not start. The
%% same is true of a value that says neither.
security_reads_body_of_a_route_group_is_checked_by_the_boot(_Config) ->
    Group = fun(Value) ->
                    #{prefix => "/reads", security => fun nova_boot_pages:allow/1, security_reads_body => Value,
                      plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],
                      routes => [{"/", fun nova_boot_pages:page/1, #{methods => [post]}}]}
            end,
    [begin
         {Result, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Group(Value)]}) end),
         ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                  {invalid_setting, security_reads_body, Reason}}}, _}}},
                      Result),
         ?assertMatch([#{setting := security_reads_body, value := Reason, app := nova_test_app,
                         prefix := "/reads"} | _],
                      nova_test_log:errors(Events)),
         ?assertEqual(undefined, whereis(nova_sup))
     end || {Value, Reason} <- [{true, {requires, plugins_before_security}}, {sometimes, sometimes}]],
    {{ok, _Port}, Events} = nova_test_log:collect(fun() -> nova_test_listener:try_start(#{host => [Group(false)]}) end),
    ?assertEqual([], [Report || {warning, {report, #{plugins := _} = Report}} <- Events]).

%% nova_plugin_manager starts every plugin the router has listed, also after
%% a restart, so the list must not carry over from a run with other routes.
plugins_of_an_earlier_run_are_forgotten(_Config) ->
    Routes = [#{prefix => "/earlier",
                plugins => [{pre_request, nova_plugin_once_pages, #{name => earlier}}],
                routes => []}],
    _ = nova_test_listener:start(#{host => Routes}),
    ?assert(lists:member(nova_plugin_once_pages, nova_router:plugins())),
    ok = nova_test_listener:stop(),
    _ = nova_test_listener:start(#{}),
    ?assertNot(lists:member(nova_plugin_once_pages, nova_router:plugins())).

%% What a boot with Settings in the nova environment is stopped with, which
%% is before anything is started.
stopped_by(Settings) ->
    [application:set_env(nova, Key, Value) || {Key, Value} <- Settings],
    try
        {error, {nova, {{Reason, _Stacktrace}, _Start}}} = nova_test_listener:try_start(#{}),
        undefined = whereis(nova_sup),
        Reason
    after
        [application:unset_env(nova, Key) || {Key, _Value} <- Settings],
        nova_test_listener:stop()
    end.

environment_notices(Events) ->
    [Report || {_Level, {report, #{read_as := _} = Report}} <- Events].

running(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> Pid
    end.

pubsub_scope() ->
    {nova_pubsub_scope, Pid, worker, [pg]} = lists:keyfind(nova_pubsub_scope, 1, supervisor:which_children(nova_sup)),
    Pid.

stopped(_Application, 0) ->
    false;
stopped(Application, Attempts) ->
    case lists:keymember(Application, 1, application:which_applications()) of
        false ->
            true;
        true ->
            timer:sleep(100),
            stopped(Application, Attempts - 1)
    end.

restarted(_Old, 0) ->
    false;
restarted(Old, Attempts) ->
    case whereis(nova_sup) =/= undefined andalso pubsub_scope() of
        New when is_pid(New), New =/= Old ->
            true;
        _NotYet ->
            timer:sleep(100),
            restarted(Old, Attempts - 1)
    end.
