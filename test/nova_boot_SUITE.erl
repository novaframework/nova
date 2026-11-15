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
         unknown_allowed_origins_fails_the_boot/1,
         session_store_for_no_session_fails_the_boot/1,
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
     unknown_allowed_origins_fails_the_boot,
     session_store_for_no_session_fails_the_boot,
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
    application:set_env(nova, non_canonical_paths, allow),
    try
        ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                 {invalid_non_canonical_paths, allow}}}, _}}},
                     nova_test_listener:try_start(#{})),
        ?assertEqual(undefined, whereis(nova_sup))
    after
        application:unset_env(nova, non_canonical_paths)
    end.

unknown_allowed_origins_fails_the_boot(_Config) ->
    [begin
         application:set_env(nova, allowed_origins, Value),
         try
             ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, nova_listener,
                                                      {invalid_allowed_origins, Value}}}, _}}},
                          nova_test_listener:try_start(#{})),
             ?assertEqual(undefined, whereis(nova_sup))
         after
             application:unset_env(nova, allowed_origins),
             nova_test_listener:stop()
         end
     end || Value <- [none, ~"https://example.com", [same_host]]].

session_store_for_no_session_fails_the_boot(_Config) ->
    application:set_env(nova, max_sessions, 0),
    try
        ?assertMatch({error, {nova, {{shutdown, {failed_to_start_child, _Child, {invalid_max_sessions, 0}}}, _}}},
                     nova_test_listener:try_start(#{})),
        ?assertEqual(undefined, whereis(nova_sup))
    after
        application:unset_env(nova, max_sessions)
    end.

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
