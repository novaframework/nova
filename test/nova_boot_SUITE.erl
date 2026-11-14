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
         pubsub_scope_is_restarted/1,
         restarted_pubsub_scope_has_no_members/1,
         pubsub_scope_started_elsewhere_is_left_alone/1,
         unset_environment_is_warned_about_once/1,
         set_environment_is_not_warned_about/1,
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
     pubsub_scope_is_restarted,
     restarted_pubsub_scope_has_no_members,
     pubsub_scope_started_elsewhere_is_left_alone,
     unset_environment_is_warned_about_once,
     set_environment_is_not_warned_about,
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

%% An unset environment is read as dev where routes are compiled, which a
%% release that forgot the key should hear about.
unset_environment_is_warned_about_once(_Config) ->
    ok = nova_test_listener:configure(#{}),
    application:unset_env(nova, environment),
    {{ok, _Started}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(nova_test_app) end),
    ?assertMatch([#{environment := undefined, read_as := dev}], environment_warnings(Events)).

set_environment_is_not_warned_about(_Config) ->
    [begin
         ok = nova_test_listener:configure(#{}),
         application:set_env(nova, environment, Environment),
         {{ok, _Started}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(nova_test_app) end),
         ?assertEqual([], environment_warnings(Events)),
         ok = nova_test_listener:stop()
     end || Environment <- [dev, prod]].

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

environment_warnings(Events) ->
    [Report || {warning, {report, #{environment := _} = Report}} <- Events].

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
