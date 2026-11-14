%%% Nova either starts with its listener serving requests or does not start.
-module(nova_boot_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         port_in_use_fails_the_boot/1,
         listener_starts_after_the_processes_it_needs/1,
         pubsub_scope_is_restarted/1,
         pubsub_scope_started_elsewhere_is_left_alone/1,
         unset_environment_is_warned_about_once/1,
         set_environment_is_not_warned_about/1
        ]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     port_in_use_fails_the_boot,
     listener_starts_after_the_processes_it_needs,
     pubsub_scope_is_restarted,
     pubsub_scope_started_elsewhere_is_left_alone,
     unset_environment_is_warned_about_once,
     set_environment_is_not_warned_about
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

environment_warnings(Events) ->
    [Report || {warning, {report, #{environment := _} = Report}} <- Events].

pubsub_scope() ->
    {nova_pubsub_scope, Pid, worker, [pg]} = lists:keyfind(nova_pubsub_scope, 1, supervisor:which_children(nova_sup)),
    Pid.

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
