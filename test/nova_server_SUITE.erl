%%% A server other than the default one.
%%%
%%% Boots the bootstrap application with nova_recording_server as Nova's
%%% `server'. That server runs on Cowboy and notes each listener call, so
%%% this checks that Nova starts, stops and drains listeners through the
%%% server it is given, with the listener's address resolved, at startup, for
%%% an application added and removed at runtime, and at shutdown.
-module(nova_server_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         bootstrap_listener_starts_on_the_server/1,
         runtime_application_starts_and_stops_on_the_server/1,
         shutdown_drains_and_stops_through_the_server/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     bootstrap_listener_starts_on_the_server,
     runtime_application_starts_and_stops_on_the_server,
     shutdown_drains_and_stops_through_the_server
    ].

init_per_suite(Config) ->
    %% The table outlives this process, which ends after init_per_suite.
    Self = self(),
    Owner = spawn(fun() ->
                          nova_recording_server = ets:new(nova_recording_server,
                                                          [named_table, public, ordered_set]),
                          Self ! {table_ready, self()},
                          receive stop -> ok end
                  end),
    receive {table_ready, Owner} -> ok end,
    Port = free_port(),
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, server, nova_recording_server),
    application:set_env(nova, server_configuration, #{port => Port}),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    application:set_env(nova, shutdown_drain_timeout, 1000),
    {ok, _Started} = application:ensure_all_started(nova_test_app),
    {ok, _} = application:ensure_all_started(inets),
    [{port, Port}, {owner, Owner} | Config].

end_per_suite(Config) ->
    application:stop(nova_test_app),
    application:stop(nova),
    application:unset_env(nova, server),
    application:unset_env(nova, server_configuration),
    application:unset_env(nova, shutdown_drain_timeout),
    ?config(owner, Config) ! stop,
    ok.

%%====================================================================
%% Cases
%%====================================================================

bootstrap_listener_starts_on_the_server(Config) ->
    Port = ?config(port, Config),
    ?assertMatch([{start_listener, nova_listener, #{ip := {0, 0, 0, 0}, port := Port}}],
                 calls(start_listener)),
    {ok, {{_, 200, _}, _, _}} =
        httpc:request("http://localhost:" ++ integer_to_list(Port) ++ "/"),
    ok.

runtime_application_starts_and_stops_on_the_server(_Config) ->
    Port = free_port(),
    {ok, nova_test_sub_app, _, Port} = nova_sup:add_application(nova_test_sub_app, #{port => Port}),
    Ref = {nova_listener, nova_test_sub_app, Port},
    ?assertMatch([_, {start_listener, Ref, #{port := Port}}], calls(start_listener)),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ?assertEqual([{stop_listener, Ref}], calls(stop_listener)).

shutdown_drains_and_stops_through_the_server(_Config) ->
    ok = application:stop(nova),
    ?assertEqual([{drain_listeners, [nova_listener], 1000}], calls(drain_listeners)),
    ?assertMatch([_, {stop_listener, nova_listener}], calls(stop_listener)).

%%====================================================================
%% Helpers
%%====================================================================

%% The recorded calls of one kind, oldest first.
calls(Kind) ->
    [Call || {_, Call} <- ets:tab2list(nova_recording_server), element(1, Call) =:= Kind].

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.
