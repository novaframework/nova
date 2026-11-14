%%% Nova either starts with its listener serving requests or does not start.
-module(nova_boot_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         port_in_use_fails_the_boot/1,
         listener_starts_after_the_processes_it_needs/1
        ]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     port_in_use_fails_the_boot,
     listener_starts_after_the_processes_it_needs
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
