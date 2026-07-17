-module(nova_pubsub_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_pubsub.hrl").

setup() ->
    ?assertEqual(ok, nova_pubsub:start()),
    ok.

cleanup(_) ->
    ok.

nova_pubsub_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun join_and_get_members/0,
      fun join_with_pid_returns_ok/0,
      fun leave_removes_member/0,
      fun leave_with_pid_returns_ok/0,
      fun broadcast_sends_to_all_and_returns_ok/0,
      fun local_broadcast_sends_to_local_and_returns_ok/0,
      fun empty_channel_returns_empty/0,
      fun multiple_processes_same_channel/0,
      fun envelope_structure/0,
      fun start_returns_ok/0
     ]}.

join_and_get_members() ->
    Channel = make_channel(),
    ?assertEqual(ok, nova_pubsub:join(Channel)),
    ?assert(lists:member(self(), nova_pubsub:get_members(Channel))),
    ?assert(lists:member(self(), nova_pubsub:get_local_members(Channel))),
    nova_pubsub:leave(Channel).

join_with_pid_returns_ok() ->
    Channel = make_channel(),
    Pid = spawn_link(fun() -> receive stop -> ok end end),
    ?assertEqual(ok, nova_pubsub:join(Channel, Pid)),
    ?assert(lists:member(Pid, nova_pubsub:get_members(Channel))),
    nova_pubsub:leave(Channel, Pid),
    Pid ! stop.

leave_removes_member() ->
    Channel = make_channel(),
    nova_pubsub:join(Channel),
    ?assert(lists:member(self(), nova_pubsub:get_members(Channel))),
    ?assertEqual(ok, nova_pubsub:leave(Channel)),
    ?assertNot(lists:member(self(), nova_pubsub:get_members(Channel))).

leave_with_pid_returns_ok() ->
    Channel = make_channel(),
    Pid = spawn_link(fun() -> receive stop -> ok end end),
    nova_pubsub:join(Channel, Pid),
    ?assertEqual(ok, nova_pubsub:leave(Channel, Pid)),
    ?assertNot(lists:member(Pid, nova_pubsub:get_members(Channel))),
    Pid ! stop.

broadcast_sends_to_all_and_returns_ok() ->
    Channel = make_channel(),
    Pid1 = spawn_member(Channel),
    Pid2 = spawn_member(Channel),
    nova_pubsub:join(Channel),
    ?assertEqual(ok, nova_pubsub:broadcast(Channel, <<"topic1">>, hello)),
    ?assertMatch(#nova_pubsub{channel = Channel, topic = <<"topic1">>, payload = hello}, recv()),
    Pid1 ! {get, self()},
    ?assertMatch(#nova_pubsub{channel = Channel, topic = <<"topic1">>, payload = hello}, recv()),
    Pid2 ! {get, self()},
    ?assertMatch(#nova_pubsub{channel = Channel, topic = <<"topic1">>, payload = hello}, recv()),
    cleanup_pids([Pid1, Pid2]),
    nova_pubsub:leave(Channel).

local_broadcast_sends_to_local_and_returns_ok() ->
    Channel = make_channel(),
    Pid = spawn_member(Channel),
    nova_pubsub:join(Channel),
    ?assertEqual(ok, nova_pubsub:local_broadcast(Channel, <<"local_topic">>, world)),
    ?assertMatch(#nova_pubsub{channel = Channel, topic = <<"local_topic">>, payload = world}, recv()),
    Pid ! {get, self()},
    ?assertMatch(#nova_pubsub{channel = Channel, topic = <<"local_topic">>, payload = world}, recv()),
    cleanup_pids([Pid]),
    nova_pubsub:leave(Channel).

empty_channel_returns_empty() ->
    Channel = make_channel(),
    ?assertEqual([], nova_pubsub:get_members(Channel)),
    ?assertEqual([], nova_pubsub:get_local_members(Channel)).

multiple_processes_same_channel() ->
    Channel = make_channel(),
    Pid1 = spawn_member(Channel),
    Pid2 = spawn_member(Channel),
    Pid3 = spawn_member(Channel),
    Members = nova_pubsub:get_members(Channel),
    ?assert(lists:member(Pid1, Members)),
    ?assert(lists:member(Pid2, Members)),
    ?assert(lists:member(Pid3, Members)),
    ?assertEqual(3, length(Members)),
    cleanup_pids([Pid1, Pid2, Pid3]).

envelope_structure() ->
    Channel = make_channel(),
    nova_pubsub:join(Channel),
    nova_pubsub:broadcast(Channel, "test_topic", #{data => 42}),
    Msg = recv(),
    ?assertMatch(#nova_pubsub{}, Msg),
    ?assertEqual(Channel, Msg#nova_pubsub.channel),
    ?assertEqual(self(), Msg#nova_pubsub.sender),
    ?assertEqual("test_topic", Msg#nova_pubsub.topic),
    ?assertEqual(#{data => 42}, Msg#nova_pubsub.payload),
    nova_pubsub:leave(Channel).

start_returns_ok() ->
    ?assertEqual(ok, nova_pubsub:start()).

%% Helpers

make_channel() ->
    list_to_atom("test_ch_" ++ integer_to_list(erlang:unique_integer([positive]))).

spawn_member(Channel) ->
    Parent = self(),
    Pid = spawn(fun() ->
        ok = nova_pubsub:join(Channel),
        Parent ! {joined, self()},
        member_loop([])
    end),
    receive {joined, Pid} -> ok after 1000 -> error(timeout) end,
    Pid.

member_loop(Buf) ->
    receive
        {get, From} ->
            case Buf of
                [Msg | Rest] ->
                    From ! Msg,
                    member_loop(Rest);
                [] ->
                    From ! no_message,
                    member_loop([])
            end;
        stop ->
            ok;
        Other ->
            member_loop(Buf ++ [Other])
    end.

recv() ->
    receive Msg -> Msg after 1000 -> error(timeout) end.

cleanup_pids(Pids) ->
    [P ! stop || P <- Pids].
