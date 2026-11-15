-module(nova_session_ets_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_pubsub.hrl").

%%====================================================================
%% Direct tests for the nova_session_ets gen_server backend.
%%
%% Starts pg + nova_session_ets, exercises get_value/set_value/delete_value
%% directly through the API and verifies ETS state via gen_server:call.
%%====================================================================

%%--------------------------------------------------------------------
%% Setup / Cleanup
%%--------------------------------------------------------------------

setup() ->
    pg:start_link(nova_scope),
    {ok, Pid} = nova_session_ets:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    ok.

sync() ->
    gen_server:call(nova_session_ets, sync).

%%====================================================================
%% Test generators
%%====================================================================

nova_session_ets_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun get_value_missing_session/0,
      fun set_and_get_value/0,
      fun set_creates_new_session/0,
      fun set_updates_existing_session/0,
      fun get_value_missing_key/0,
      fun delete_value_whole_session/0,
      fun delete_value_single_key/0,
      fun delete_value_key_from_missing_session/0,
      fun delete_value_missing_session/0,
      fun overwrite_key/0,
      fun multiple_sessions_isolated/0,
      fun unknown_call_returns_ok/0,
      fun unknown_cast_is_noreply/0,
      fun unknown_info_is_noreply/0,
      fun format_status_passthrough/0,
      fun code_change_converts_old_rows/0,
      fun terminate_returns_ok/0,
      fun write_is_visible_without_sync/0,
      fun session_exists_after_write/0,
      fun session_exists_false_after_delete/0
     ]}.

expiry_test_() ->
    {foreach,
     fun() -> application:set_env(nova, session_max_age, 1), setup() end,
     fun(Pid) -> cleanup(Pid), application:unset_env(nova, session_max_age) end,
     [
      fun session_expires_after_max_age/0,
      fun update_does_not_extend_max_age/0
     ]}.

cap_test_() ->
    {foreach,
     fun() -> nova_test_settings:set(max_sessions, 10), setup() end,
     fun(Pid) ->
             cleanup(Pid),
             nova_test_settings:unset(max_sessions),
             application:unset_env(nova, session_max_age)
     end,
     [
      fun new_session_at_the_cap_replaces_the_one_nearest_its_expiry/0,
      fun existing_session_is_written_at_the_cap/0,
      fun deleted_session_makes_room/0,
      fun expired_session_makes_room/0,
      fun cap_can_be_turned_off/0,
      fun provisional_session_goes_first/0,
      fun provisional_session_replaces_the_provisional_one_nearest_its_expiry/0,
      fun provisional_session_is_refused_when_every_session_is_kept/0,
      fun written_provisional_session_is_kept/0,
      fun provisional_write_leaves_a_session_kept/0,
      fun replicated_session_does_not_grow_the_table_past_the_cap/0,
      fun replicated_provisional_session_is_dropped_when_every_session_is_kept/0,
      fun cap_is_the_one_nova_started_with/0
     ]}.

provisional_test_() ->
    {foreach,
     fun setup/0,
     fun(Pid) -> cleanup(Pid), application:unset_env(nova, session_max_age) end,
     [
      fun provisional_session_lives_a_day_at_most/0,
      fun provisional_session_lives_no_longer_than_any_other/0,
      fun written_provisional_session_lives_as_long_as_any_other/0,
      fun every_session_is_in_the_expiry_index_once/0
     ]}.

reads_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun get_value_does_not_wait_for_the_server/0
     ]}.

get_value_without_the_server_test() ->
    ?assertEqual(undefined, whereis(nova_session_ets)),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(~"s", ~"k")).

fill(Count) ->
    [ok = nova_session_ets:set_value(integer_to_binary(N), ~"k", ~"v") || N <- lists:seq(1, Count)],
    ok.

%% The first session written is the one nearest its expiry.
new_session_at_the_cap_replaces_the_one_nearest_its_expiry() ->
    fill(10),
    ?assertEqual(ok, nova_session_ets:set_value(~"eleventh", ~"k", ~"v")),
    ?assert(nova_session_ets:session_exists(~"eleventh")),
    ?assertNot(nova_session_ets:session_exists(~"1")),
    ?assertEqual(10, ets:info(nova_session_ets_entries, size)).

existing_session_is_written_at_the_cap() ->
    fill(10),
    ?assertEqual(ok, nova_session_ets:set_value(~"3", ~"other", ~"value")),
    ?assertEqual({ok, ~"value"}, nova_session_ets:get_value(~"3", ~"other")).

deleted_session_makes_room() ->
    fill(10),
    ok = nova_session_ets:delete_value(~"3"),
    ?assertEqual(ok, nova_session_ets:set_value(~"eleventh", ~"k", ~"v")),
    ?assert(nova_session_ets:session_exists(~"1")).

%% An expired session goes before one that is in use. One is dropped for
%% each new session, and the sweep drops the others.
expired_session_makes_room() ->
    application:set_env(nova, session_max_age, 1),
    fill(9),
    timer:sleep(1100),
    application:set_env(nova, session_max_age, 3600),
    ok = nova_session_ets:set_provisional_value(~"visitor", ~"k", ~"v"),
    ?assertEqual(ok, nova_session_ets:set_value(~"eleventh", ~"k", ~"v")),
    ?assert(nova_session_ets:session_exists(~"visitor")),
    ?assertEqual(10, ets:info(nova_session_ets_entries, size)),
    whereis(nova_session_ets) ! sweep,
    sync(),
    ?assertEqual(2, ets:info(nova_session_ets_entries, size)).

cap_can_be_turned_off() ->
    nova_test_settings:set(max_sessions, infinity),
    ?assertEqual(ok, fill(25)).

kept() ->
    [integer_to_binary(N) || N <- lists:seq(1, 10), nova_session_ets:session_exists(integer_to_binary(N))].

%% A session that only holds what every visitor gets makes room for one
%% that is in use.
provisional_session_goes_first() ->
    fill(9),
    ok = nova_session_ets:set_provisional_value(~"visitor", ~"k", ~"v"),
    ?assertEqual(ok, nova_session_ets:set_value(~"10", ~"k", ~"v")),
    ?assertNot(nova_session_ets:session_exists(~"visitor")),
    ?assertEqual(10, length(kept())).

provisional_session_replaces_the_provisional_one_nearest_its_expiry() ->
    fill(8),
    ok = nova_session_ets:set_provisional_value(~"first", ~"k", ~"v"),
    timer:sleep(2),
    ok = nova_session_ets:set_provisional_value(~"second", ~"k", ~"v"),
    ?assertEqual(ok, nova_session_ets:set_provisional_value(~"third", ~"k", ~"v")),
    ?assertEqual([false, true, true],
                 [nova_session_ets:session_exists(Id) || Id <- [~"first", ~"second", ~"third"]]),
    ?assertEqual(8, length(kept())).

provisional_session_is_refused_when_every_session_is_kept() ->
    fill(10),
    ?assertEqual({error, session_store_full}, nova_session_ets:set_provisional_value(~"visitor", ~"k", ~"v")),
    ?assertNot(nova_session_ets:session_exists(~"visitor")),
    ?assertEqual(10, length(kept())).

%% A write with set_value/3 is what tells a session that is in use from one
%% that is not.
written_provisional_session_is_kept() ->
    ok = nova_session_ets:set_provisional_value(~"visitor", ~"token", ~"t"),
    ok = nova_session_ets:set_value(~"visitor", ~"user", ~"alice"),
    [ok = nova_session_ets:set_provisional_value(integer_to_binary(N), ~"k", ~"v") || N <- lists:seq(1, 30)],
    ?assertEqual({ok, ~"alice"}, nova_session_ets:get_value(~"visitor", ~"user")),
    ?assertEqual({ok, ~"t"}, nova_session_ets:get_value(~"visitor", ~"token")),
    ?assertEqual(10, ets:info(nova_session_ets_entries, size)).

provisional_write_leaves_a_session_kept() ->
    ok = nova_session_ets:set_value(~"user", ~"user", ~"alice"),
    ok = nova_session_ets:set_provisional_value(~"user", ~"token", ~"t"),
    [ok = nova_session_ets:set_provisional_value(integer_to_binary(N), ~"k", ~"v") || N <- lists:seq(1, 30)],
    ?assertEqual({ok, ~"alice"}, nova_session_ets:get_value(~"user", ~"user")),
    ?assertEqual({ok, ~"t"}, nova_session_ets:get_value(~"user", ~"token")).

%% What another node stores arrives without the check the writing node made.
replicated_session_does_not_grow_the_table_past_the_cap() ->
    fill(10),
    [whereis(nova_session_ets) ! #nova_pubsub{channel = '__sessions', sender = self(), topic = "set_value",
                                              payload = {<<"replicated", N>>, ~"k", ~"v"}}
     || N <- lists:seq(1, 5)],
    sync(),
    ?assertEqual(10, ets:info(nova_session_ets_entries, size)),
    ?assert(nova_session_ets:session_exists(<<"replicated", 5>>)).

replicated_provisional_session_is_dropped_when_every_session_is_kept() ->
    fill(10),
    whereis(nova_session_ets) ! #nova_pubsub{channel = '__sessions', sender = self(),
                                             topic = "set_provisional_value", payload = {~"visitor", ~"k", ~"v"}},
    sync(),
    ?assertNot(nova_session_ets:session_exists(~"visitor")),
    ?assertEqual(10, length(kept())).

%% The setting is checked and kept when Nova starts. What is put in the
%% environment later is not what the store goes by.
cap_is_the_one_nova_started_with() ->
    application:set_env(nova, max_sessions, 0),
    fill(12),
    ?assertEqual(10, ets:info(nova_session_ets_entries, size)).

expires_in(SessionId) ->
    [Row] = ets:lookup(nova_session_ets_entries, SessionId),
    (element(3, Row) - erlang:system_time(millisecond)) div 1000.

provisional_session_lives_a_day_at_most() ->
    ok = nova_session_ets:set_provisional_value(~"visitor", ~"k", ~"v"),
    ok = nova_session_ets:set_value(~"user", ~"k", ~"v"),
    ?assert(expires_in(~"visitor") =< 86400),
    ?assert(expires_in(~"visitor") > 86000),
    ?assert(expires_in(~"user") > 1209000),
    application:set_env(nova, session_max_age, infinity),
    ok = nova_session_ets:set_provisional_value(~"later", ~"k", ~"v"),
    ?assert(expires_in(~"later") =< 86400).

provisional_session_lives_no_longer_than_any_other() ->
    application:set_env(nova, session_max_age, 60),
    ok = nova_session_ets:set_provisional_value(~"visitor", ~"k", ~"v"),
    ?assert(expires_in(~"visitor") =< 60).

written_provisional_session_lives_as_long_as_any_other() ->
    ok = nova_session_ets:set_provisional_value(~"visitor", ~"k", ~"v"),
    ok = nova_session_ets:set_value(~"visitor", ~"user", ~"alice"),
    ?assert(expires_in(~"visitor") > 1209000).

%% The index is what a full table is cleared by, so a session that is missing
%% from it would never make room and one that is there twice would be
%% dropped for another.
every_session_is_in_the_expiry_index_once() ->
    ok = nova_session_ets:set_provisional_value(~"a", ~"k", ~"v"),
    ok = nova_session_ets:set_provisional_value(~"a", ~"k2", ~"v"),
    ok = nova_session_ets:set_value(~"a", ~"user", ~"alice"),
    ok = nova_session_ets:set_value(~"b", ~"k", ~"v"),
    ok = nova_session_ets:set_provisional_value(~"b", ~"k2", ~"v"),
    ok = nova_session_ets:delete_value(~"b", ~"k"),
    ok = nova_session_ets:set_provisional_value(~"c", ~"k", ~"v"),
    ok = nova_session_ets:set_value(~"d", ~"k", ~"v"),
    ok = nova_session_ets:delete_value(~"d"),
    ok = nova_session_ets:delete_value(~"missing"),
    ?assertEqual([~"a", ~"b", ~"c"],
                 lists:sort([Id || {{_Rank, _ExpiresAt, Id}} <- ets:tab2list(nova_session_ets_expiry)])),
    ?assertEqual(3, ets:info(nova_session_ets_entries, size)).

%% Reads go to the table, so they do not queue behind the writes.
get_value_does_not_wait_for_the_server() ->
    ok = nova_session_ets:set_value(~"s", ~"k", ~"v"),
    ok = sys:suspend(nova_session_ets),
    try
        ?assertEqual({ok, ~"v"}, nova_session_ets:get_value(~"s", ~"k")),
        ?assertEqual({error, not_found}, nova_session_ets:get_value(~"s", ~"missing")),
        ?assertEqual({error, not_found}, nova_session_ets:get_value(~"nobody", ~"k"))
    after
        sys:resume(nova_session_ets)
    end.

%% A request that follows the response must find what it stored.
write_is_visible_without_sync() ->
    ok = nova_session_ets:set_value(~"s", ~"k", ~"v"),
    ?assertEqual({ok, ~"v"}, nova_session_ets:get_value(~"s", ~"k")),
    ?assert(nova_session_ets:session_exists(~"s")).

session_exists_after_write() ->
    ?assertNot(nova_session_ets:session_exists(~"never-issued")),
    ok = nova_session_ets:set_value(~"issued", ~"k", ~"v"),
    ?assert(nova_session_ets:session_exists(~"issued")).

session_exists_false_after_delete() ->
    ok = nova_session_ets:set_value(~"gone", ~"k", ~"v"),
    ok = nova_session_ets:delete_value(~"gone"),
    ?assertNot(nova_session_ets:session_exists(~"gone")).

session_expires_after_max_age() ->
    ok = nova_session_ets:set_value(~"old", ~"k", ~"v"),
    ?assert(nova_session_ets:session_exists(~"old")),
    timer:sleep(1100),
    ?assertNot(nova_session_ets:session_exists(~"old")),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(~"old", ~"k")),
    Pid = whereis(nova_session_ets),
    Pid ! sweep,
    sync(),
    ?assertEqual([], ets:lookup(nova_session_ets_entries, ~"old")).

update_does_not_extend_max_age() ->
    ok = nova_session_ets:set_value(~"abs", ~"k", ~"v"),
    timer:sleep(400),
    ok = nova_session_ets:set_value(~"abs", ~"k2", ~"v2"),
    timer:sleep(800),
    ?assertNot(nova_session_ets:session_exists(~"abs")).

%%====================================================================
%% Tests
%%====================================================================

get_value_missing_session() ->
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"nosuch">>, <<"key">>)).

set_and_get_value() ->
    ok = nova_session_ets:set_value(<<"s1">>, <<"k">>, <<"v">>),
    sync(),
    ?assertEqual({ok, <<"v">>}, nova_session_ets:get_value(<<"s1">>, <<"k">>)).

set_creates_new_session() ->
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"new">>, <<"k">>)),
    ok = nova_session_ets:set_value(<<"new">>, <<"k">>, <<"val">>),
    sync(),
    ?assertEqual({ok, <<"val">>}, nova_session_ets:get_value(<<"new">>, <<"k">>)).

set_updates_existing_session() ->
    ok = nova_session_ets:set_value(<<"s2">>, <<"a">>, <<"1">>),
    sync(),
    ok = nova_session_ets:set_value(<<"s2">>, <<"b">>, <<"2">>),
    sync(),
    ?assertEqual({ok, <<"1">>}, nova_session_ets:get_value(<<"s2">>, <<"a">>)),
    ?assertEqual({ok, <<"2">>}, nova_session_ets:get_value(<<"s2">>, <<"b">>)).

get_value_missing_key() ->
    ok = nova_session_ets:set_value(<<"s3">>, <<"exists">>, <<"yes">>),
    sync(),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"s3">>, <<"nope">>)).

delete_value_whole_session() ->
    ok = nova_session_ets:set_value(<<"del1">>, <<"k">>, <<"v">>),
    sync(),
    ok = nova_session_ets:delete_value(<<"del1">>),
    sync(),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"del1">>, <<"k">>)).

delete_value_single_key() ->
    ok = nova_session_ets:set_value(<<"del2">>, <<"keep">>, <<"a">>),
    ok = nova_session_ets:set_value(<<"del2">>, <<"remove">>, <<"b">>),
    sync(),
    ok = nova_session_ets:delete_value(<<"del2">>, <<"remove">>),
    sync(),
    ?assertEqual({ok, <<"a">>}, nova_session_ets:get_value(<<"del2">>, <<"keep">>)),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"del2">>, <<"remove">>)).

delete_value_key_from_missing_session() ->
    ok = nova_session_ets:delete_value(<<"ghost">>, <<"key">>),
    sync(),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"ghost">>, <<"key">>)).

delete_value_missing_session() ->
    ok = nova_session_ets:delete_value(<<"ghost">>),
    sync(),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(<<"ghost">>, <<"any">>)).

overwrite_key() ->
    ok = nova_session_ets:set_value(<<"ow">>, <<"k">>, <<"old">>),
    sync(),
    ok = nova_session_ets:set_value(<<"ow">>, <<"k">>, <<"new">>),
    sync(),
    ?assertEqual({ok, <<"new">>}, nova_session_ets:get_value(<<"ow">>, <<"k">>)).

multiple_sessions_isolated() ->
    ok = nova_session_ets:set_value(<<"iso1">>, <<"k">>, <<"v1">>),
    ok = nova_session_ets:set_value(<<"iso2">>, <<"k">>, <<"v2">>),
    sync(),
    ?assertEqual({ok, <<"v1">>}, nova_session_ets:get_value(<<"iso1">>, <<"k">>)),
    ?assertEqual({ok, <<"v2">>}, nova_session_ets:get_value(<<"iso2">>, <<"k">>)).

unknown_call_returns_ok() ->
    ?assertEqual(ok, gen_server:call(nova_session_ets, some_random_call)).

unknown_cast_is_noreply() ->
    ok = gen_server:cast(nova_session_ets, some_random_cast),
    sync(),
    ok.

unknown_info_is_noreply() ->
    nova_session_ets ! some_random_message,
    sync(),
    ok.

format_status_passthrough() ->
    Status = #{state => test, log => [], message => undefined, reason => undefined},
    ?assertEqual(Status, nova_session_ets:format_status(Status)).

%% Older releases stored {SessionId, Data} and then {SessionId, Data,
%% ExpiresAt}; a hot upgrade converts the rows of both and leaves the ones it
%% already converted alone.
code_change_converts_old_rows() ->
    Pid = nova_session_ets,
    Started = whereis(Pid),
    Soon = erlang:system_time(millisecond) + 60000,
    Past = erlang:system_time(millisecond) - 1,
    ok = nova_session_ets:set_value(~"current", ~"k", ~"v"),
    sys:replace_state(Pid, fun(State) ->
                                   ets:insert(nova_session_ets_entries,
                                              [{~"two", #{~"k" => ~"v"}},
                                               {~"three", #{~"k" => ~"v"}, Soon},
                                               {~"three-forever", #{~"k" => ~"v"}, infinity},
                                               {~"three-expired", #{~"k" => ~"v"}, Past}]),
                                   State
                           end),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, nova_session_ets, "1.0", []),
    ok = sys:change_code(Pid, nova_session_ets, "1.0", []),
    ok = sys:resume(Pid),
    Live = [~"two", ~"three", ~"three-forever", ~"current"],
    [?assertEqual({Id, {ok, ~"v"}}, {Id, nova_session_ets:get_value(Id, ~"k")}) || Id <- Live],
    [?assertEqual({Id, ok}, {Id, nova_session_ets:set_value(Id, ~"k2", ~"v2")}) || Id <- Live],
    [?assertEqual({Id, {ok, ~"v2"}}, {Id, nova_session_ets:get_value(Id, ~"k2")}) || Id <- Live],
    ?assertEqual({error, not_found}, nova_session_ets:get_value(~"three-expired", ~"k")),
    ?assertMatch([{~"three", _, Soon, kept}], ets:lookup(nova_session_ets_entries, ~"three")),
    Started ! sweep,
    ok = gen_server:call(Pid, sync),
    ?assertEqual([], ets:lookup(nova_session_ets_entries, ~"three-expired")),
    ?assertEqual(4, ets:info(nova_session_ets_entries, size)),
    ?assertEqual(4, ets:info(nova_session_ets_expiry, size)),
    ?assertEqual(Started, whereis(Pid)).

terminate_returns_ok() ->
    ?assertEqual(ok, nova_session_ets:terminate(normal, #{})).
