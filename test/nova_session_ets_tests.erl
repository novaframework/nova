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
