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
    nova_session_ets:get_value(<<"__sync__">>, <<"__sync__">>).

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
      fun code_change_passthrough/0,
      fun terminate_returns_ok/0
     ]}.

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

code_change_passthrough() ->
    ?assertEqual({ok, mystate}, nova_session_ets:code_change("1.0", mystate, [])).

terminate_returns_ok() ->
    ?assertEqual(ok, nova_session_ets:terminate(normal, #{})).
