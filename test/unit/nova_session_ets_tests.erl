-module(nova_session_ets_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

session_ets_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Get value - not found", fun test_get_value_not_found/0},
      {"Set value creates session", fun test_set_value_creates_session/0},
      {"Set value updates existing session", fun test_set_value_updates/0},
      {"Delete value - whole session", fun test_delete_session/0},
      {"Delete value - single key", fun test_delete_key/0}
     ]}.

setup() ->
    %% Mock nova_pubsub to avoid dependency on full PubSub system
    meck:new(nova_pubsub, [non_strict]),
    meck:expect(nova_pubsub, join, fun(_Channel) -> ok end),
    meck:expect(nova_pubsub, broadcast, fun(_Channel, _Topic, _Payload) -> ok end),

    %% Start the session ETS server
    {ok, Pid} = nova_session_ets:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the server
    exit(Pid, normal),
    timer:sleep(50),

    catch meck:unload(nova_pubsub),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_get_value_not_found() ->
    %% Try to get value from non-existent session
    SessionId = <<"nonexistent_session">>,

    Result = nova_session_ets:get_value(SessionId, <<"any_key">>),

    ?assertEqual({error, not_found}, Result).

test_set_value_creates_session() ->
    %% Set value should broadcast to PubSub (we just verify the call happens)
    SessionId = <<"new_session">>,

    ok = nova_session_ets:set_value(SessionId, <<"key">>, <<"value">>),

    %% Verify broadcast was called
    ?assert(meck:called(nova_pubsub, broadcast, ['__sessions', "set_value", {SessionId, <<"key">>, <<"value">>}])).

test_set_value_updates() ->
    %% Test updating an existing key
    SessionId = <<"update_session">>,

    %% First set
    ok = nova_session_ets:set_value(SessionId, <<"counter">>, <<"1">>),

    %% Second set (update)
    ok = nova_session_ets:set_value(SessionId, <<"counter">>, <<"2">>),

    %% Verify broadcasts happened
    ?assertEqual(2, meck:num_calls(nova_pubsub, broadcast, ['__sessions', "set_value", '_'])).

test_delete_session() ->
    %% Test deleting entire session
    SessionId = <<"delete_me">>,

    ok = nova_session_ets:delete_value(SessionId),

    %% Verify broadcast was called
    ?assert(meck:called(nova_pubsub, broadcast, ['__sessions', "delete_value", SessionId])).

test_delete_key() ->
    %% Test deleting a single key from session
    SessionId = <<"session_with_key">>,
    Key = <<"delete_this_key">>,

    ok = nova_session_ets:delete_value(SessionId, Key),

    %% Verify broadcast was called with tuple
    ?assert(meck:called(nova_pubsub, broadcast, ['__sessions', "delete_value", {SessionId, Key}])).
