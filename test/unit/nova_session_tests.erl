-module(nova_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Generate session ID", fun test_generate_session_id/0},
      {"Session ID uniqueness", fun test_session_id_uniqueness/0},
      {"Get from non-existent session", fun test_get_no_session/0},
      {"Set without session ID", fun test_set_no_session/0}
     ]}.

setup() ->
    %% Set up session configuration
    application:set_env(nova, use_sessions, true),

    %% Mock nova environment
    meck:new(nova, [passthrough]),
    meck:expect(nova, get_env, fun(use_sessions, _Default) -> true end),

    ok.

cleanup(_) ->
    catch meck:unload(nova),
    ok.

%%------------------------------------------------------------------------------
%% Helper Functions
%%------------------------------------------------------------------------------

%% Mock cowboy_req functions for testing
mock_req_with_session(SessionId) ->
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, match_cookies,
                fun([{session_id, [], undefined}], _Req) ->
                        #{session_id => SessionId}
                end),
    #{session_id => SessionId}.

mock_req_without_session() ->
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, match_cookies,
                fun([{session_id, [], undefined}], _Req) ->
                        #{session_id => undefined}
                end),
    meck:expect(cowboy_req, set_resp_cookie,
                fun(<<"session_id">>, _SessionId, Req, _Opts) ->
                        Req#{cookie_set => true}
                end),
    #{}.

cleanup_meck() ->
    catch meck:unload(cowboy_req).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_generate_session_id() ->
    %% Generate a session ID
    {ok, SessionId} = nova_session:generate_session_id(),

    %% Session ID should be a binary
    ?assert(is_binary(SessionId)),

    %% Session ID should be base64 encoded (length should be appropriate)
    ?assert(byte_size(SessionId) > 0).

test_session_id_uniqueness() ->
    %% Generate multiple session IDs
    {ok, Id1} = nova_session:generate_session_id(),
    {ok, Id2} = nova_session:generate_session_id(),
    {ok, Id3} = nova_session:generate_session_id(),

    %% All IDs should be unique
    ?assertNotEqual(Id1, Id2),
    ?assertNotEqual(Id2, Id3),
    ?assertNotEqual(Id1, Id3).

test_get_no_session() ->
    %% Try to get from request without session
    Req = mock_req_without_session(),

    Result = nova_session:get(Req, <<"key">>),

    %% Should return error
    ?assertEqual({error, not_found}, Result),

    cleanup_meck().

test_set_no_session() ->
    %% Try to set without session ID
    Req = mock_req_without_session(),

    Result = nova_session:set(Req, <<"key">>, <<"value">>),

    %% Should return error
    ?assertEqual({error, session_id_not_set}, Result),

    cleanup_meck().

