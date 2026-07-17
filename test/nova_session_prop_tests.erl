-module(nova_session_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Properties
%%====================================================================

%% generate_session_id always returns {ok, Binary}
prop_generate_session_id_returns_ok_binary() ->
    ?FORALL(_, integer(),
            begin
                {ok, Id} = nova_session:generate_session_id(),
                is_binary(Id)
            end).

%% Session IDs are always valid base64
prop_session_id_is_valid_base64() ->
    ?FORALL(_, integer(),
            begin
                {ok, Id} = nova_session:generate_session_id(),
                %% base64:decode will crash if not valid base64
                Decoded = base64:decode(Id),
                is_binary(Decoded) andalso byte_size(Decoded) =:= 32
            end).

%%====================================================================
%% EUnit tests
%%====================================================================

%% Two calls never produce the same ID (statistical property)
session_id_uniqueness_test() ->
    Ids = [begin {ok, Id} = nova_session:generate_session_id(), Id end
           || _ <- lists:seq(1, 1000)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)).

%%====================================================================
%% EUnit wrappers for PropEr
%%====================================================================

proper_test_() ->
    Opts = [{numtests, 100}, {to_file, user}],
    [
     {"generate_session_id returns {ok, binary()}",
      ?_assert(proper:quickcheck(prop_generate_session_id_returns_ok_binary(), Opts))},
     {"session ID is valid base64 of 32 bytes",
      ?_assert(proper:quickcheck(prop_session_id_is_valid_base64(), Opts))}
    ].
