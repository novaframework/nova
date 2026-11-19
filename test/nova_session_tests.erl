-module(nova_session_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_pubsub.hrl").

%%====================================================================
%% Integration tests for nova_session + nova_session_ets
%%
%% These tests start the real nova_session_ets gen_server with pg-backed
%% pubsub, exercising the full set/get/delete path through ETS.
%%====================================================================

%%--------------------------------------------------------------------
%% Setup / Cleanup
%%--------------------------------------------------------------------

setup() ->
    pg:start_link(nova_scope),
    application:unset_env(nova, environment),
    application:set_env(nova, bootstrap_application, nova),
    application:set_env(nova, use_sessions, true),
    application:set_env(nova, session_manager, nova_session_ets),
    {ok, Pid} = nova_session_ets:start_link(),
    meck:new(cowboy_req, [non_strict]),
    meck:expect(cowboy_req, parse_cookies,
                fun(Req) ->
                    case maps:get(cookie_session_id, Req, undefined) of
                        undefined -> [];
                        unparsable -> exit({request_error, {header, ~"cookie"}, malformed});
                        SId -> [{nova_session:cookie_name(), SId}]
                    end
                end),
    meck:expect(cowboy_req, set_resp_cookie,
                fun(Name, Value, Req, Opts) ->
                    Req#{resp_cookie => #{name => Name, value => Value, opts => Opts}}
                end),
    meck:expect(cowboy_req, cast, fun(Event, Req) -> meck:passthrough([Event, Req]) end),
    Pid.

cleanup(Pid) ->
    meck:unload(cowboy_req),
    gen_server:stop(Pid),
    application:unset_env(nova, bootstrap_application),
    application:unset_env(nova, use_sessions),
    application:unset_env(nova, session_manager),
    ok.

%% After a pubsub broadcast, do a gen_server:call to synchronize
sync() ->
    nova_session_ets:get_value(<<"__sync__">>, <<"__sync__">>).

req_with_session(SessionId) ->
    #{nova_session_id => SessionId}.

req_with_cookie(SessionId) ->
    #{cookie_session_id => SessionId}.

%% A request whose connection process is a stand-in that forwards what
%% nova_session tells the stream handler.
stream_req(SessionId) ->
    Self = self(),
    Conn = spawn_link(fun() ->
                              receive
                                  {{_Pid, 7}, Event} -> Self ! {stream_event, Event}
                              after 2000 ->
                                      ok
                              end
                      end),
    #{nova_session_id => SessionId, pid => Conn, streamid => 7, nova_session_owner => self()}.

%% The request as nova_stream_h hands it over when the client sent a cookie.
presented_req(Presented, Pending) ->
    (maps:remove(nova_session_id, stream_req(Pending)))#{nova_session_presented => Presented,
                                                         nova_session_pending => Pending}.

no_stream_event() ->
    receive
        {stream_event, Event} -> error({unexpected_stream_event, Event})
    after 200 ->
            ok
    end.

stream_event() ->
    receive
        {stream_event, {nova_session, Event}} -> Event
    after 1000 ->
            error(no_stream_event)
    end.

%%====================================================================
%% Test generators
%%====================================================================

nova_session_ets_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun set_and_get_value/0,
      fun get_missing_session/0,
      fun get_missing_key/0,
      fun set_multiple_keys/0,
      fun delete_session/0,
      fun delete_single_key/0,
      fun cookie_fallback/0,
      fun no_session_returns_error/0,
      fun unparsable_cookie/0,
      fun no_session_delete_returns_ok/0,
      fun sessions_disabled_throws/0,
      fun use_sessions_env/0,
      fun delete_moves_request_to_new_session/0,
      fun delete_tells_stream_handler/0,
      fun set_tells_stream_handler/0,
      fun renew_drops_old_session/0,
      fun renew_tells_stream_handler/0,
      fun sign_in_stores_under_a_new_id/0,
      fun sign_in_moves_a_visitor_off_an_id_that_was_handed_out/0,
      fun sign_in_says_when_the_backend_stores_nothing/0,
      fun backend_is_asked_once_about_a_presented_id/0,
      fun deleted_session_is_asked_about_again/0,
      fun session_deleted_by_another_process_is_not_written_again/0,
      fun renew_without_session/0,
      fun renew_sets_cookie_without_stream_handler/0,
      fun delete_expires_cookie_without_stream_handler/0,
      fun invalid_cookie_name_raises_without_stream_handler/0,
      fun invalid_cookie_domain_raises_without_stream_handler/0,
      fun renew_with_stream_handler_sets_no_cookie/0,
      fun known_presented_id_is_used/0,
      fun unknown_presented_id_is_replaced/0,
      fun id_that_was_handed_out_is_kept_by_the_first_write/0,
      fun id_that_was_handed_out_without_a_secret_is_replaced/0,
      fun id_nobody_handed_out_is_replaced_with_a_secret_set/0,
      fun id_of_a_secret_that_is_gone_is_replaced/0,
      fun id_in_another_spelling_is_replaced/0,
      fun handed_out_id_is_not_kept_for_another_process/0,
      fun handed_out_id_that_was_deleted_is_not_written_again/0,
      fun handed_out_id_another_process_deleted_is_not_written_again/0,
      fun handed_out_id_that_ended_elsewhere_is_not_written/0,
      fun write_that_arrives_behind_the_delete_starts_another_session/0,
      fun backend_without_ended_ids_is_not_asked_about_them/0,
      fun session_ids_have_one_length/0,
      fun renew_drops_presented_id/0,
      fun spawned_process_does_not_notify/0,
      fun request_without_owner_does_not_notify/0,
      fun non_error_set_results_notify/0,
      fun provisional_write_goes_to_a_backend_that_takes_it/0,
      fun provisional_write_is_an_ordinary_one_for_a_backend_without_it/0,
      fun delete_key_preserves_other_keys/0,
      fun set_overwrites_existing_key/0,
      fun generate_session_id_format/0
     ]}.

%%====================================================================
%% Tests
%%====================================================================

set_and_get_value() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"username">>, <<"alice">>),
    sync(),
    ?assertEqual({ok, <<"alice">>}, nova_session:get(Req, <<"username">>)).

get_missing_session() ->
    Req = #{},
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)).

get_missing_key() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"exists">>, <<"yes">>),
    sync(),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"missing">>)).

set_multiple_keys() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"k1">>, <<"v1">>),
    ok = nova_session:set(Req, <<"k2">>, <<"v2">>),
    sync(),
    ?assertEqual({ok, <<"v1">>}, nova_session:get(Req, <<"k1">>)),
    ?assertEqual({ok, <<"v2">>}, nova_session:get(Req, <<"k2">>)).

delete_session() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"key">>, <<"val">>),
    sync(),
    {ok, _Req1} = nova_session:delete(Req),
    sync(),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)).

delete_single_key() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"keep">>, <<"yes">>),
    ok = nova_session:set(Req, <<"remove">>, <<"bye">>),
    sync(),
    {ok, _Req1} = nova_session:delete(Req, <<"remove">>),
    sync(),
    ?assertEqual({ok, <<"yes">>}, nova_session:get(Req, <<"keep">>)),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"remove">>)).

cookie_fallback() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_cookie(SId),
    ok = nova_session:set(Req, <<"via_cookie">>, <<"works">>),
    sync(),
    ?assertEqual({ok, <<"works">>}, nova_session:get(Req, <<"via_cookie">>)).

no_session_returns_error() ->
    Req = #{},
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"key">>)),
    ?assertEqual({error, session_id_not_set}, nova_session:set(Req, <<"k">>, <<"v">>)).

unparsable_cookie() ->
    Req = req_with_cookie(unparsable),
    ?assertEqual(unparsable, nova_session:presented_session_id(Req)),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"key")),
    ?assertEqual({error, session_id_not_set}, nova_session:set(Req, ~"k", ~"v")).

no_session_delete_returns_ok() ->
    Req = #{},
    ?assertEqual({ok, Req}, nova_session:delete(Req)),
    ?assertEqual({ok, Req}, nova_session:delete(Req, <<"key">>)).

sessions_disabled_throws() ->
    application:set_env(nova, use_sessions, false),
    Req = req_with_session(<<"id">>),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:get(Req, <<"k">>)),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:set(Req, <<"k">>, <<"v">>)),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:delete(Req)),
    ?assertThrow({nova_session, unsupported_session_used}, nova_session:delete(Req, <<"k">>)),
    application:set_env(nova, use_sessions, true).

%% Same rule as json_lib: the bootstrap application wins, then nova.
use_sessions_env() ->
    application:set_env(nova, bootstrap_application, nova_session_tests_app),
    application:set_env(nova_session_tests_app, use_sessions, true),
    application:set_env(nova, use_sessions, false),
    try
        ?assert(nova_session:enabled()),
        application:unset_env(nova_session_tests_app, use_sessions),
        ?assertNot(nova_session:enabled()),
        ?assertThrow({nova_session, unsupported_session_used},
                     nova_session:get(req_with_session(~"id"), ~"k")),
        application:unset_env(nova, use_sessions),
        ?assert(nova_session:enabled())
    after
        application:unset_env(nova_session_tests_app, use_sessions),
        application:set_env(nova, bootstrap_application, nova),
        application:set_env(nova, use_sessions, true)
    end.

delete_moves_request_to_new_session() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, ~"key", ~"val"),
    {ok, Req1} = nova_session:delete(Req),
    #{nova_session_id := NewId} = Req1,
    ?assertNotEqual(SId, NewId),
    ok = nova_session:set(Req1, ~"other", ~"x"),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"other")),
    ?assertEqual({ok, ~"x"}, nova_session:get(Req1, ~"other")).

delete_tells_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = stream_req(SId),
    {ok, #{nova_session_id := NewId} = Req1} = nova_session:delete(Req),
    ?assertEqual({deleted, NewId}, stream_event()),
    ?assertNot(is_map_key(resp_cookie, Req1)).

set_tells_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    ok = nova_session:set(stream_req(SId), ~"k", ~"v"),
    ?assertEqual({written, SId}, stream_event()).

renew_drops_old_session() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, ~"user_id", ~"1"),
    {ok, Req1} = nova_session:renew(Req),
    #{nova_session_id := NewId} = Req1,
    ?assertNotEqual(SId, NewId),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"user_id")),
    ?assertEqual({error, not_found}, nova_session:get(Req1, ~"user_id")),
    ok = nova_session:set(Req1, ~"user_id", ~"2"),
    ?assertEqual({ok, ~"2"}, nova_session:get(Req1, ~"user_id")),
    ?assertNot(nova_session_ets:session_exists(SId)).

renew_tells_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, #{nova_session_id := NewId}} = nova_session:renew(stream_req(SId)),
    ?assertEqual({renewed, NewId}, stream_event()).

%% Without nova_stream_h, renew/1 and delete/1 put the cookie on the request.
renew_sets_cookie_without_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, Req1} = nova_session:renew(req_with_cookie(SId)),
    #{nova_session_id := NewId, resp_cookie := #{name := Name, value := Value, opts := Opts}} = Req1,
    ?assertEqual({~"__Host-session_id", NewId}, {Name, Value}),
    ?assertMatch(#{path := ~"/", http_only := true, max_age := 1209600}, Opts),
    {ok, Req2} = nova_session:renew(Req1),
    #{nova_session_id := NewerId, resp_cookie := #{value := NewerId}} = Req2,
    ?assertNotEqual(NewId, NewerId).

delete_expires_cookie_without_stream_handler() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, Req1} = nova_session:delete(req_with_cookie(SId)),
    ?assertMatch(#{resp_cookie := #{name := ~"__Host-session_id", value := <<>>,
                                    opts := #{max_age := 0, path := ~"/", http_only := true}}}, Req1).

%% cowboy_req:set_resp_cookie/4 is mocked here, so the refusal is Nova's own
%% and does not depend on the cowlib version.
invalid_cookie_name_raises_without_stream_handler() ->
    application:set_env(nova, session_cookie_name, ~"app:sid"),
    try
        {ok, SId} = nova_session:generate_session_id(),
        ?assertError({invalid_session_cookie_config, #{name := ~"app:sid"}},
                     nova_session:renew(req_with_cookie(SId)))
    after
        application:unset_env(nova, session_cookie_name)
    end.

invalid_cookie_domain_raises_without_stream_handler() ->
    application:set_env(nova, session_cookie_name, ~"session_id"),
    application:set_env(nova, session_cookie_opts, #{domain => <<"a\nb">>}),
    try
        {ok, SId} = nova_session:generate_session_id(),
        ?assertError({invalid_session_cookie_config, #{domain := <<"a\nb">>}},
                     nova_session:renew(req_with_cookie(SId))),
        ?assertError({invalid_session_cookie_config, #{domain := <<"a\nb">>}},
                     nova_session:delete(req_with_cookie(SId)))
    after
        application:unset_env(nova, session_cookie_name),
        application:unset_env(nova, session_cookie_opts)
    end.

renew_with_stream_handler_sets_no_cookie() ->
    {ok, SId} = nova_session:generate_session_id(),
    {ok, Req1} = nova_session:renew(req_with_session(SId)),
    ?assertNot(is_map_key(resp_cookie, Req1)),
    {ok, Req2} = nova_session:delete(Req1),
    ?assertNot(is_map_key(resp_cookie, Req2)).

renew_without_session() ->
    {ok, #{nova_session_id := NewId}} = nova_session:renew(nova_test_helper:mock_req(~"GET", ~"/")),
    ?assert(is_binary(NewId)).

known_presented_id_is_used() ->
    ok = nova_session_ets:set_value(~"known", ~"k", ~"v"),
    Req = presented_req(~"known", ~"pending"),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")),
    ok = nova_session:set(Req, ~"k2", ~"v2"),
    ?assertEqual({written, ~"known"}, stream_event()).

%% A client must not be able to pick its own session id.
unknown_presented_id_is_replaced() ->
    Req = presented_req(~"attacker-chosen", ~"pending"),
    ok = nova_session:set(Req, ~"k", ~"v"),
    ?assertEqual({written, ~"pending"}, stream_event()),
    ?assertNot(nova_session_ets:session_exists(~"attacker-chosen")),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")).

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").
-define(OTHER_SECRET, ~"another secret of thirty-two bytes or more, for them").

with_secret(Secret, Fun) ->
    ok = nova_test_settings:set(secret_key_base, Secret),
    try Fun()
    after nova_test_settings:unset(secret_key_base)
    end.

%% With a secret set, an id says by itself that Nova handed it out. A
%% visitor who got one with a page, and no session, keeps it when the
%% session is first written.
id_that_was_handed_out_is_kept_by_the_first_write() ->
    with_secret(?SECRET,
                fun() ->
                        {ok, Id} = nova_session:generate_session_id(),
                        Req = presented_req(Id, ~"pending"),
                        ?assertEqual({error, not_found}, nova_session:get(Req, ~"k")),
                        ok = nova_session:set(Req, ~"k", ~"v"),
                        ?assertEqual({written, Id}, stream_event()),
                        ?assertEqual({ok, ~"v"}, nova_session_ets:get_value(Id, ~"k")),
                        ?assertNot(nova_session_ets:session_exists(~"pending"))
                end).

id_that_was_handed_out_without_a_secret_is_replaced() ->
    {ok, Id} = nova_session:generate_session_id(),
    ok = nova_session:set(presented_req(Id, ~"pending"), ~"k", ~"v"),
    ?assertEqual({written, ~"pending"}, stream_event()),
    ?assertNot(nova_session_ets:session_exists(Id)).

id_nobody_handed_out_is_replaced_with_a_secret_set() ->
    with_secret(?SECRET,
                fun() ->
                        {ok, <<First, Rest/binary>>} = nova_session:generate_session_id(),
                        Changed = <<(case First of $A -> $B; _ -> $A end), Rest/binary>>,
                        [begin
                             ok = nova_session:set(presented_req(Chosen, ~"pending"), ~"k", ~"v"),
                             ?assertEqual({Chosen, {written, ~"pending"}}, {Chosen, stream_event()}),
                             ?assertNot(nova_session_ets:session_exists(Chosen))
                         end || Chosen <- [~"attacker-chosen", Changed, base64:encode(crypto:strong_rand_bytes(32)),
                                           base64:encode(crypto:strong_rand_bytes(16)), ~"", ~"!!!!"]]
                end).

%% The decoder skips whitespace, takes an id without its padding when told
%% to and ignores the bits of the last character that belong to no byte, so
%% one signed id can be written in any number of ways. Each of them would be
%% another session to the backend, and to whatever counts or blocks by the
%% cookie. Only the spelling Nova hands out is the id.
id_in_another_spelling_is_replaced() ->
    with_secret(?SECRET,
                fun() ->
                        {ok, Id} = nova_session:generate_session_id(),
                        [begin
                             ?assertEqual({Spelling, base64:decode(Id)},
                                          {Spelling, base64:decode(Spelling, #{padding => false})}),
                             ok = nova_session:set(presented_req(Spelling, ~"pending"), ~"k", ~"v"),
                             ?assertEqual({Spelling, {written, ~"pending"}}, {Spelling, stream_event()}),
                             ?assertNot(nova_session_ets:session_exists(Spelling))
                         end || Spelling <- other_spellings(Id)],
                        ok = nova_session:set(presented_req(Id, ~"pending"), ~"k", ~"v"),
                        ?assertEqual({written, Id}, stream_event())
                end).

%% Whitespace in front, inside and behind, the padding left out, and every
%% other value of the two bits the last character of 32 bytes has to spare.
other_spellings(<<Half:22/binary, Rest/binary>> = Id) ->
    <<Front:42/binary, Last, "=">> = Id,
    [<<" ", Id/binary>>, <<Half/binary, " ", Rest/binary>>, <<Half/binary, "\t", Rest/binary>>,
     <<Half/binary, "\r\n", Rest/binary>>, <<Id/binary, "\n">>, <<Front/binary, Last>>
     | [<<Front/binary, Spare, "=">> || Spare <- same_bytes_as(Last), Spare =/= Last]].

same_bytes_as(Character) ->
    Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    Values = lists:zip(Alphabet, lists:seq(0, 63)),
    {Character, Value} = lists:keyfind(Character, 1, Values),
    [Other || {Other, OtherValue} <- Values, OtherValue bsr 2 =:= Value bsr 2].

id_of_a_secret_that_is_gone_is_replaced() ->
    Id = with_secret(?SECRET, fun() -> {ok, Id} = nova_session:generate_session_id(), Id end),
    with_secret([?OTHER_SECRET, ?SECRET],
                fun() ->
                        ok = nova_session:set(presented_req(Id, ~"pending"), ~"k", ~"v"),
                        ?assertEqual({written, Id}, stream_event())
                end),
    ok = nova_session_ets:delete_value(Id),
    with_secret(?OTHER_SECRET,
                fun() ->
                        ok = nova_session:set(presented_req(Id, ~"other-pending"), ~"k", ~"v"),
                        ?assertEqual({written, ~"other-pending"}, stream_event())
                end).

%% Signing in is one call: the id changes and the value is stored under the
%% new one, which the request that comes back carries.
sign_in_stores_under_a_new_id() ->
    ok = nova_session_ets:set_value(~"before-sign-in", ~"basket", ~"1"),
    Req = presented_req(~"before-sign-in", ~"pending"),
    {ok, SignedIn} = nova_session:sign_in(Req, ~"user", ~"alice"),
    #{nova_session_id := <<NewId/binary>>} = SignedIn,
    ?assertEqual({renewed, NewId}, stream_event()),
    ?assertNot(lists:member(NewId, [~"before-sign-in", ~"pending"])),
    ?assertEqual({ok, ~"alice"}, nova_session:get(SignedIn, ~"user")),
    ?assertEqual({ok, ~"alice"}, nova_session_ets:get_value(NewId, ~"user")),
    ?assertEqual({error, not_found}, nova_session:get(SignedIn, ~"basket")),
    ?assertNot(nova_session_ets:session_exists(~"before-sign-in")),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"user")).

sign_in_moves_a_visitor_off_an_id_that_was_handed_out() ->
    with_secret(?SECRET,
                fun() ->
                        {ok, HandedOut} = nova_session:generate_session_id(),
                        {ok, SignedIn} = nova_session:sign_in(presented_req(HandedOut, ~"pending"), ~"user", ~"alice"),
                        #{nova_session_id := <<NewId/binary>>} = SignedIn,
                        ?assertEqual({renewed, NewId}, stream_event()),
                        ?assertNot(lists:member(NewId, [HandedOut, ~"pending"])),
                        ?assertEqual({ok, ~"alice"}, nova_session_ets:get_value(NewId, ~"user")),
                        ?assertNot(nova_session_ets:session_exists(HandedOut))
                end).

sign_in_says_when_the_backend_stores_nothing() ->
    meck:new(nova_session_ets, [passthrough]),
    try
        meck:expect(nova_session_ets, set_value, fun(_SessionId, _Key, _Value) -> {error, session_store_full} end),
        ?assertEqual({error, session_store_full},
                     nova_session:sign_in(presented_req(~"refused", ~"pending"), ~"user", ~"alice"))
    after
        meck:unload(nova_session_ets)
    end.

%% A process that is not serving the request cannot move the cookie, and
%% has no say in which id a session starts under.
handed_out_id_is_not_kept_for_another_process() ->
    with_secret(?SECRET,
                fun() ->
                        {ok, Id} = nova_session:generate_session_id(),
                        Req = presented_req(Id, ~"pending"),
                        {Pid, Ref} = spawn_monitor(fun() -> ok = nova_session:set(Req, ~"k", ~"v") end),
                        receive {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason)
                        after 1000 -> error(timeout)
                        end,
                        ?assertNot(nova_session_ets:session_exists(Id)),
                        ?assert(nova_session_ets:session_exists(~"pending"))
                end).

%% An id that says Nova handed it out says nothing about the session being
%% there still. What deleted_session_is_asked_about_again and
%% session_deleted_by_another_process_is_not_written_again hold goes for it.
handed_out_id_that_was_deleted_is_not_written_again() ->
    with_secret(?SECRET,
                fun() ->
                        [begin
                             {ok, Id} = nova_session:generate_session_id(),
                             ok = nova_session_ets:set_value(Id, ~"k", ~"v"),
                             Req = presented_req(Id, ~"pending"),
                             {ok, _Req} = Drop(Req),
                             ok = nova_session:set(Req, ~"k", ~"again"),
                             ?assertNot(nova_session_ets:session_exists(Id))
                         end || Drop <- [fun nova_session:delete/1, fun nova_session:renew/1]]
                end).

handed_out_id_another_process_deleted_is_not_written_again() ->
    with_secret(?SECRET,
                fun() ->
                        {ok, Id} = nova_session:generate_session_id(),
                        ok = nova_session_ets:set_value(Id, ~"user", ~"alice"),
                        Req = presented_req(Id, ~"pending"),
                        ?assertEqual({ok, ~"alice"}, nova_session:get(Req, ~"user")),
                        {Pid, Ref} = spawn_monitor(fun() -> {ok, _} = nova_session:delete(req_with_session(Id)) end),
                        receive {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason)
                        after 1000 -> error(timeout)
                        end,
                        ok = nova_session:set(Req, ~"k", ~"v"),
                        ?assertEqual({written, ~"pending"}, stream_event()),
                        ?assertNot(nova_session_ets:session_exists(Id))
                end).

%% The request has not touched its session before another one ends it, so
%% nothing in its own process says the id is over. The backend does.
handed_out_id_that_ended_elsewhere_is_not_written() ->
    with_secret(?SECRET,
                fun() ->
                        [begin
                             {ok, Id} = nova_session:generate_session_id(),
                             {ok, Pending} = nova_session:generate_session_id(),
                             _ = Stored andalso nova_session_ets:set_value(Id, ~"user", ~"alice"),
                             Req = presented_req(Id, Pending),
                             {Pid, Ref} = spawn_monitor(fun() -> {ok, _} = Drop(req_with_session(Id)) end),
                             receive {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason)
                             after 1000 -> error(timeout)
                             end,
                             ok = nova_session:set(Req, ~"note", ~"late"),
                             ?assertEqual({written, Pending}, stream_event()),
                             ?assertNot(nova_session_ets:session_exists(Id)),
                             ?assertEqual({ok, ~"late"}, nova_session:get(Req, ~"note"))
                         end || Stored <- [true, false], Drop <- [fun nova_session:delete/1, fun nova_session:renew/1]]
                end).

%% The backend said the session was there, and it is deleted before the
%% write gets to it. What the backend refuses for an id that has ended is
%% stored as it would have been a moment later: in a session of its own.
write_that_arrives_behind_the_delete_starts_another_session() ->
    ok = nova_session_ets:set_value(~"ends-meanwhile", ~"user", ~"alice"),
    Req = presented_req(~"ends-meanwhile", ~"pending"),
    meck:new(nova_session_ets, [passthrough]),
    try
        meck:expect(nova_session_ets, session_exists,
                    fun(~"ends-meanwhile") -> ok =:= nova_session_ets:delete_value(~"ends-meanwhile");
                       (SessionId) -> meck:passthrough([SessionId])
                    end),
        ?assertEqual(ok, nova_session:set(Req, ~"note", ~"late")),
        ?assertEqual({written, ~"pending"}, stream_event())
    after
        meck:unload(nova_session_ets)
    end,
    ?assertNot(nova_session_ets:session_exists(~"ends-meanwhile")),
    ?assertEqual({ok, ~"late"}, nova_session_ets:get_value(~"pending", ~"note")),
    ?assertEqual({ok, ~"late"}, nova_session:get(Req, ~"note")).

%% session_ended/1 is for a backend to have or not.
backend_without_ended_ids_is_not_asked_about_them() ->
    meck:new(fake_session_backend, [non_strict]),
    application:set_env(nova, session_manager, fake_session_backend),
    try
        with_secret(?SECRET,
                    fun() ->
                            {ok, Id} = nova_session:generate_session_id(),
                            meck:expect(fake_session_backend, session_exists, fun(_SessionId) -> false end),
                            meck:expect(fake_session_backend, set_value, fun(_SessionId, _Key, _Value) -> ok end),
                            ?assertEqual(ok, nova_session:set(presented_req(Id, ~"pending"), ~"k", ~"v")),
                            ?assertEqual({written, Id}, stream_event()),
                            meck:expect(fake_session_backend, session_ended, fun(_SessionId) -> true end),
                            ?assertEqual(ok, nova_session:set(presented_req(Id, ~"pending"), ~"k", ~"v")),
                            ?assertEqual({written, ~"pending"}, stream_event())
                    end)
    after
        application:set_env(nova, session_manager, nova_session_ets),
        meck:unload(fake_session_backend)
    end.

session_ids_have_one_length() ->
    {ok, Plain} = nova_session:generate_session_id(),
    {ok, Signed} = with_secret(?SECRET, fun nova_session:generate_session_id/0),
    ?assertEqual({44, 44}, {byte_size(Plain), byte_size(Signed)}),
    ?assertNotEqual(with_secret(?SECRET, fun nova_session:generate_session_id/0), {ok, Signed}).

backend_is_asked_once_about_a_presented_id() ->
    ok = nova_session_ets:set_value(~"asked-once", ~"k", ~"v"),
    Req = presented_req(~"asked-once", ~"pending"),
    meck:new(nova_session_ets, [passthrough]),
    try
        ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")),
        ?assertEqual({error, not_found}, nova_session:get(Req, ~"other")),
        ?assertEqual(1, meck:num_calls(nova_session_ets, session_exists, [~"asked-once"]))
    after
        meck:unload(nova_session_ets)
    end.

%% The process of a websocket, or a request that takes its time, may have
%% read the session before another request signed the client out.
session_deleted_by_another_process_is_not_written_again() ->
    ok = nova_session_ets:set_value(~"signed-out-elsewhere", ~"user", ~"alice"),
    Req = presented_req(~"signed-out-elsewhere", ~"pending"),
    ?assertEqual({ok, ~"alice"}, nova_session:get(Req, ~"user")),
    {Pid, Ref} = spawn_monitor(fun() ->
                                       {ok, _} = nova_session:delete(req_with_session(~"signed-out-elsewhere"))
                               end),
    receive {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason) after 1000 -> error(timeout) end,
    ok = nova_session:set(Req, ~"k", ~"v"),
    ?assertEqual({written, ~"pending"}, stream_event()),
    ?assertNot(nova_session_ets:session_exists(~"signed-out-elsewhere")),
    {ok, _Req} = nova_session:delete(Req, ~"k"),
    ?assertNot(nova_session_ets:session_exists(~"signed-out-elsewhere")).

%% A controller that goes on with the request it had before the delete must
%% not bring the deleted session back under the id the client still holds.
deleted_session_is_asked_about_again() ->
    ok = nova_session_ets:set_value(~"signed-out", ~"k", ~"v"),
    Req = presented_req(~"signed-out", ~"pending"),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")),
    {ok, _Req} = nova_session:delete(Req),
    ok = nova_session:set(Req, ~"k", ~"again"),
    ?assertNot(nova_session_ets:session_exists(~"signed-out")).

renew_drops_presented_id() ->
    ok = nova_session_ets:set_value(~"before", ~"k", ~"v"),
    {ok, Req1} = nova_session:renew(presented_req(~"before", ~"pending")),
    ?assertNot(maps:is_key(nova_session_presented, Req1)),
    ?assertNot(maps:is_key(nova_session_pending, Req1)),
    ?assertMatch({renewed, _}, stream_event()),
    ?assertEqual({error, not_found}, nova_session:get(Req1, ~"k")).

%% Only the request process tells the stream handler. A message from anywhere
%% else would reach whatever owns the connection by then, a websocket handler
%% for instance.
spawned_process_does_not_notify() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = stream_req(SId),
    Self = self(),
    _ = spawn_link(fun() -> Self ! {spawned, nova_session:set(Req, ~"k", ~"v")} end),
    receive {spawned, Result} -> ?assertEqual(ok, Result) after 1000 -> error(timeout) end,
    no_stream_event(),
    ?assertEqual({ok, ~"v"}, nova_session:get(Req, ~"k")).

request_without_owner_does_not_notify() ->
    {ok, SId} = nova_session:generate_session_id(),
    ok = nova_session:set(maps:remove(nova_session_owner, stream_req(SId)), ~"k", ~"v"),
    no_stream_event().

%% Backends that answer true or {ok, _} wrote the value too.
non_error_set_results_notify() ->
    meck:new(fake_session_backend, [non_strict]),
    application:set_env(nova, session_manager, fake_session_backend),
    try
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> true end),
        ?assertEqual(true, nova_session:set(stream_req(~"a"), ~"k", ~"v")),
        ?assertEqual({written, ~"a"}, stream_event()),
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> {ok, stored} end),
        ?assertEqual({ok, stored}, nova_session:set(stream_req(~"b"), ~"k", ~"v")),
        ?assertEqual({written, ~"b"}, stream_event()),
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> {error, full} end),
        ?assertEqual({error, full}, nova_session:set(stream_req(~"c"), ~"k", ~"v")),
        no_stream_event()
    after
        application:set_env(nova, session_manager, nova_session_ets),
        meck:unload(fake_session_backend)
    end.

provisional_write_goes_to_a_backend_that_takes_it() ->
    meck:new(fake_session_backend, [non_strict]),
    application:set_env(nova, session_manager, fake_session_backend),
    try
        meck:expect(fake_session_backend, set_value, fun(_, _, _) -> {error, not_this_one} end),
        meck:expect(fake_session_backend, set_provisional_value, fun(~"a", ~"k", ~"v") -> ok end),
        ?assertEqual(ok, nova_session:set_provisional(stream_req(~"a"), ~"k", ~"v")),
        ?assertEqual({written, ~"a"}, stream_event()),
        meck:expect(fake_session_backend, set_provisional_value, fun(_, _, _) -> {error, full} end),
        ?assertEqual({error, full}, nova_session:set_provisional(stream_req(~"b"), ~"k", ~"v")),
        no_stream_event()
    after
        application:set_env(nova, session_manager, nova_session_ets),
        meck:unload(fake_session_backend)
    end.

provisional_write_is_an_ordinary_one_for_a_backend_without_it() ->
    meck:new(fake_session_backend, [non_strict]),
    application:set_env(nova, session_manager, fake_session_backend),
    try
        meck:expect(fake_session_backend, set_value, fun(~"a", ~"k", ~"v") -> ok end),
        ?assertEqual(ok, nova_session:set_provisional(stream_req(~"a"), ~"k", ~"v")),
        ?assertEqual({written, ~"a"}, stream_event())
    after
        application:set_env(nova, session_manager, nova_session_ets),
        meck:unload(fake_session_backend)
    end.

delete_key_preserves_other_keys() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"a">>, <<"1">>),
    ok = nova_session:set(Req, <<"b">>, <<"2">>),
    ok = nova_session:set(Req, <<"c">>, <<"3">>),
    sync(),
    {ok, _} = nova_session:delete(Req, <<"b">>),
    sync(),
    ?assertEqual({ok, <<"1">>}, nova_session:get(Req, <<"a">>)),
    ?assertEqual({error, not_found}, nova_session:get(Req, <<"b">>)),
    ?assertEqual({ok, <<"3">>}, nova_session:get(Req, <<"c">>)).

set_overwrites_existing_key() ->
    {ok, SId} = nova_session:generate_session_id(),
    Req = req_with_session(SId),
    ok = nova_session:set(Req, <<"key">>, <<"old">>),
    sync(),
    ok = nova_session:set(Req, <<"key">>, <<"new">>),
    sync(),
    ?assertEqual({ok, <<"new">>}, nova_session:get(Req, <<"key">>)).

generate_session_id_format() ->
    {ok, Id} = nova_session:generate_session_id(),
    ?assert(is_binary(Id)),
    Decoded = base64:decode(Id),
    ?assertEqual(32, byte_size(Decoded)),
    {ok, Id2} = nova_session:generate_session_id(),
    ?assertNotEqual(Id, Id2).

%%--------------------------------------------------------------------
%% cookie_opts/1
%%--------------------------------------------------------------------

cookie_opts_test_() ->
    {foreach,
     fun() -> application:set_env(nova, bootstrap_application, nova) end,
     fun(_) ->
             [application:unset_env(nova, Key)
              || Key <- [environment, session_cookie_name, session_cookie_opts, session_max_age,
                         bootstrap_application]]
     end,
     [
      fun opts_prod/0,
      fun opts_dev/0,
      fun opts_configured/0,
      fun opts_host_prefix/0,
      fun opts_max_age/0
     ]}.

opts_prod() ->
    application:set_env(nova, environment, prod),
    ?assertEqual(#{path => ~"/", http_only => true, same_site => lax, secure => true, max_age => 1209600},
                 nova_session:cookie_opts(#{scheme => ~"http"})).

opts_dev() ->
    application:set_env(nova, environment, dev),
    ?assertMatch(#{secure := false}, nova_session:cookie_opts(#{scheme => ~"http"})),
    ?assertMatch(#{secure := true}, nova_session:cookie_opts(#{scheme => ~"https"})).

%% A value of the wrong type leaves the default in place.
opts_configured() ->
    application:set_env(nova, session_cookie_name, ~"sid"),
    application:set_env(nova, session_cookie_opts,
                        #{same_site => strict, path => "/x", http_only => false, domain => ~"example.com"}),
    ?assertMatch(#{same_site := strict, path := ~"/", http_only := false, domain := ~"example.com"},
                 nova_session:cookie_opts(#{scheme => ~"http"})).

%% A browser only keeps a __Host- cookie that is Secure, has Path=/ and no
%% Domain, so those win over session_cookie_opts.
opts_host_prefix() ->
    application:set_env(nova, session_cookie_opts,
                        #{domain => ~"example.com", path => ~"/app", secure => false, same_site => strict}),
    Opts = nova_session:cookie_opts(#{scheme => ~"http"}),
    ?assertEqual(#{path => ~"/", http_only => true, same_site => strict, secure => true, max_age => 1209600},
                 Opts),
    application:set_env(nova, environment, dev),
    application:set_env(nova, session_cookie_name, ~"__Host-sid"),
    ?assertEqual(Opts, nova_session:cookie_opts(#{scheme => ~"http"})).

%% The cookie lasts as long as the server keeps the session.
opts_max_age() ->
    ?assertMatch(#{max_age := 1209600}, nova_session:cookie_opts(#{scheme => ~"https"})),
    application:set_env(nova, session_max_age, 60),
    ?assertMatch(#{max_age := 60}, nova_session:cookie_opts(#{scheme => ~"https"})),
    application:set_env(nova, session_max_age, 0),
    ?assertMatch(#{max_age := 1209600}, nova_session:cookie_opts(#{scheme => ~"https"})),
    application:set_env(nova, session_max_age, infinity),
    ?assertNot(is_map_key(max_age, nova_session:cookie_opts(#{scheme => ~"https"}))),
    application:set_env(nova, session_cookie_opts, #{max_age => 3600}),
    ?assertMatch(#{max_age := 3600}, nova_session:cookie_opts(#{scheme => ~"https"})).

%%--------------------------------------------------------------------
%% The name of the cookie and the session id a request presents
%%--------------------------------------------------------------------

cookie_name_test_() ->
    {foreach,
     fun() ->
             application:set_env(nova, bootstrap_application, nova),
             application:unset_env(nova, environment),
             application:unset_env(nova, session_cookie_name),
             nova_once:forget({nova_session, ambiguous_cookie_warned})
     end,
     fun(_) ->
             application:unset_env(nova, environment),
             application:unset_env(nova, session_cookie_name),
             application:unset_env(nova, bootstrap_application),
             nova_once:forget({nova_session, ambiguous_cookie_warned})
     end,
     [
      fun name_outside_dev/0,
      fun name_in_dev/0,
      fun name_configured/0,
      fun old_name/0,
      fun ambiguous_cookie/0,
      fun repeated_cookie/0,
      fun needs_https/0
     ]}.

name_outside_dev() ->
    ?assertEqual(~"__Host-session_id", nova_session:cookie_name()),
    [begin
         application:set_env(nova, environment, Environment),
         ?assertEqual({Environment, ~"__Host-session_id"}, {Environment, nova_session:cookie_name()})
     end || Environment <- [prod, test, staging]].

%% A browser keeps session_id over plain http.
name_in_dev() ->
    application:set_env(nova, environment, dev),
    ?assertEqual(~"session_id", nova_session:cookie_name()),
    ?assertMatch(#{secure := false}, nova_session:cookie_opts(#{scheme => ~"http"})).

name_configured() ->
    application:set_env(nova, session_cookie_name, ~"sid"),
    ?assertEqual(~"sid", nova_session:cookie_name()),
    application:set_env(nova, environment, dev),
    ?assertEqual(~"sid", nova_session:cookie_name()),
    [begin
         application:set_env(nova, session_cookie_name, Unusable),
         ?assertEqual(~"session_id", nova_session:cookie_name())
     end || Unusable <- [<<>>, "sid", sid]].

%% The session_id cookie of a Nova that named it so is no session.
old_name() ->
    ?assertEqual(undefined, nova_session:presented_session_id(cookie_req(~"session_id=before"))),
    ?assertEqual(~"new", nova_session:presented_session_id(cookie_req(~"session_id=before; __Host-session_id=new"))).

%% A cookie of the same name that is set for a parent domain or another path
%% arrives next to ours, and neither is trusted.
ambiguous_cookie() ->
    Req = cookie_req(~"__Host-session_id=one; __Host-session_id=two"),
    ?assertEqual(undefined, nova_session:presented_session_id(Req)),
    ?assertEqual({error, not_found}, nova_session:get(Req, ~"k")),
    ?assertNot(nova_once:first({nova_session, ambiguous_cookie_warned})).

repeated_cookie() ->
    Req = cookie_req(~"__Host-session_id=same; __Host-session_id=same"),
    ?assertEqual(~"same", nova_session:presented_session_id(Req)),
    ?assert(nova_once:first({nova_session, ambiguous_cookie_warned})).

%% A listener without TLS is where the cookie needs https, unless Secure was
%% taken off the cookie or sessions are off.
needs_https() ->
    ?assert(nova_session:needs_https()),
    application:set_env(nova, session_cookie_name, ~"session_id"),
    ?assert(nova_session:needs_https()),
    application:set_env(nova, session_cookie_opts, #{secure => false}),
    ?assertNot(nova_session:needs_https()),
    application:unset_env(nova, session_cookie_name),
    application:unset_env(nova, session_cookie_opts),
    application:set_env(nova, environment, dev),
    ?assertNot(nova_session:needs_https()),
    application:unset_env(nova, environment),
    application:set_env(nova, use_sessions, false),
    try
        ?assertNot(nova_session:needs_https())
    after
        application:unset_env(nova, use_sessions),
        application:unset_env(nova, session_cookie_opts)
    end.

cookie_req(Cookie) ->
    nova_test_helper:with_header(~"cookie", Cookie, nova_test_helper:mock_req(~"GET", ~"/")).

%%--------------------------------------------------------------------
%% validate_cookie_config/0
%%--------------------------------------------------------------------

validate_test_() ->
    {foreach,
     fun() ->
             application:set_env(nova, bootstrap_application, nova),
             application:unset_env(nova, environment),
             application:unset_env(nova, session_cookie_name),
             application:unset_env(nova, session_cookie_opts)
     end,
     fun(_) ->
             application:unset_env(nova, environment),
             application:unset_env(nova, session_cookie_name),
             application:unset_env(nova, session_cookie_opts),
             application:unset_env(nova, bootstrap_application)
     end,
     [
      fun valid_defaults/0,
      fun valid_name/0,
      fun bad_names/0,
      fun bad_values/0,
      fun prefix_conflicts/0,
      fun prefix_fits/0,
      fun dropped_opts/0,
      fun opts_not_a_map/0,
      fun same_site_none/0,
      fun secure_prefix/0
     ]}.

valid_defaults() ->
    ?assertEqual(ok, nova_session:validate_cookie_config()),
    application:set_env(nova, environment, dev),
    ?assertEqual(ok, nova_session:validate_cookie_config()).

valid_name() ->
    application:set_env(nova, session_cookie_name, ~"a!#$%&'*+-.^_`|~9"),
    application:set_env(nova, session_cookie_opts, #{path => ~"/app", domain => ~"example.com"}),
    ?assertEqual(ok, nova_session:validate_cookie_config()).

bad_names() ->
    [begin
         application:set_env(nova, session_cookie_name, Name),
         ?assertError({invalid_session_cookie_config, #{name := Name, path := ~"/", domain := undefined}},
                      nova_session:validate_cookie_config())
     end || Name <- [<<>>, "session", session_id, ~"app:sid", ~"a/b", ~"a\"b", ~"a b", <<"s", 200>>]].

%% A control character or a semicolon in Path or Domain would end the
%% attribute and start another one, or another header.
bad_values() ->
    application:set_env(nova, session_cookie_name, ~"sid"),
    [begin
         application:set_env(nova, session_cookie_opts, Opts),
         Path = maps:get(path, Opts, ~"/"),
         Domain = maps:get(domain, Opts, undefined),
         ?assertError({invalid_session_cookie_config, #{name := ~"sid", path := Path, domain := Domain}},
                      nova_session:validate_cookie_config())
     end || Opts <- [#{path => ~"/a\nb"},
                     #{path => ~"/a;b"},
                     #{domain => ~"a\r\nset-cookie: x=y"},
                     #{domain => <<"ex", 200, ".com">>},
                     #{domain => <<"a", 127>>, path => ~"/app"}]].

%% The default name has the __Host- prefix outside dev, and what it cannot
%% have would be dropped without a word.
prefix_conflicts() ->
    [begin
         application:set_env(nova, session_cookie_opts, Opts),
         ?assertError({invalid_setting, session_cookie_opts, {requires, session_cookie_name}},
                      nova_session:validate_cookie_config())
     end || Opts <- [#{domain => ~"example.com"}, #{path => ~"/app"}, #{secure => false}]].

prefix_fits() ->
    application:set_env(nova, session_cookie_opts,
                        #{path => ~"/", secure => true, same_site => strict, max_age => 60}),
    ?assertEqual(ok, nova_session:validate_cookie_config()),
    Opts = #{domain => ~"example.com", path => ~"/app", secure => false},
    application:set_env(nova, session_cookie_opts, Opts),
    application:set_env(nova, session_cookie_name, ~"session_id"),
    ?assertEqual(ok, nova_session:validate_cookie_config()),
    ?assertMatch(#{domain := ~"example.com", path := ~"/app", secure := false},
                 nova_session:cookie_opts(#{scheme => ~"http"})),
    application:unset_env(nova, session_cookie_name),
    application:set_env(nova, environment, dev),
    ?assertEqual(ok, nova_session:validate_cookie_config()).

%% What cookie_opts/1 leaves out is refused when Nova starts.
dropped_opts() ->
    [begin
         application:set_env(nova, session_cookie_opts, Opts),
         ?assertError({invalid_setting, session_cookie_opts, {not_taken, Keys}},
                      nova_session:validate_cookie_config())
     end || {Opts, Keys} <- [{#{same_site => ~"strict"}, [same_site]},
                             {#{same_site => sideways}, [same_site]},
                             {#{path => "/app"}, [path]},
                             {#{domain => 'example.com'}, [domain]},
                             {#{secure => yes}, [secure]},
                             {#{http_only => 0}, [http_only]},
                             {#{max_age => -1}, [max_age]},
                             {#{~"path" => ~"/"}, [~"path"]},
                             {#{unknown => 1, path => ~"/", max_age => infinity}, [max_age, unknown]}]].

opts_not_a_map() ->
    application:set_env(nova, session_cookie_opts, [{secure, true}]),
    ?assertError({invalid_setting, session_cookie_opts, not_a_map}, nova_session:validate_cookie_config()).

%% A browser drops a SameSite=None cookie that is not Secure.
same_site_none() ->
    ?assertEqual(ok, validate(#{same_site => none})),
    application:set_env(nova, session_cookie_name, ~"sid"),
    ?assertEqual(ok, validate(#{same_site => none})),
    ?assertEqual(ok, validate(#{same_site => none, secure => true})),
    ?assertError({invalid_setting, session_cookie_opts, {requires, secure}},
                 validate(#{same_site => none, secure => false})),
    application:set_env(nova, environment, dev),
    ?assertEqual(ok, validate(#{same_site => none, secure => true})),
    ?assertError({invalid_setting, session_cookie_opts, {requires, secure}}, validate(#{same_site => none})).

%% And one with the __Secure- prefix that is not Secure.
secure_prefix() ->
    application:set_env(nova, session_cookie_name, ~"__Secure-sid"),
    ?assertEqual(ok, validate(#{})),
    ?assertEqual(ok, validate(#{secure => true, domain => ~"example.com"})),
    ?assertError({invalid_setting, session_cookie_name, {requires, secure}}, validate(#{secure => false})),
    application:set_env(nova, environment, dev),
    ?assertEqual(ok, validate(#{secure => true})),
    ?assertError({invalid_setting, session_cookie_name, {requires, secure}}, validate(#{})).

validate(Opts) ->
    application:set_env(nova, session_cookie_opts, Opts),
    nova_session:validate_cookie_config().

%% What nova_stream_h asks before it puts the cookie on a response.
valid_cookie_test_() ->
    [?_assert(nova_session:valid_cookie(~"__Host-session_id", #{path => ~"/", domain => ~"example.com"})),
     ?_assert(nova_session:valid_cookie(~"a!#$%&'*+-.^_`|~9", #{})),
     ?_assertNot(nova_session:valid_cookie(<<>>, #{})),
     ?_assertNot(nova_session:valid_cookie("session_id", #{})),
     ?_assertNot(nova_session:valid_cookie(session_id, #{})),
     ?_assertNot(nova_session:valid_cookie(~"app:sid", #{})),
     ?_assertNot(nova_session:valid_cookie(~"sid", #{path => ~"/a\nb"})),
     ?_assertNot(nova_session:valid_cookie(~"sid", #{domain => <<"a", 127>>}))].
