-module(nova_log_filter_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% redact_params/2
%%====================================================================

redacts_password_test() ->
    Params = #{<<"username">> => <<"alice">>, <<"password">> => <<"s3cret">>},
    Result = nova_log_filter:redact_params(Params, [<<"password">>]),
    ?assertEqual(<<"alice">>, maps:get(<<"username">>, Result)),
    ?assertEqual(<<"[FILTERED]">>, maps:get(<<"password">>, Result)).

redacts_case_insensitive_test() ->
    Params = #{<<"Password">> => <<"s3cret">>, <<"API_KEY">> => <<"abc">>},
    Result = nova_log_filter:redact_params(Params, [<<"password">>, <<"api_key">>]),
    ?assertEqual(<<"[FILTERED]">>, maps:get(<<"Password">>, Result)),
    ?assertEqual(<<"[FILTERED]">>, maps:get(<<"API_KEY">>, Result)).

redacts_nested_maps_test() ->
    Params = #{<<"user">> => #{<<"name">> => <<"bob">>, <<"token">> => <<"xyz">>}},
    Result = nova_log_filter:redact_params(Params, [<<"token">>]),
    ?assertEqual(<<"[FILTERED]">>, maps:get(<<"token">>, maps:get(<<"user">>, Result))),
    ?assertEqual(<<"bob">>, maps:get(<<"name">>, maps:get(<<"user">>, Result))).

redacts_atom_keys_test() ->
    Params = #{password => <<"s3cret">>, username => <<"alice">>},
    Result = nova_log_filter:redact_params(Params, [<<"password">>]),
    ?assertEqual(<<"[FILTERED]">>, maps:get(password, Result)),
    ?assertEqual(<<"alice">>, maps:get(username, Result)).

redacts_partial_match_test() ->
    Params = #{<<"password_hash">> => <<"abc123">>},
    Result = nova_log_filter:redact_params(Params, [<<"password">>]),
    ?assertEqual(<<"[FILTERED]">>, maps:get(<<"password_hash">>, Result)).

leaves_non_matching_keys_test() ->
    Params = #{<<"email">> => <<"a@b.com">>, <<"name">> => <<"alice">>},
    Result = nova_log_filter:redact_params(Params, [<<"password">>]),
    ?assertEqual(Params, Result).

%%====================================================================
%% filter/2 (OTP logger callback)
%%====================================================================

filter_redacts_report_test() ->
    meck:new(nova, [passthrough]),
    try
        meck:expect(nova, get_env,
                    fun(filter_parameters, _Default) -> [<<"password">>] end),
        Event = #{level => info,
                  msg => {report, #{password => <<"s3cret">>, user => <<"bob">>}},
                  meta => #{}},
        #{msg := {report, Filtered}} = nova_log_filter:filter(Event, #{}),
        ?assertEqual(<<"[FILTERED]">>, maps:get(password, Filtered)),
        ?assertEqual(<<"bob">>, maps:get(user, Filtered))
    after
        meck:unload(nova)
    end.

filter_passes_non_report_test() ->
    Event = #{level => info, msg => {string, "hello"}, meta => #{}},
    ?assertEqual(Event, nova_log_filter:filter(Event, #{})).
