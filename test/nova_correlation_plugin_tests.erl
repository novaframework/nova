-module(nova_correlation_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

logger_key(Options) ->
    Req = #{headers => #{}, resp_headers => #{}},
    try
        {ok, #{correlation_id := Id}, state} = nova_correlation_plugin:pre_security(Req, #{}, Options, state),
        case logger:get_process_metadata() of
            undefined -> [];
            Metadata -> [Key || Key := Value <- Metadata, Value =:= Id]
        end
    after
        logger:unset_process_metadata()
    end.

logger_metadata_key_is_an_atom_test_() ->
    [?_assertEqual(['correlation-id'], logger_key(#{})),
     ?_assertEqual([request_id], logger_key(#{logger_metadata_key => request_id})),
     ?_assertEqual(['correlation-id'], logger_key(#{logger_metadata_key => ~"correlation-id"})),
     ?_assertEqual([trace_ref], logger_key(#{logger_metadata_key => ~"trace_ref"}))].

logger_metadata_key_naming_no_atom_test() ->
    Name = ~"nova correlation key that names no atom",
    Options = #{logger_metadata_key => Name},
    try
        {Key, [{error, _Msg}]} = nova_test_log:collect(fun() -> logger_key(Options) end),
        ?assertEqual(['correlation-id'], Key),
        ?assertEqual({['correlation-id'], []}, nova_test_log:collect(fun() -> logger_key(Options) end))
    after
        persistent_term:erase({nova_correlation_plugin, logger_metadata_key, Name})
    end.
