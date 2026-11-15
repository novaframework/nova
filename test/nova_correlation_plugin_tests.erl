-module(nova_correlation_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OPTIONS, #{request_correlation_header => ~"X-Correlation-ID"}).

correlation_id(Value) ->
    Req = #{headers => #{~"x-correlation-id" => Value}, resp_headers => #{}},
    try
        {ok, #{correlation_id := Id, resp_headers := #{~"x-correlation-id" := Id}}, state} =
            nova_correlation_plugin:pre_request(Req, #{}, ?OPTIONS, state),
        #{'correlation-id' := Id} = logger:get_process_metadata(),
        Id
    after
        logger:unset_process_metadata()
    end.

is_generated(Id) ->
    byte_size(Id) =:= 36.

client_id_is_kept_test_() ->
    [?_assertEqual(Id, correlation_id(Id))
     || Id <- [~"EX123",
               ~"0af7651916cd43dd8448eb211c80319c",
               ~"Root=1-67891233-abcdef012345678912345678;Sampled=1",
               ~"dGVzdA==/+",
               binary:copy(~"a", 255)]].

client_id_is_replaced_test_() ->
    [?_assert(is_generated(correlation_id(Id)))
     || Id <- [<<>>,
               <<255, 0>>,
               ~"two words",
               ~"tab\there",
               <<"r", 228, "ka">>,
               ~"räka",
               <<"del", 127>>,
               binary:copy(~"a", 256)]].

missing_header_generates_an_id_test() ->
    try
        {ok, #{correlation_id := Id}, state} =
            nova_correlation_plugin:pre_request(#{headers => #{}, resp_headers => #{}}, #{}, ?OPTIONS, state),
        ?assert(is_generated(Id))
    after
        logger:unset_process_metadata()
    end.

no_header_configured_ignores_the_client_test() ->
    try
        Req = #{headers => #{~"x-correlation-id" => ~"EX123"}, resp_headers => #{}},
        {ok, #{correlation_id := Id}, state} = nova_correlation_plugin:pre_request(Req, #{}, #{}, state),
        ?assert(is_generated(Id))
    after
        logger:unset_process_metadata()
    end.

logger_key(Options) ->
    Req = #{headers => #{}, resp_headers => #{}},
    try
        {ok, #{correlation_id := Id}, state} = nova_correlation_plugin:pre_request(Req, #{}, Options, state),
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
