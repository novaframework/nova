-module(nova_correlation_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OPTIONS, #{request_correlation_header => ~"X-Correlation-ID"}).

correlation_id(Value) ->
    Req = #{headers => #{~"x-correlation-id" => Value}, resp_headers => #{}},
    try
        {ok, #{correlation_id := Id, resp_headers := #{~"x-correlation-id" := Id}}, state} =
            nova_correlation_plugin:pre_request(Req, #{}, ?OPTIONS, state),
        #{~"correlation-id" := Id} = logger:get_process_metadata(),
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
