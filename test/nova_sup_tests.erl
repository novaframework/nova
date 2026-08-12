-module(nova_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%% The listener is a supervisor child spec, not a side effect.
child_spec_is_supervised_test() ->
    Spec = nova_sup:clear_child_spec(shape_ref, [{port, 0}], #{}),
    ?assertEqual({ranch_embedded_sup, shape_ref}, maps:get(id, Spec)),
    ?assertEqual(supervisor, maps:get(type, Spec)),
    {ranch_embedded_sup, start_link, [shape_ref, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts]} =
        maps:get(start, Spec),
    ?assertEqual(supervisor, maps:get(connection_type, TransOpts)),
    ?assertEqual(supervisor, maps:get(connection_type, ProtoOpts)).

tls_child_spec_uses_ssl_transport_test() ->
    Spec = nova_sup:tls_child_spec(tls_ref, [{port, 0}], #{}),
    {ranch_embedded_sup, start_link, [tls_ref, ranch_ssl, _, cowboy_tls, _]} = maps:get(start, Spec).

listener_lifecycle_test_() ->
    {setup,
     fun() ->
             {ok, Apps} = application:ensure_all_started(cowboy),
             Apps
     end,
     fun(Apps) -> [application:stop(A) || A <- lists:reverse(Apps)] end,
     [
      fun binds_on_free_port/0,
      fun fails_loudly_on_busy_port/0
     ]}.

%% A free port yields a live listener process.
binds_on_free_port() ->
    Spec = nova_sup:clear_child_spec(free_ref, [{port, 0}, {ip, {127, 0, 0, 1}}], #{}),
    {M, F, A} = maps:get(start, Spec),
    Result = erlang:apply(M, F, A),
    ?assertMatch({ok, _}, Result),
    {ok, Pid} = Result,
    ?assert(is_process_alive(Pid)),
    unlink(Pid),
    exit(Pid, shutdown).

%% A busy port fails the start instead of being swallowed.
fails_loudly_on_busy_port() ->
    {ok, LSock} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    Spec = nova_sup:clear_child_spec(busy_ref, [{port, Port}, {ip, {127, 0, 0, 1}}], #{}),
    {M, F, A} = maps:get(start, Spec),
    process_flag(trap_exit, true),
    Result = erlang:apply(M, F, A),
    ?assertMatch({error, _}, Result),
    gen_tcp:close(LSock).
