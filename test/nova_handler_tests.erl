-module(nova_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, render_error_pages, false),
    %% Set up dispatch table for render_response/3
    Tree = nova_routing_trie:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_use_stacktrace, false),
    %% Start nova_handlers gen_server (registers ETS table + default handlers)
    {ok, Pid} = nova_handlers:start_link(),
    %% Need meck for cowboy_req:reply
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, reply, fun(Status, Req) ->
        Req#{resp_status_code => Status}
    end),
    meck:expect(cowboy_req, reply, fun(Status, Headers, Body, Req) ->
        Req#{resp_status_code => Status, resp_headers => Headers, resp_body => Body}
    end),
    meck:expect(cowboy_req, set_resp_header, fun(Name, Value, Req) ->
        Headers = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => Headers#{Name => Value}}
    end),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    meck:unload(cowboy_req),
    gen_server:stop(Pid),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_use_stacktrace),
    nova_test_helper:cleanup_nova_env(Prev).

%%====================================================================
%% execute/2 — callback path (normal controller)
%%====================================================================

execute_json_return_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> {json, #{hello => world}} end,
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(200, maps:get(resp_status_code, Req1))
    end}.

execute_status_return_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> {status, 404, #{}, <<"not found">>} end,
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(404, maps:get(resp_status_code, Req1))
    end}.

execute_redirect_return_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> {redirect, <<"/login">>} end,
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(302, maps:get(resp_status_code, Req1))
    end}.

execute_controller_crash_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> error(intentional_crash) end,
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

execute_controller_throw_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> throw({403, forbidden}) end,
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(403, maps:get(resp_status_code, Req1))
    end}.

execute_redirect_to_invalid_location_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> {redirect, ~"/a\r\nset-cookie: x=1"} end,
        Req = nova_test_helper:mock_req(~"GET", ~"/"),
        Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertNot(maps:is_key(~"location", maps:get(resp_headers, Req1, #{})))
    end}.

execute_controller_crash_with_stacktrace_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:put(nova_use_stacktrace, true),
        Callback = fun(_Req) -> error(boom) end,
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        CrashInfo = maps:get(crash_info, Req1),
        ?assert(maps:is_key(stacktrace, CrashInfo))
    end}.

%%====================================================================
%% execute/2 — cowboy_handler path
%%====================================================================

execute_cowboy_handler_ok_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_cowboy_handler, [non_strict, no_link]),
        meck:expect(test_cowboy_handler, init, fun(Req, _Args) -> {ok, Req, #{}} end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{cowboy_handler => test_cowboy_handler, arguments => #{}, app => test_app},
        {ok, Req1, Env1} = nova_handler:execute(Req, Env),
        ?assertMatch(Req1 when is_map(Req1), Req1),
        ?assert(maps:is_key(result, Env1)),
        meck:unload(test_cowboy_handler)
    end}.

execute_cowboy_handler_crash_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_cowboy_handler2, [non_strict, no_link]),
        meck:expect(test_cowboy_handler2, init, fun(_Req, _Args) -> error(crash) end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{cowboy_handler => test_cowboy_handler2, arguments => #{}, app => test_app},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(404, maps:get(resp_status_code, Req1)),
        meck:unload(test_cowboy_handler2)
    end}.

execute_cowboy_handler_crash_with_stacktrace_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:put(nova_use_stacktrace, true),
        meck:new(test_cowboy_handler3, [non_strict, no_link]),
        meck:expect(test_cowboy_handler3, init, fun(_Req, _Args) -> error(boom) end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{cowboy_handler => test_cowboy_handler3, arguments => #{}, app => test_app},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(404, maps:get(resp_status_code, Req1)),
        CrashInfo = maps:get(crash_info, Req1),
        ?assert(maps:is_key(stacktrace, CrashInfo)),
        meck:unload(test_cowboy_handler3)
    end}.

execute_cowboy_handler_upgrade_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_upgrade_handler, [non_strict, no_link]),
        meck:expect(test_upgrade_handler, init, fun(Req, _Args) ->
            {test_upgrade_mod, Req, my_state}
        end),
        meck:new(test_upgrade_mod, [non_strict, no_link]),
        meck:expect(test_upgrade_mod, upgrade, fun(Req, Env, _Handler, _State) ->
            {ok, Req, Env}
        end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{cowboy_handler => test_upgrade_handler, arguments => #{}},
        {ok, _Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assert(meck:called(test_upgrade_mod, upgrade, '_')),
        meck:unload(test_upgrade_handler),
        meck:unload(test_upgrade_mod)
    end}.

execute_cowboy_handler_upgrade_with_opts_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_upgrade_handler4, [non_strict, no_link]),
        meck:expect(test_upgrade_handler4, init, fun(Req, _Args) ->
            {test_upgrade_mod4, Req, my_state, my_opts}
        end),
        meck:new(test_upgrade_mod4, [non_strict, no_link]),
        meck:expect(test_upgrade_mod4, upgrade, fun(Req, Env, _Handler, _State, _Opts) ->
            {ok, Req, Env}
        end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{cowboy_handler => test_upgrade_handler4, arguments => #{}},
        {ok, _Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assert(meck:called(test_upgrade_mod4, upgrade, '_')),
        meck:unload(test_upgrade_handler4),
        meck:unload(test_upgrade_mod4)
    end}.

%%====================================================================
%% terminate/3
%%====================================================================

terminate_with_callback_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual(ok, nova_handler:terminate(normal, #{}, fun erlang:date/0))
    end}.


%% terminate/3 is checked for and called with three arguments.
terminate_called_with_three_arguments_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Env = #{callback => fun nova_handler_test_controller:crash/1, app => test_app,
                secure => false, controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertEqual({terminated, boom, #{}}, receive {terminated, _, _} = M -> M after 0 -> none end)
    end}.

cowboy_handler_terminate_gets_its_state_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_cowboy_terminate, [non_strict, no_link]),
        Self = self(),
        meck:expect(test_cowboy_terminate, init, fun(Req, _Args) -> {ok, Req, handler_state} end),
        meck:expect(test_cowboy_terminate, terminate,
                    fun(Reason, _Req, State) -> Self ! {terminated, Reason, State}, ok end),
        Env = #{cowboy_handler => test_cowboy_terminate, arguments => #{}, app => test_app},
        {ok, _Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
        ?assertEqual({terminated, normal, handler_state}, receive {terminated, _, _} = M -> M after 1000 -> none end),
        meck:unload(test_cowboy_terminate)
    end}.

%% A non-tuple return used to raise badarg in element/2 and never reach the
%% fallback controller.
non_tuple_return_reaches_fallback_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Env = #{callback => fun nova_handler_test_controller:atom_return/1, app => test_app,
                secure => false, controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
        ?assertEqual(409, maps:get(resp_status_code, Req1))
    end}.
