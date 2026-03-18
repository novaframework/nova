-module(nova_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, render_error_pages, false),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_use_stacktrace, false),
    {ok, Pid} = nova_handlers:start_link(),
    meck:new(test_ws_mod, [non_strict, no_link]),
    meck:expect(test_ws_mod, init, fun(CD) -> {ok, CD} end),
    meck:expect(test_ws_mod, websocket_init, fun(CD) -> {ok, CD} end),
    meck:expect(test_ws_mod, websocket_handle, fun(_Frame, CD) -> {ok, CD} end),
    meck:expect(test_ws_mod, websocket_info, fun(_Msg, CD) -> {ok, CD} end),
    meck:expect(test_ws_mod, terminate, fun(_Reason, _PartialReq, _CD) -> ok end),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    meck:unload(test_ws_mod),
    gen_server:stop(Pid),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_use_stacktrace),
    nova_test_helper:cleanup_nova_env(Prev).

make_req() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
    Req#{plugins => []}.

make_state() ->
    #{module => test_ws_mod, controller_data => #{}}.

make_ws_state() ->
    #{mod => test_ws_mod, controller_data => #{}, plugins => [], commands => []}.

%%====================================================================
%% init/2
%%====================================================================

init_successful_upgrade_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = make_req(),
        State = make_state(),
        Result = nova_ws_handler:init(Req, State),
        ?assertMatch({cowboy_websocket, _, _}, Result),
        {cowboy_websocket, _Req1, State1} = Result,
        ?assert(maps:is_key(controller_data, State1)),
        ?assertEqual(test_ws_mod, maps:get(mod, State1))
    end}.

init_injects_req_into_controller_data_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = make_req(),
        State = make_state(),
        {cowboy_websocket, _Req1, State1} = nova_ws_handler:init(Req, State),
        CD = maps:get(controller_data, State1),
        ?assert(maps:is_key(req, CD))
    end}.

init_module_error_renders_status_page_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, init, fun(_CD) -> {error, bad_stuff} end),
        Req = make_req(),
        State = make_state(),
        Result = nova_ws_handler:init(Req, State),
        ?assertMatch({ok, _, _}, Result),
        {ok, Req1, _Env} = Result,
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

init_preserves_existing_controller_data_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = make_req(),
        State = #{module => test_ws_mod, controller_data => #{existing => value}},
        {cowboy_websocket, _Req1, State1} = nova_ws_handler:init(Req, State),
        CD = maps:get(controller_data, State1),
        ?assertEqual(value, maps:get(existing, CD))
    end}.

init_default_controller_data_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = make_req(),
        State = #{module => test_ws_mod},
        {cowboy_websocket, _Req1, State1} = nova_ws_handler:init(Req, State),
        ?assert(maps:is_key(controller_data, State1))
    end}.

%%====================================================================
%% websocket_init/1
%%====================================================================

websocket_init_calls_module_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_init(State),
        ?assertMatch({[], _}, Result),
        ?assert(meck:called(test_ws_mod, websocket_init, '_'))
    end}.

websocket_init_injects_ws_handler_process_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = make_ws_state(),
        {_Cmds, State1} = nova_ws_handler:websocket_init(State),
        CD = maps:get(controller_data, State1),
        ?assertEqual(self(), maps:get(ws_handler_process, CD))
    end}.

websocket_init_skips_if_not_exported_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_ws_mod_no_init, [non_strict, no_link]),
        meck:expect(test_ws_mod_no_init, websocket_handle, fun(_F, CD) -> {ok, CD} end),
        State = #{mod => test_ws_mod_no_init, controller_data => #{}, plugins => []},
        Result = nova_ws_handler:websocket_init(State),
        ?assertMatch({ok, _}, Result),
        {ok, State1} = Result,
        CD = maps:get(controller_data, State1),
        ?assertEqual(self(), maps:get(ws_handler_process, CD)),
        meck:unload(test_ws_mod_no_init)
    end}.

%%====================================================================
%% websocket_handle/2
%%====================================================================

websocket_handle_ok_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = make_ws_state(),
        Frame = {text, <<"hello">>},
        Result = nova_ws_handler:websocket_handle(Frame, State),
        ?assertMatch({[], _}, Result),
        {_Cmds, State1} = Result,
        ?assert(maps:is_key(controller_data, State1))
    end}.

websocket_handle_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, websocket_handle, fun(Frame, CD) ->
            {reply, Frame, CD}
        end),
        State = make_ws_state(),
        Frame = {text, <<"echo">>},
        Result = nova_ws_handler:websocket_handle(Frame, State),
        ?assertMatch({[{text, <<"echo">>}], _}, Result)
    end}.

websocket_handle_reply_hibernate_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, websocket_handle, fun(Frame, CD) ->
            {reply, Frame, CD, hibernate}
        end),
        State = make_ws_state(),
        Frame = {text, <<"sleep">>},
        Result = nova_ws_handler:websocket_handle(Frame, State),
        ?assertMatch({[{text, <<"sleep">>}], _, hibernate}, Result)
    end}.

websocket_handle_ok_hibernate_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, websocket_handle, fun(_Frame, CD) ->
            {ok, CD, hibernate}
        end),
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_handle({text, <<"hi">>}, State),
        ?assertMatch({[], _, hibernate}, Result)
    end}.

websocket_handle_stop_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, websocket_handle, fun(_Frame, CD) ->
            {stop, CD}
        end),
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_handle({text, <<"bye">>}, State),
        ?assertMatch({stop, _}, Result)
    end}.

%%====================================================================
%% websocket_info/2
%%====================================================================

websocket_info_ok_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_info(tick, State),
        ?assertMatch({[], _}, Result)
    end}.

websocket_info_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, websocket_info, fun(_Msg, CD) ->
            {reply, {text, <<"pong">>}, CD}
        end),
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_info(ping, State),
        ?assertMatch({[{text, <<"pong">>}], _}, Result)
    end}.

%%====================================================================
%% terminate/3
%%====================================================================

terminate_calls_module_terminate_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = #{mod => test_ws_mod, controller_data => #{foo => bar},
                  plugins => []},
        Result = nova_ws_handler:terminate(normal, #{}, State),
        ?assertMatch({ok, _}, Result),
        ?assert(meck:called(test_ws_mod, terminate, [normal, #{}, #{foo => bar}]))
    end}.

terminate_runs_post_ws_connection_plugins_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_ws_plugin, [non_strict, no_link]),
        meck:expect(test_ws_plugin, post_ws_connection,
            fun(_CS, State, _Opts) -> {ok, #{}, State} end),
        State = #{mod => test_ws_mod, controller_data => #{},
                  plugins => [{post_ws_connection, [{test_ws_plugin, []}]}]},
        nova_ws_handler:terminate(normal, #{}, State),
        ?assert(meck:called(test_ws_plugin, post_ws_connection, '_')),
        meck:unload(test_ws_plugin)
    end}.

terminate_skips_if_not_exported_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_ws_mod_no_term, [non_strict, no_link]),
        State = #{mod => test_ws_mod_no_term, controller_data => #{},
                  plugins => []},
        ?assertEqual(ok, nova_ws_handler:terminate(normal, #{}, State)),
        meck:unload(test_ws_mod_no_term)
    end}.

terminate_catchall_returns_ok_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = #{some => data},
        ?assertEqual(ok, nova_ws_handler:terminate(normal, #{}, State))
    end}.

%%====================================================================
%% Controller crash in invoke_controller
%%====================================================================

controller_crash_returns_stop_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:expect(test_ws_mod, websocket_handle, fun(_Frame, _CD) ->
            error(intentional_crash)
        end),
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_handle({text, <<"boom">>}, State),
        ?assertMatch({stop, _}, Result)
    end}.

%%====================================================================
%% Plugin returning stop signal
%%====================================================================

pre_plugin_stop_signal_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_stop_plugin, [non_strict, no_link]),
        meck:expect(test_stop_plugin, pre_ws_request,
            fun(_CS, State, _Opts) -> {stop, State} end),
        State = #{mod => test_ws_mod, controller_data => #{},
                  plugins => [{pre_ws_request, [{test_stop_plugin, []}]}],
                  commands => []},
        Result = nova_ws_handler:websocket_handle({text, <<"hello">>}, State),
        ?assertMatch({stop, _}, Result),
        %% Controller should not have been called
        ?assertNot(meck:called(test_ws_mod, websocket_handle, '_')),
        meck:unload(test_stop_plugin)
    end}.

%%====================================================================
%% handle_ws with no controller_data in state
%%====================================================================

handle_ws_no_controller_data_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        State = #{mod => test_ws_mod, plugins => []},
        Result = nova_ws_handler:websocket_handle({text, <<"hi">>}, State),
        ?assertMatch({[], _}, Result),
        {_Cmds, State1} = Result,
        ?assert(maps:is_key(controller_data, State1))
    end}.

%%====================================================================
%% Plugin error handling
%%====================================================================

pre_plugin_crash_returns_stop_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_crash_plugin, [non_strict, no_link]),
        meck:expect(test_crash_plugin, pre_ws_request,
            fun(_CS, _State, _Opts) -> error(plugin_crash) end),
        State = #{mod => test_ws_mod, controller_data => #{},
                  plugins => [{pre_ws_request, [{test_crash_plugin, []}]}],
                  commands => []},
        Result = nova_ws_handler:websocket_handle({text, <<"hi">>}, State),
        ?assertMatch({stop, _}, Result),
        meck:unload(test_crash_plugin)
    end}.

pre_plugin_error_returns_stop_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_err_plugin, [non_strict, no_link]),
        meck:expect(test_err_plugin, pre_ws_request,
            fun(_CS, _State, _Opts) -> {error, bad_plugin} end),
        State = #{mod => test_ws_mod, controller_data => #{},
                  plugins => [{pre_ws_request, [{test_err_plugin, []}]}],
                  commands => []},
        Result = nova_ws_handler:websocket_handle({text, <<"hi">>}, State),
        ?assertMatch({stop, _}, Result),
        meck:unload(test_err_plugin)
    end}.

pre_plugin_break_skips_remaining_plugins_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_break_plugin, [non_strict, no_link]),
        meck:expect(test_break_plugin, pre_ws_request,
            fun(_CS, State, _Opts) -> {break, State} end),
        meck:new(test_second_plugin, [non_strict, no_link]),
        meck:expect(test_second_plugin, pre_ws_request,
            fun(_CS, State, _Opts) -> {ok, #{}, State} end),
        State = #{mod => test_ws_mod, controller_data => #{},
                  plugins => [{pre_ws_request, [{test_break_plugin, []},
                                                {test_second_plugin, []}]}],
                  commands => []},
        Result = nova_ws_handler:websocket_handle({text, <<"hi">>}, State),
        ?assertMatch({[], _}, Result),
        ?assert(meck:called(test_break_plugin, pre_ws_request, '_')),
        ?assertNot(meck:called(test_second_plugin, pre_ws_request, '_')),
        meck:unload(test_break_plugin),
        meck:unload(test_second_plugin)
    end}.

%%====================================================================
%% Handler not found
%%====================================================================

ws_handler_not_registered_returns_stop_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        nova_handlers:unregister_handler(ws),
        State = make_ws_state(),
        Result = nova_ws_handler:websocket_handle({text, <<"hi">>}, State),
        ?assertMatch({stop, _}, Result),
        %% Re-register for other tests
        nova_handlers:register_handler(ws, fun nova_basic_handler:handle_ws/2)
    end}.
