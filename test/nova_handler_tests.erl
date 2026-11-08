-module(nova_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-export([forward_event/4, no_status_action/1, no_status_handler/3, crashing_action/1, error_page/1]).
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

execute_controller_crash_keeps_route_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Callback = fun(_Req) -> error(intentional_crash) end,
        Req = (nova_test_helper:mock_req(<<"GET">>, <<"/users/1">>))#{nova_route => ~"/users/:id"},
        Env = #{callback => Callback, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertEqual(~"/users/:id", maps:get(nova_route, Req1))
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
        {ok, Req1, Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertEqual(500, maps:get(status_code, maps:get(crash_info, Req1))),
        ?assertNot(maps:is_key(cowboy_handler, Env1)),
        meck:unload(test_cowboy_handler2)
    end}.

execute_cowboy_handler_crash_with_stacktrace_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:put(nova_use_stacktrace, true),
        meck:new(test_cowboy_handler3, [non_strict, no_link]),
        meck:expect(test_cowboy_handler3, init, fun(_Req, _Args) -> error(boom) end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{cowboy_handler => test_cowboy_handler3, arguments => #{}, app => test_app},
        {ok, Req1, Env1} = nova_handler:execute(Req, Env),
        ?assertNot(maps:is_key(cowboy_handler, Env1)),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        CrashInfo = maps:get(crash_info, Req1),
        ?assert(maps:is_key(stacktrace, CrashInfo)),
        ?assertEqual(500, maps:get(status_code, CrashInfo)),
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


%%====================================================================
%% Telemetry
%%====================================================================

controller_status_telemetry_test_() ->
    {setup,
     fun() ->
             {ok, _} = application:ensure_all_started(telemetry),
             setup()
     end,
     fun cleanup/1,
     fun() ->
             Id = make_ref(),
             ok = telemetry:attach_many(Id, [[nova, controller, stop], [nova, controller, exception]],
                                        fun ?MODULE:forward_event/4, self()),
             try
                 [begin
                      Callback = fun(_Req) -> erlang:raise(Class, {Status, nope}, []) end,
                      Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
                      {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
                      ?assertEqual(Status, maps:get(resp_status_code, Req1)),
                      receive
                          {telemetry, Event, _Measurements, Metadata} ->
                              ?assertEqual([nova, controller, stop], Event),
                              ?assertEqual(Status, maps:get(status, Metadata))
                      after 1000 ->
                              error(no_event)
                      end
                  end || {Class, Status} <- [{throw, 403}, {error, 404}, {exit, 410}]]
             after
                 telemetry:detach(Id)
             end
     end}.

forward_event(Event, Measurements, Metadata, Pid) ->
    Pid ! {telemetry, Event, Measurements, Metadata}.

-define(CONTROLLER_EVENTS, [[nova, controller, Phase] || Phase <- [start, stop, exception]]).

controller_telemetry_test_() ->
    {setup,
     fun() ->
             {ok, _} = application:ensure_all_started(telemetry),
             setup()
     end,
     fun cleanup/1,
     [fun() -> with_handler(fun controller_telemetry_metadata/0) end,
      fun() -> with_handler(fun controller_telemetry_exception/0) end,
      {timeout, 1, fun() -> with_handler(fun controller_telemetry_status_page/0) end},
      fun controller_unsupported_return/0]}.

controller_telemetry_metadata() ->
    nova_handlers:register_handler(no_status, fun ?MODULE:no_status_handler/3),
    _ = sys:get_state(nova_handlers),
    Req = (nova_test_helper:mock_req(~"GET", ~"/users/1"))#{scheme => ~"https",
                                                            version => 'HTTP/2',
                                                            peer => {{127, 0, 0, 1}, 4000},
                                                            nova_route => ~"/users/:id"},
    Env = #{callback => fun ?MODULE:no_status_action/1, app => test_app, secure => false,
            controller_data => #{}},
    %% Cowboy's reply does not record the status in the Req, so the span
    %% falls back to 200 when the handler set none.
    meck:expect(cowboy_req, reply, fun(_Status, R) -> R end),
    try
        {ok, _Req1, _Env1} = nova_handler:execute(Req, Env)
    after
        meck:expect(cowboy_req, reply, fun(Status, R) -> R#{resp_status_code => Status} end)
    end,
    Expected = #{method => ~"GET", path => ~"/users/1", host => ~"localhost", scheme => ~"https",
                 version => 'HTTP/2', peer => {{127, 0, 0, 1}, 4000}, app => test_app,
                 controller => ?MODULE, action => no_status_action, route => ~"/users/:id",
                 status_page => false},
    {[nova, controller, start], _, Start} = next_event(),
    ?assertEqual(Expected, maps:remove(telemetry_span_context, Start)),
    {[nova, controller, stop], _, Stop} = next_event(),
    ?assertEqual(Expected#{status => 200}, maps:remove(telemetry_span_context, Stop)),

    Teapot = Env#{callback => fun(_) -> {status, 418} end},
    {ok, _Req2, _Env2} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Teapot),
    {[nova, controller, start], _, _} = next_event(),
    {[nova, controller, stop], _, TeapotStop} = next_event(),
    ?assertEqual(418, maps:get(status, TeapotStop)).

controller_telemetry_exception() ->
    Env = #{callback => fun ?MODULE:crashing_action/1, app => test_app, secure => false,
            controller_data => #{}},
    {ok, _Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
    {[nova, controller, start], _, _} = next_event(),
    {[nova, controller, exception], _, Exception} = next_event(),
    ?assertMatch(#{kind := error, reason := boom, action := crashing_action, status_page := false},
                 Exception).

controller_telemetry_status_page() ->
    application:set_env(nova, render_error_pages, true),
    ErrorPage = #nova_handler_value{app = test_app, callback = fun ?MODULE:error_page/1, plugins = [],
                                    secure = false, extra_state = #{}},
    {ok, Tree} = nova_routing_trie:insert('_', 500, '_', ErrorPage, persistent_term:get(nova_dispatch)),
    persistent_term:put(nova_dispatch, Tree),
    try
        Env = #{callback => fun ?MODULE:crashing_action/1, app => test_app, secure => false,
                controller_data => #{}},
        {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        {[nova, controller, start], _, #{status_page := false}} = next_event(),
        {[nova, controller, exception], _, _} = next_event(),
        {[nova, controller, start], _, PageStart} = next_event(),
        ?assertMatch(#{app := nova, action := error_page, status_page := true}, PageStart),
        {[nova, controller, stop], _, PageStop} = next_event(),
        ?assertMatch(#{status := 500, status_page := true}, PageStop)
    after
        application:set_env(nova, render_error_pages, false)
    end.

controller_unsupported_return() ->
    Env = #{callback => fun(_) -> {unsupported, value} end, app => test_app, secure => false,
            controller_data => #{}},
    {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
    ?assertEqual(500, maps:get(resp_status_code, Req1)),
    ?assertMatch(#{title := ~"Controller returned unsupported data"}, maps:get(crash_info, Req1)).

with_handler(Test) ->
    Id = make_ref(),
    ok = telemetry:attach_many(Id, ?CONTROLLER_EVENTS, fun ?MODULE:forward_event/4, self()),
    try
        Test()
    after
        telemetry:detach(Id)
    end.

next_event() ->
    receive
        {telemetry, Event, Measurements, Metadata} -> {Event, Measurements, Metadata}
    after 1000 ->
            error(no_event)
    end.

no_status_action(_Req) ->
    {no_status, ok}.

no_status_handler(_RetObj, _Callback, Req) ->
    {ok, Req}.

crashing_action(_Req) ->
    erlang:error(boom).

error_page(_Req) ->
    {status, 500}.
