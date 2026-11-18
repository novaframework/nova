-module(nova_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-export([forward_event/4, no_status_action/1, no_status_handler/3, crashing_action/1, error_page/1]).
-include("../include/nova_router.hrl").

-define(SETUP, fun setup/0).
-define(CLEANUP, fun cleanup/1).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    %% Set up dispatch table for render_response/3
    Tree = nova_routing_trie:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_use_stacktrace, false),
    %% Start nova_handlers gen_server (registers ETS table + default handlers)
    {ok, Pid} = nova_handlers:start_link(),
    %% Need meck for nova_req:reply
    meck:new(nova_req, [passthrough, no_link]),
    meck:expect(nova_req, reply, fun nova_test_helper:reply/2),
    meck:expect(nova_req, reply, fun(Status, Headers, Body, Req) ->
        Req#{resp_status_code => Status, resp_headers => Headers, resp_body => Body}
    end),
    meck:expect(nova_req, set_resp_header, fun(Name, Value, Req) ->
        Headers = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => Headers#{Name => Value}}
    end),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    meck:unload(nova_req),
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

%% Cowboy refuses a body with these two, which made the response a 500.
execute_status_without_content_drops_the_body_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        [begin
             Callback = fun(_Req) -> {status, Status, #{}, ~"body"} end,
             Req = nova_test_helper:mock_req(~"GET", ~"/"),
             Env = #{callback => Callback, app => test_app, secure => false, controller_data => #{}},
             {ok, Req1, _Env1} = nova_handler:execute(Req, Env),
             ?assertEqual({Status, false}, {maps:get(resp_status_code, Req1), is_map_key(resp_body, Req1)})
         end || Status <- [204, 304]]
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
        ?assertMatch(#{crash_info := #{stacktrace := _}}, Req1)
    end}.

%%====================================================================
%% execute/2 — handler_module path
%%====================================================================

execute_module_handler_ok_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_module_handler, [non_strict, no_link]),
        meck:expect(test_module_handler, init, fun(Req, _Args) -> {ok, Req, #{}} end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{handler_module => test_module_handler, arguments => #{}, app => test_app},
        {ok, Req1, Env1} = nova_handler:execute(Req, Env),
        ?assertMatch(Req1 when is_map(Req1), Req1),
        ?assert(maps:is_key(result, Env1)),
        meck:unload(test_module_handler)
    end}.

execute_module_handler_crash_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_module_handler2, [non_strict, no_link]),
        meck:expect(test_module_handler2, init, fun(_Req, _Args) -> error(crash) end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{handler_module => test_module_handler2, arguments => #{}, app => test_app},
        {ok, Req1, Env1} = nova_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertMatch(#{crash_info := #{status_code := 500}}, Req1),
        ?assertNot(maps:is_key(handler_module, Env1)),
        meck:unload(test_module_handler2)
    end}.

execute_module_handler_crash_with_stacktrace_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:put(nova_use_stacktrace, true),
        meck:new(test_module_handler3, [non_strict, no_link]),
        meck:expect(test_module_handler3, init, fun(_Req, _Args) -> error(boom) end),
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/ws">>),
        Env = #{handler_module => test_module_handler3, arguments => #{}, app => test_app},
        {ok, Req1, Env1} = nova_handler:execute(Req, Env),
        ?assertNot(maps:is_key(handler_module, Env1)),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertMatch(#{crash_info := #{stacktrace := _, status_code := 500}}, Req1),
        meck:unload(test_module_handler3)
    end}.

execute_websocket_upgrade_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_upgrade_handler, [non_strict, no_link]),
        meck:expect(test_upgrade_handler, init, fun(Req, _Args) ->
            {websocket, Req, my_state}
        end),
        Req = nova_fake_server:req(#{path => ~"/ws"}),
        Env = #{handler_module => test_upgrade_handler, arguments => #{}},
        {ok, _Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual([{upgrade_websocket, test_upgrade_handler, my_state, #{}, []}], nova_fake_server:sent()),
        meck:unload(test_upgrade_handler)
    end}.

upgrade_opts_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_upgrade_handler4, [non_strict, no_link]),
        meck:expect(test_upgrade_handler4, init, fun(Req, _Args) ->
            {websocket, Req, my_state, #{idle_timeout => 1000}}
        end),
        Req = nova_fake_server:req(#{path => ~"/ws"}),
        Env = #{handler_module => test_upgrade_handler4, arguments => #{}},
        {ok, _Req1, _Env1} = nova_handler:execute(Req, Env),
        ?assertEqual([{upgrade_websocket, test_upgrade_handler4, my_state, #{idle_timeout => 1000}, []}],
                     nova_fake_server:sent()),
        meck:unload(test_upgrade_handler4)
    end}.

%%====================================================================
%% terminate/3
%%====================================================================

terminate_with_callback_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(~"GET", ~"/"),
        ?assertEqual(ok, nova_handler:terminate(normal, Req, fun erlang:date/0))
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

module_handler_terminate_gets_its_state_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(test_module_terminate, [non_strict, no_link]),
        Self = self(),
        meck:expect(test_module_terminate, init, fun(Req, _Args) -> {ok, Req, handler_state} end),
        meck:expect(test_module_terminate, terminate,
                    fun(Reason, _Req, State) -> Self ! {terminated, Reason, State}, ok end),
        Env = #{handler_module => test_module_terminate, arguments => #{}, app => test_app},
        {ok, _Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
        ?assertEqual({terminated, normal, handler_state}, receive {terminated, _, _} = M -> M after 1000 -> none end),
        meck:unload(test_module_terminate)
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
      fun() -> with_handler(fun controller_telemetry_opt_in_request_fields/0) end,
      fun() -> with_handler(fun controller_telemetry_exception/0) end,
      fun() -> with_handler(fun controller_telemetry_exception_reason_is_a_tag/0) end,
      {timeout, 1, fun() -> with_handler(fun controller_telemetry_status_page/0) end},
      fun controller_unsupported_return/0,
      fun controller_return_for_a_websocket/0]}.

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
    meck:expect(nova_req, reply, fun(_Status, R) -> R end),
    try
        {ok, _Req1, _Env1} = nova_handler:execute(Req, Env)
    after
        meck:expect(nova_req, reply, fun nova_test_helper:reply/2)
    end,
    Expected = #{method => ~"GET", host => ~"localhost", scheme => ~"https",
                 version => 'HTTP/2', app => test_app,
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

controller_telemetry_opt_in_request_fields() ->
    Req = (nova_test_helper:mock_req(~"GET", ~"/reset/secret-token"))#{peer => {{127, 0, 0, 1}, 4000}},
    Env = #{callback => fun(_) -> {status, 204} end, app => test_app, secure => false, controller_data => #{}},
    Keys = fun(Configured) ->
                   application:set_env(nova, telemetry_request_fields, Configured),
                   {ok, _Req1, _Env1} = nova_handler:execute(Req, Env),
                   {[nova, controller, start], _, Start} = next_event(),
                   {[nova, controller, stop], _, Stop} = next_event(),
                   ?assertEqual(Start#{status => 204}, Stop),
                   maps:with([path, peer, headers, pid], Start)
           end,
    try
        ?assertEqual(#{path => ~"/reset/secret-token", peer => {{127, 0, 0, 1}, 4000}}, Keys([path, peer])),
        ?assertEqual(#{peer => {{127, 0, 0, 1}, 4000}}, Keys([peer])),
        ?assertEqual(#{path => ~"/reset/secret-token"}, Keys([headers, path, pid, ~"peer"])),
        ?assertEqual(#{}, Keys([])),
        ?assertEqual(#{}, Keys(path))
    after
        application:unset_env(nova, telemetry_request_fields)
    end.

controller_telemetry_exception_reason_is_a_tag() ->
    Env = #{callback => fun(Req) -> ok = Req end, app => test_app, secure => false, controller_data => #{}},
    Req = (nova_test_helper:mock_req(~"GET", ~"/"))#{headers => #{~"cookie" => ~"session_id=do-not-export-me"}},
    {ok, _Req1, _Env1} = nova_handler:execute(Req, Env),
    {[nova, controller, start], _, _} = next_event(),
    {[nova, controller, exception], _, Exception} = next_event(),
    ?assertMatch(#{kind := error, reason := badmatch, status_page := false}, Exception),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~p", [Exception])), ~"do-not-export-me")).

controller_telemetry_exception() ->
    Env = #{callback => fun ?MODULE:crashing_action/1, app => test_app, secure => false,
            controller_data => #{}},
    {ok, _Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
    {[nova, controller, start], _, _} = next_event(),
    {[nova, controller, exception], _, Exception} = next_event(),
    ?assertMatch(#{kind := error, reason := boom, action := crashing_action, status_page := false},
                 Exception).

controller_telemetry_status_page() ->
    ErrorPage = #nova_handler_value{app = test_app, callback = fun ?MODULE:error_page/1, plugins = [],
                                    secure = false, extra_state = #{}},
    {ok, Tree} = nova_routing_trie:insert('_', 500, '_', ErrorPage, persistent_term:get(nova_dispatch)),
    persistent_term:put(nova_dispatch, Tree),
    Env = #{callback => fun ?MODULE:crashing_action/1, app => test_app, secure => false,
            controller_data => #{}},
    {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
    ?assertEqual(500, maps:get(resp_status_code, Req1)),
    {[nova, controller, start], _, #{status_page := false}} = next_event(),
    {[nova, controller, exception], _, _} = next_event(),
    {[nova, controller, start], _, PageStart} = next_event(),
    ?assertMatch(#{app := nova, action := error_page, status_page := true}, PageStart),
    {[nova, controller, stop], _, PageStop} = next_event(),
    ?assertMatch(#{status := 500, status_page := true}, PageStop).

controller_unsupported_return() ->
    Env = #{callback => fun(_) -> {unsupported, value} end, app => test_app, secure => false,
            controller_data => #{}},
    {ok, Req1, _Env1} = nova_handler:execute(nova_test_helper:mock_req(~"GET", ~"/"), Env),
    ?assertEqual(500, maps:get(resp_status_code, Req1)),
    ?assertMatch(#{title := ~"Controller returned unsupported data"}, maps:get(crash_info, Req1)).

%% The handler of `ws` takes what a websocket controller returns, not a
%% return, a callback and a request.
controller_return_for_a_websocket() ->
    Env = #{callback => fun(_) -> {ws, value} end, app => test_app, secure => false,
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
