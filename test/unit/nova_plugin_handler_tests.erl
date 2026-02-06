-module(nova_plugin_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

plugin_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Empty plugin list", fun test_empty_plugins/0},
      {"Plugin returns ok", fun test_plugin_ok/0},
      {"Plugin returns ok with reply", fun test_plugin_ok_with_reply/0},
      {"Plugin returns break", fun test_plugin_break/0},
      {"Plugin returns break with reply", fun test_plugin_break_with_reply/0},
      {"Plugin returns stop", fun test_plugin_stop/0},
      {"Plugin returns stop with reply", fun test_plugin_stop_with_reply/0},
      {"Multiple plugins in sequence", fun test_multiple_plugins/0},
      {"Plugin crash handling", fun test_plugin_crash/0},
      {"Post-request plugins", fun test_post_request_plugins/0}
     ]}.

setup() ->
    %% Start plugin manager
    {ok, _Pid} = nova_plugin_manager:start_link(),

    %% Register test plugin
    nova_plugin_manager:add_plugin(test_plugin, fun test_plugin:init/1, #{}),

    %% Set up dispatch backend
    application:set_env(nova, dispatch_backend, persistent_term),

    ok.

cleanup(_) ->
    %% Stop plugin manager
    case whereis(nova_plugin_manager) of
        undefined -> ok;
        Pid -> exit(Pid, normal)
    end,

    %% Clean up persistent_term
    catch persistent_term:erase(nova_dispatch),

    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_empty_plugins() ->
    %% Test with no plugins - request without plugins key should just pass through
    Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}},
    Env = #{},

    {ok, Req1, Env1} = nova_plugin_handler:execute(Req, Env),

    %% Should just pass through unchanged
    ?assertEqual(Req, Req1),
    ?assertEqual(Env, Env1).

test_plugin_ok() ->
    %% Test plugin that returns {ok, Req, State}
    Plugin = {fun test_plugin:pre_request/4, #{action => ok}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, Env),

    %% Plugin should have modified the request
    ?assertEqual(true, maps:get(plugin_ok, Req1)).

test_plugin_ok_with_reply() ->
    %% Test plugin that returns {ok, Reply, Req, State}
    Plugin = {fun test_plugin:pre_request/4, #{action => ok_with_reply}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, Env),

    %% Reply should have been added to request
    ?assertEqual(200, maps:get(resp_status_code, Req1)),
    ?assertEqual(<<"Plugin data">>, maps:get(resp_body, Req1)),

    %% Headers should be set
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"active">>, maps:get(<<"x-plugin">>, Headers)).

test_plugin_break() ->
    %% Test plugin that returns {break, Req, State}
    Plugin = {fun test_plugin:pre_request/4, #{action => break}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    %% Break should stop plugin chain but return ok
    {ok, Req1} = nova_plugin_handler:execute(Req, Env),

    %% Plugin should have modified the request
    ?assertEqual(true, maps:get(plugin_break, Req1)).

test_plugin_break_with_reply() ->
    %% Test plugin that returns {break, Reply, Req, State}
    Plugin = {fun test_plugin:pre_request/4, #{action => break_with_reply}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    {ok, Req1} = nova_plugin_handler:execute(Req, Env),

    %% Should have set response status and body
    ?assertEqual(403, maps:get(resp_status_code, Req1)),
    ?assertEqual(<<"Forbidden">>, maps:get(resp_body, Req1)).

test_plugin_stop() ->
    %% Test plugin that returns {stop, Req, State}
    Plugin = {fun test_plugin:pre_request/4, #{action => stop}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    %% Stop should return {stop, Req}
    {stop, Req1} = nova_plugin_handler:execute(Req, Env),

    %% Plugin should have modified the request
    ?assertEqual(true, maps:get(plugin_stop, Req1)).

test_plugin_stop_with_reply() ->
    %% Test plugin that returns {stop, Reply, Req, State}
    %% Note: This test uses meck to mock cowboy_req:reply
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, reply, fun(StatusCode, Req) -> Req#{replied => StatusCode} end),

    Plugin = {fun test_plugin:pre_request/4, #{action => stop_with_reply}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    {stop, Req1} = nova_plugin_handler:execute(Req, Env),

    %% Should have set response
    ?assertEqual(401, maps:get(resp_status_code, Req1)),
    ?assertEqual(<<"Unauthorized">>, maps:get(resp_body, Req1)),

    %% Verify cowboy_req:reply was called
    ?assert(meck:called(cowboy_req, reply, [401, '_'])),

    meck:unload(cowboy_req).

test_multiple_plugins() ->
    %% Test multiple plugins executing in sequence
    Plugin1 = {fun test_plugin:pre_request/4, #{action => ok}},
    Plugin2 = {fun(Req, _Env, _Opts, State) ->
                       {ok, Req#{second_plugin => true}, State}
               end, #{}},
    Req = #{plugins => [{pre_request, [Plugin1, Plugin2]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, Env),

    %% Both plugins should have executed
    ?assertEqual(true, maps:get(plugin_ok, Req1)),
    ?assertEqual(true, maps:get(second_plugin, Req1)).

test_plugin_crash() ->
    %% Test plugin crash handling
    %% Mock the router for status page rendering
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),

    Plugin = {fun test_plugin:pre_request/4, #{action => crash}},
    Req = #{plugins => [{pre_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{},

    %% Should catch the crash and render error page
    {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, Env),

    %% Should have crash_info set
    ?assertMatch(#{crash_info := #{class := error,
                                   reason := plugin_crash}}, Req1).

test_post_request_plugins() ->
    %% Test post-request plugins
    Plugin = {fun test_plugin:post_request/4, #{}},
    Req = #{plugins => [{post_request, [Plugin]}],
            method => <<"GET">>,
            path => <<"/">>,
            headers => #{}},
    Env = #{plugin_state => pre_request},

    {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, Env),

    %% Post-request plugin should have executed
    ?assertEqual(true, maps:get(post_plugin_executed, Req1)).
