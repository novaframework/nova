-module(routing_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/nova_router.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

routing_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Full request lifecycle - simple route", fun test_simple_request_lifecycle/0},
      {"Full request lifecycle - with bindings", fun test_request_with_bindings/0},
      {"Error propagation through middleware", fun test_error_propagation/0},
      {"404 not found handling", fun test_not_found_handling/0}
     ]}.

setup() ->
    %% Set up environment
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, use_strict_routing, false),
    application:set_env(nova, json_lib, thoas),
    application:set_env(nova, render_error_pages, false),

    %% Initialize routing tree
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),

    %% Create a simple callback
    SimpleCallback = fun(Req) ->
                             {ok, #{message => <<"Hello, World!">>}}
                     end,

    %% Create callback with bindings
    BindingCallback = fun(#{bindings := Bindings} = Req) ->
                              UserId = maps:get(<<"id">>, Bindings),
                              {ok, #{user_id => UserId}}
                      end,

    %% Create routes
    Value1 = #nova_handler_value{
                app = test_app,
                callback = SimpleCallback,
                plugins = [],
                secure = false,
                extra_state = #{}
               },

    Value2 = #nova_handler_value{
                app = test_app,
                callback = BindingCallback,
                plugins = [],
                secure = false,
                extra_state = #{}
               },

    %% Insert routes
    Tree1 = nova_router:insert('_', "/hello", <<"GET">>, Value1, Tree),
    Tree2 = nova_router:insert('_', "/users/:id", <<"GET">>, Value2, Tree1),

    %% Store dispatch
    persistent_term:put(nova_dispatch, Tree2),

    %% Set up handlers table
    catch ets:new(nova_handlers_table, [named_table, public, set]),

    %% Register handlers
    OkHandler = fun({ok, Data}, _Callback, Req) ->
                        {ok, Req#{response_data => Data}}
                end,
    ets:insert(nova_handlers_table, {ok, OkHandler}),

    %% Mock cowboy_req:reply
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, reply, fun(StatusCode, Req) ->
                                           Req#{replied => StatusCode}
                                   end),

    ok.

cleanup(_) ->
    catch persistent_term:erase(nova_dispatch),
    catch persistent_term:erase(nova_apps),
    catch ets:delete(nova_handlers_table),
    catch meck:unload(cowboy_req),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_simple_request_lifecycle() ->
    %% Create a request
    Req = #{method => <<"GET">>,
            path => <<"/hello">>,
            host => <<"localhost">>,
            headers => #{}},

    Env = #{},

    %% Step 1: Router executes
    {ok, Req1, Env1} = nova_router:execute(Req, Env),

    %% Verify routing succeeded
    ?assertMatch(#{callback := _}, Env1),

    %% Step 2: Handler executes
    {ok, Req2, _Env2} = nova_handler:execute(Req1, Env1),

    %% Verify response was prepared
    ?assertMatch(#{response_data := #{message := <<"Hello, World!">>}}, Req2).

test_request_with_bindings() ->
    %% Create a request with path parameter
    Req = #{method => <<"GET">>,
            path => <<"/users/123">>,
            host => <<"localhost">>,
            headers => #{}},

    Env = #{},

    %% Route the request
    {ok, Req1, Env1} = nova_router:execute(Req, Env),

    %% Verify bindings were extracted
    ?assertMatch(#{bindings := #{<<"id">> := <<"123">>}}, Req1),

    %% Execute handler
    {ok, Req2, _Env2} = nova_handler:execute(Req1, Env1),

    %% Verify the binding was used in the response
    ?assertMatch(#{response_data := #{user_id := <<"123">>}}, Req2).

test_request_with_plugin() ->
    %% Create a simple plugin
    TestPlugin = fun(Req, _Env, _Options, State) ->
                         %% Add a header to the request
                         {ok, Req#{plugin_executed => true}, State}
                 end,

    %% Create route with plugin
    Tree = persistent_term:get(nova_dispatch),

    Callback = fun(Req) ->
                       {ok, #{plugin_check => maps:get(plugin_executed, Req, false)}}
               end,

    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [{pre_request, [{TestPlugin, #{}}]}],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/with-plugin", <<"GET">>, Value, Tree),
    persistent_term:put(nova_dispatch, Tree1),

    %% Make request
    Req = #{method => <<"GET">>,
            path => <<"/with-plugin">>,
            host => <<"localhost">>,
            headers => #{}},

    Env = #{},

    %% Route
    {ok, Req1, Env1} = nova_router:execute(Req, Env),

    %% Execute plugins
    {ok, Req2, Env2} = nova_plugin_handler:execute(Req1, Env1),

    %% Verify plugin executed
    ?assertEqual(true, maps:get(plugin_executed, Req2)),

    %% Execute handler
    {ok, Req3, _Env3} = nova_handler:execute(Req2, Env2),

    %% Verify plugin effect was visible to handler
    ?assertMatch(#{response_data := #{plugin_check := true}}, Req3).

test_plugin_breaks_chain() ->
    %% Create a plugin that breaks the chain
    BreakPlugin = fun(Req, _Env, _Options, State) ->
                          {break, Req#{break_executed => true}, State}
                  end,

    %% Create route with breaking plugin
    Tree = persistent_term:get(nova_dispatch),

    Callback = fun(_Req) ->
                       %% This should not be called
                       {ok, #{should_not_see => true}}
               end,

    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [{pre_request, [{BreakPlugin, #{}}]}],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/break-chain", <<"GET">>, Value, Tree),
    persistent_term:put(nova_dispatch, Tree1),

    %% Make request
    Req = #{method => <<"GET">>,
            path => <<"/break-chain">>,
            host => <<"localhost">>,
            headers => #{}},

    Env = #{},

    %% Route
    {ok, Req1, Env1} = nova_router:execute(Req, Env),

    %% Execute plugins
    {ok, Req2} = nova_plugin_handler:execute(Req1, Env1),

    %% Verify plugin broke the chain
    ?assertEqual(true, maps:get(break_executed, Req2)),

    %% Handler should not execute when plugin breaks
    %% (In real system, middleware chain would stop here)
    ok.

test_error_propagation() ->
    %% Create a callback that throws an error
    ErrorCallback = fun(_Req) ->
                            error(intentional_error)
                    end,

    Tree = persistent_term:get(nova_dispatch),

    Value = #nova_handler_value{
               app = test_app,
               callback = ErrorCallback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/error-route", <<"GET">>, Value, Tree),
    persistent_term:put(nova_dispatch, Tree1),

    %% Make request
    Req = #{method => <<"GET">>,
            path => <<"/error-route">>,
            host => <<"localhost">>,
            headers => #{}},

    Env = #{},

    %% Route
    {ok, Req1, Env1} = nova_router:execute(Req, Env),

    %% Execute handler - should catch the error
    {ok, Req2, _Env2} = nova_handler:execute(Req1, Env1),

    %% Verify error was caught and crash_info was set
    ?assertMatch(#{crash_info := #{class := error,
                                   reason := intentional_error}}, Req2).

test_not_found_handling() ->
    %% Make request to non-existent route
    Req = #{method => <<"GET">>,
            path => <<"/does-not-exist">>,
            host => <<"localhost">>,
            headers => #{}},

    Env = #{},

    %% Router should return error page rendering
    {ok, Req1, Env1} = nova_router:execute(Req, Env),

    %% Verify 404 status code was set
    ?assertEqual(404, maps:get(resp_status_code, Req1)),

    %% Verify error controller callback was set
    ?assertMatch(#{callback := _}, Env1).
