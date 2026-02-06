-module(nova_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/nova_router.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Successful controller execution", fun test_successful_execution/0},
      {"Controller execution with callback", fun test_callback_execution/0},
      {"Controller crash without stacktrace", fun test_crash_no_stacktrace/0},
      {"Controller crash with stacktrace", fun test_crash_with_stacktrace/0},
      {"Status code exception", fun test_status_exception/0},
      {"Terminate callback invocation", fun test_terminate_callback/0},
      {"Unsupported return type", fun test_unsupported_return/0}
     ]}.

setup() ->
    %% Set up required environment
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, render_error_pages, false),
    persistent_term:put(nova_use_stacktrace, false),

    %% Initialize routing dispatch (needed for error page rendering)
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),

    %% Initialize handlers table (same name as in nova_handlers module)
    catch ets:new(nova_handlers_table, [named_table, public, set]),

    %% Register basic handlers for testing
    register_test_handlers(),

    %% Mock cowboy_req:reply to avoid actual HTTP operations
    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, reply, fun(StatusCode, Req) ->
                                           Req#{replied => StatusCode}
                                   end),

    ok.

cleanup(_) ->
    %% Clean up persistent_term
    catch persistent_term:erase(nova_use_stacktrace),
    catch persistent_term:erase(nova_dispatch),

    %% Clean up ETS
    catch ets:delete(nova_handlers_table),

    %% Unload meck
    catch meck:unload(cowboy_req),

    ok.

register_test_handlers() ->
    %% Register a simple 'ok' handler for testing
    OkHandler = fun({ok, Data}, _Callback, Req) ->
                        {ok, Req#{handler_data => Data}}
                end,
    ets:insert(nova_handlers_table, {ok, OkHandler}),

    %% Register a status handler
    StatusHandler = fun({status, StatusCode}, _Callback, Req) ->
                            {ok, Req#{resp_status_code => StatusCode}}
                    end,
    ets:insert(nova_handlers_table, {status, StatusHandler}).

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_successful_execution() ->
    %% Create a simple successful callback
    Callback = fun(Req) ->
                       {ok, #{result => success}}
               end,

    Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}},
    Env = #{callback => Callback},

    %% Execute the handler
    {ok, Req1, Env1} = nova_handler:execute(Req, Env),

    %% Verify the result
    ?assertMatch(#{handler_data := #{result := success}}, Req1),
    ?assertEqual(Env, Env1).

test_callback_execution() ->
    %% Test that callback is properly executed
    Callback = fun(Req) ->
                       {ok, #{data => <<"test">>}}
               end,

    Req = #{method => <<"POST">>, path => <<"/test">>, headers => #{}},
    Env = #{callback => Callback},

    {ok, Req1, _Env1} = nova_handler:execute(Req, Env),

    ?assertMatch(#{handler_data := #{data := <<"test">>}}, Req1).

test_crash_no_stacktrace() ->
    %% Test controller crash without stacktrace
    persistent_term:put(nova_use_stacktrace, false),

    Callback = fun(_Req) ->
                       error(simulated_crash)
               end,

    Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}},
    Env = #{callback => Callback},

    %% Should catch the error and return error response
    {ok, Req1, _Env1} = nova_handler:execute(Req, Env),

    %% Verify crash_info is set
    ?assertMatch(#{crash_info := #{status_code := 500,
                                   class := error,
                                   reason := simulated_crash}}, Req1).

test_crash_with_stacktrace() ->
    %% Test controller crash with stacktrace enabled
    persistent_term:put(nova_use_stacktrace, true),

    Callback = fun(_Req) ->
                       error(crash_with_trace)
               end,

    Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}},
    Env = #{callback => Callback},

    {ok, Req1, _Env1} = nova_handler:execute(Req, Env),

    %% Verify crash_info includes stacktrace
    ?assertMatch(#{crash_info := #{status_code := 500,
                                   class := error,
                                   reason := crash_with_trace,
                                   stacktrace := _}}, Req1),

    %% Reset for other tests
    persistent_term:put(nova_use_stacktrace, false).

test_status_exception() ->
    %% Test throwing a status code exception
    Callback = fun(_Req) ->
                       throw({500, <<"Internal error">>})
               end,

    Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}},
    Env = #{callback => Callback},

    {ok, Req1, _Env1} = nova_handler:execute(Req, Env),

    %% Verify the crash_info contains the thrown reason
    ?assertMatch(#{crash_info := <<"Internal error">>}, Req1),
    ?assertMatch(#{resp_status_code := 500}, Req1).

test_terminate_callback() ->
    %% Create a simple callback function
    Callback = fun(Req) -> {ok, Req} end,

    %% Test that terminate returns ok (even if module doesn't have terminate/3)
    Result = nova_handler:terminate(normal, #{}, Callback),

    %% Should return ok
    ?assertEqual(ok, Result).

test_unsupported_return() ->
    %% Test controller returning unsupported type
    Callback = fun(_Req) ->
                       {unsupported_type, data}
               end,

    Req = #{method => <<"GET">>, path => <<"/">>, headers => #{}},
    Env = #{callback => Callback},

    %% Should handle unsupported return type
    {ok, Req1, _Env1} = nova_handler:execute(Req, Env),

    %% Should have crash_info indicating unsupported return
    ?assertMatch(#{crash_info := #{status_code := 500}}, Req1).
