-module(nova_handlers_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

handlers_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Register new handler", fun test_register_handler/0},
      {"Register handler with MFA tuple", fun test_register_mfa_handler/0},
      {"Get existing handler", fun test_get_handler/0},
      {"Get non-existent handler", fun test_get_nonexistent_handler/0},
      {"Unregister handler", fun test_unregister_handler/0},
      {"Duplicate registration handling", fun test_duplicate_registration/0},
      {"Default handlers registered on init", fun test_default_handlers/0}
     ]}.

setup() ->
    %% Start the handlers gen_server
    {ok, Pid} = nova_handlers:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the gen_server
    exit(Pid, normal),
    %% Wait for it to terminate
    timer:sleep(50),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_register_handler() ->
    %% Register a custom handler
    HandlerFun = fun({custom, Data}, _Callback, Req) ->
                         {ok, Req#{custom_data => Data}}
                 end,

    ok = nova_handlers:register_handler(custom, HandlerFun),

    %% Wait for the cast to be processed
    timer:sleep(10),

    %% Verify the handler was registered
    {ok, RegisteredHandler} = nova_handlers:get_handler(custom),

    ?assert(is_function(RegisteredHandler)).

test_register_mfa_handler() ->
    %% Create a mock module with a handler function
    meck:new(test_handler_module, [non_strict]),
    meck:expect(test_handler_module, handle_test,
                fun({test, _Data}, _Callback, _Req, _State) ->
                        {ok, #{result => success}}
                end),

    %% Register a handler using MFA tuple (arity 4 for nova handlers)
    ok = nova_handlers:register_handler(test_handler, {test_handler_module, handle_test}),

    %% Wait for the cast to be processed
    timer:sleep(10),

    %% Verify the handler was registered and converted to function
    {ok, Handler} = nova_handlers:get_handler(test_handler),

    ?assert(is_function(Handler)),

    meck:unload(test_handler_module).

test_get_handler() ->
    %% Register a handler
    HandlerFun = fun({test, _}, _Callback, Req) -> {ok, Req} end,
    ok = nova_handlers:register_handler(test, HandlerFun),

    timer:sleep(10),

    %% Get the handler
    Result = nova_handlers:get_handler(test),

    ?assertMatch({ok, _}, Result).

test_get_nonexistent_handler() ->
    %% Try to get a handler that doesn't exist
    Result = nova_handlers:get_handler(nonexistent_handler),

    ?assertEqual({error, not_found}, Result).

test_unregister_handler() ->
    %% Register a handler
    HandlerFun = fun({temp, _}, _Callback, Req) -> {ok, Req} end,
    ok = nova_handlers:register_handler(temp, HandlerFun),

    timer:sleep(10),

    %% Verify it exists
    ?assertMatch({ok, _}, nova_handlers:get_handler(temp)),

    %% Unregister it
    ok = nova_handlers:unregister_handler(temp),

    %% Verify it's gone
    ?assertEqual({error, not_found}, nova_handlers:get_handler(temp)).

test_duplicate_registration() ->
    %% Register a handler
    Handler1 = fun({dup, _}, _Callback, Req) -> {ok, Req#{version => 1}} end,
    ok = nova_handlers:register_handler(dup, Handler1),

    timer:sleep(10),

    %% Try to register another handler with the same name
    Handler2 = fun({dup, _}, _Callback, Req) -> {ok, Req#{version => 2}} end,
    ok = nova_handlers:register_handler(dup, Handler2),

    timer:sleep(10),

    %% The first handler should still be registered (duplicate ignored)
    {ok, RegisteredHandler} = nova_handlers:get_handler(dup),

    %% Verify it's still the first handler (can't easily test function equality,
    %% but we can verify it exists)
    ?assert(is_function(RegisteredHandler)).

test_default_handlers() ->
    %% Default handlers should be registered on init
    %% These are: json, ok, status, redirect, sendfile, ws, view

    ?assertMatch({ok, _}, nova_handlers:get_handler(json)),
    ?assertMatch({ok, _}, nova_handlers:get_handler(ok)),
    ?assertMatch({ok, _}, nova_handlers:get_handler(status)),
    ?assertMatch({ok, _}, nova_handlers:get_handler(redirect)),
    ?assertMatch({ok, _}, nova_handlers:get_handler(sendfile)),
    ?assertMatch({ok, _}, nova_handlers:get_handler(ws)),
    ?assertMatch({ok, _}, nova_handlers:get_handler(view)).
