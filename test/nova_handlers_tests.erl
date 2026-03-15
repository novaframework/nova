-module(nova_handlers_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    {ok, Pid} = nova_handlers:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% Default handlers registered on init
%%====================================================================

default_json_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(json),
        ?assert(is_function(Handler))
    end}.

default_ok_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(ok),
        ?assert(is_function(Handler))
    end}.

default_status_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(status),
        ?assert(is_function(Handler))
    end}.

default_redirect_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(redirect),
        ?assert(is_function(Handler))
    end}.

default_sendfile_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(sendfile),
        ?assert(is_function(Handler))
    end}.

default_ws_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(ws),
        ?assert(is_function(Handler))
    end}.

default_view_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        {ok, Handler} = nova_handlers:get_handler(view),
        ?assert(is_function(Handler))
    end}.

%%====================================================================
%% get_handler/1
%%====================================================================

get_handler_not_found_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual({error, not_found}, nova_handlers:get_handler(nonexistent))
    end}.

%%====================================================================
%% register_handler/2
%%====================================================================

register_custom_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        CustomFun = fun(_RetObj, _Callback, _Req) -> {ok, #{}} end,
        nova_handlers:register_handler(custom, CustomFun),
        timer:sleep(50),
        {ok, Handler} = nova_handlers:get_handler(custom),
        ?assertEqual(CustomFun, Handler)
    end}.

register_duplicate_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Fun1 = fun(_RetObj, _Callback, _Req) -> {ok, #{a => 1}} end,
        Fun2 = fun(_RetObj, _Callback, _Req) -> {ok, #{b => 2}} end,
        nova_handlers:register_handler(dup_test, Fun1),
        timer:sleep(50),
        %% Second registration should be rejected (already exists)
        nova_handlers:register_handler(dup_test, Fun2),
        timer:sleep(50),
        {ok, Handler} = nova_handlers:get_handler(dup_test),
        ?assertEqual(Fun1, Handler)
    end}.

register_mfa_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        nova_handlers:register_handler(mfa_test, {lists, reverse}),
        timer:sleep(50),
        {ok, Handler} = nova_handlers:get_handler(mfa_test),
        ?assert(is_function(Handler))
    end}.

%%====================================================================
%% unregister_handler/1
%%====================================================================

unregister_handler_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        CustomFun = fun(_RetObj, _Callback, _Req) -> ok end,
        nova_handlers:register_handler(to_remove, CustomFun),
        timer:sleep(50),
        ?assertMatch({ok, _}, nova_handlers:get_handler(to_remove)),
        ok = nova_handlers:unregister_handler(to_remove),
        ?assertEqual({error, not_found}, nova_handlers:get_handler(to_remove))
    end}.

%%====================================================================
%% format_status/1
%%====================================================================

format_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Status = #{state => test, log => [], message => none, reason => none},
        ?assertEqual(Status, nova_handlers:format_status(Status))
    end}.

%%====================================================================
%% code_change/3
%%====================================================================

code_change_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ?assertEqual({ok, my_state}, nova_handlers:code_change("1.0", my_state, []))
    end}.
