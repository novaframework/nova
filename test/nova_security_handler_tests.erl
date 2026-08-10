-module(nova_security_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_use_stacktrace, false),
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, reply, fun(Status, Req) ->
        Req#{resp_status_code => Status}
    end),
    meck:expect(cowboy_req, set_resp_headers, fun(Headers, Req) ->
        ExistingHeaders = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => maps:merge(ExistingHeaders, Headers)}
    end),
    meck:expect(cowboy_req, set_resp_body, fun(Body, Req) ->
        Req#{resp_body => Body}
    end),
    meck:expect(cowboy_req, set_resp_header, fun(Name, Value, Req) ->
        Headers = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => Headers#{Name => Value}}
    end),
    Prev.

cleanup(Prev) ->
    meck:unload(cowboy_req),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_use_stacktrace),
    nova_test_helper:cleanup_nova_env(Prev).

req() ->
    nova_test_helper:mock_req(<<"GET">>, <<"/">>).

env() ->
    #{secure => false, app => test_app, controller_data => #{}}.

%%====================================================================
%% secure=false — passthrough
%%====================================================================

secure_false_passthrough_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Env = env(),
        ?assertEqual({ok, Req, Env}, nova_security_handler:execute(Req, Env))
    end}.

%%====================================================================
%% Callback returns true
%%====================================================================

callback_true_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> true end,
        Env = (env())#{secure => Callback},
        ?assertEqual({ok, Req, Env}, nova_security_handler:execute(Req, Env))
    end}.

%%====================================================================
%% Callback returns {true, AuthData}
%%====================================================================

callback_true_with_auth_data_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        AuthData = #{user_id => 42},
        Callback = fun(_) -> {true, AuthData} end,
        Env = (env())#{secure => Callback},
        {ok, Req1, _Env1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(AuthData, maps:get(auth_data, Req1))
    end}.

%%====================================================================
%% Callback returns {true, AuthData} for websocket handler
%%====================================================================

callback_true_auth_data_websocket_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        AuthData = #{user_id => 99},
        Callback = fun(_) -> {true, AuthData} end,
        Env = (env())#{secure => Callback,
                     cowboy_handler => nova_ws_handler,
                     arguments => #{existing => data}},
        {ok, _Req1, Env1} = nova_security_handler:execute(Req, Env),
        Args = maps:get(arguments, Env1),
        ?assertEqual(#{auth_data => AuthData}, maps:get(controller_data, Args)),
        ?assertEqual(data, maps:get(existing, Args))
    end}.

%%====================================================================
%% Callback returns {false, Headers} -> 401 stop
%%====================================================================

callback_false_headers_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        RespHeaders = #{<<"x-error">> => <<"denied">>},
        Callback = fun(_) -> {false, RespHeaders} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(401, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"denied">>, maps:get(<<"x-error">>, maps:get(resp_headers, Req1)))
    end}.

%%====================================================================
%% Callback returns {false, 403, Headers} -> 403 stop
%%====================================================================

callback_false_custom_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        RespHeaders = #{<<"x-reason">> => <<"forbidden">>},
        Callback = fun(_) -> {false, 403, RespHeaders} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"forbidden">>, maps:get(<<"x-reason">>, maps:get(resp_headers, Req1)))
    end}.

%%====================================================================
%% Callback returns {false, 403, Headers, Body} -> stop with body
%%====================================================================

callback_false_with_body_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        RespHeaders = #{<<"content-type">> => <<"text/plain">>},
        Body = <<"Access denied">>,
        Callback = fun(_) -> {false, 403, RespHeaders, Body} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(Body, maps:get(resp_body, Req1))
    end}.

%%====================================================================
%% Callback returns {redirect, "/login"} -> 302 stop
%%====================================================================

callback_redirect_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> {redirect, "/login"} end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(302, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"/login">>, maps:get(<<"location">>, maps:get(resp_headers, Req1)))
    end}.

%%====================================================================
%% Callback returns unexpected value -> 401 stop
%%====================================================================

callback_unexpected_value_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> something_weird end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(401, maps:get(resp_status_code, Req1))
    end}.

%%====================================================================
%% Callback crashes -> 500 stop (stacktrace disabled)
%%====================================================================

callback_crash_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Callback = fun(_) -> error(boom) end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

%%====================================================================
%% Callback crashes with stacktrace enabled -> 500 stop
%%====================================================================

callback_crash_stacktrace_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:put(nova_use_stacktrace, true),
        Req = req(),
        Callback = fun(_) -> error(boom) end,
        Env = (env())#{secure => Callback},
        {stop, Req1} = nova_security_handler:execute(Req, Env),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assert(is_map(maps:get(crash_info, Req1))),
        CrashInfo = maps:get(crash_info, Req1),
        ?assert(is_list(maps:get(stacktrace, CrashInfo)))
    end}.
