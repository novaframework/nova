-module(nova_plugin_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),
    persistent_term:put(nova_plugins, []),
    persistent_term:put(nova_use_stacktrace, false),
    {ok, Pid} = nova_plugin_manager:start_link(),
    meck:new(cowboy_req, [passthrough, no_link]),
    meck:expect(cowboy_req, reply, fun(Status, Req) ->
        Req#{resp_status_code => Status}
    end),
    meck:expect(cowboy_req, set_resp_headers, fun(Headers, Req) ->
        Existing = maps:get(resp_headers, Req, #{}),
        Req#{resp_headers => maps:merge(Existing, Headers)}
    end),
    meck:expect(cowboy_req, set_resp_body, fun(Body, Req) ->
        Req#{resp_body => Body}
    end),
    {Prev, Pid}.

cleanup({Prev, Pid}) ->
    gen_server:stop(Pid),
    meck:unload(cowboy_req),
    persistent_term:erase(nova_dispatch),
    persistent_term:erase(nova_plugins),
    persistent_term:erase(nova_use_stacktrace),
    nova_test_helper:cleanup_nova_env(Prev).

req() ->
    nova_test_helper:mock_req(<<"GET">>, <<"/">>).

req(Plugins) ->
    R = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    R#{plugins => Plugins}.

env() ->
    #{app => test_app, controller_data => #{}}.

env(Extra) ->
    maps:merge(env(), Extra).

plugin(Fun) ->
    {Fun, #{}}.

%%====================================================================
%% No plugins key in Req -> passthrough
%%====================================================================

no_plugins_key_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req(),
        Env = env(),
        ?assertEqual({ok, Req, Env}, nova_plugin_handler:execute(Req, Env))
    end}.

%%====================================================================
%% Empty pre_request plugins -> sets plugin_state
%%====================================================================

empty_pre_plugins_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req([{pre_request, []}]),
        Env = env(),
        {ok, Req1, Env1} = nova_plugin_handler:execute(Req, Env),
        ?assertEqual(Req, Req1),
        ?assertEqual(pre_request, maps:get(plugin_state, Env1))
    end}.

%%====================================================================
%% Pre plugin returns {ok, Req, State} -> continues chain
%%====================================================================

pre_plugin_ok_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, R#{plugin_touched => true}, #{ran => true}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, Env1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(true, maps:get(plugin_touched, Req1)),
        ?assertEqual(pre_request, maps:get(plugin_state, Env1))
    end}.

%%====================================================================
%% Pre plugin returns {ok, Reply, Req, State} -> handles reply, continues
%%====================================================================

pre_plugin_ok_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, {reply, <<"hello">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, Env1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(200, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"hello">>, maps:get(resp_body, Req1)),
        ?assertEqual(pre_request, maps:get(plugin_state, Env1))
    end}.

%%====================================================================
%% Pre plugin returns {break, Req, State} -> stops chain
%%====================================================================

pre_plugin_break_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {break, R#{broke => true}, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(true, maps:get(broke, Req1))
    end}.

%%====================================================================
%% Pre plugin returns {break, Reply, Req, State} -> handles reply, stops
%%====================================================================

pre_plugin_break_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {break, {reply, 403, <<"forbidden">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"forbidden">>, maps:get(resp_body, Req1))
    end}.

%%====================================================================
%% Pre plugin returns {stop, Req, State} -> {stop, Req}
%%====================================================================

pre_plugin_stop_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {stop, R#{stopped => true}, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(true, maps:get(stopped, Req1))
    end}.

%%====================================================================
%% Pre plugin returns {stop, Reply, Req, State} -> replies, {stop, Req}
%%====================================================================

pre_plugin_stop_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {stop, {reply, 503, <<"unavailable">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(503, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"unavailable">>, maps:get(resp_body, Req1)),
        ?assert(meck:called(cowboy_req, reply, [503, '_']))
    end}.

%%====================================================================
%% Post request path (plugin_state already pre_request in Env)
%%====================================================================

post_plugin_path_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, R#{post_ran => true}, #{}}
        end,
        Req = req([{post_request, [plugin(P)]}]),
        Env = env(#{plugin_state => pre_request}),
        {ok, Req1, Env1} = nova_plugin_handler:execute(Req, Env),
        ?assertEqual(true, maps:get(post_ran, Req1)),
        ?assertEqual(post_request, maps:get(plugin_state, Env1))
    end}.

%%====================================================================
%% Plugin crash -> renders 500
%%====================================================================

plugin_crash_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(_R, _Env, _Opts, _State) ->
            error(boom)
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assert(is_map(maps:get(crash_info, Req1)))
    end}.

%%====================================================================
%% Multiple plugins chain in order
%%====================================================================

multiple_plugins_chain_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P1 = fun(R, _Env, _Opts, _State) ->
            Order = maps:get(order, R, []),
            {ok, R#{order => Order ++ [first]}, #{}}
        end,
        P2 = fun(R, _Env, _Opts, _State) ->
            Order = maps:get(order, R, []),
            {ok, R#{order => Order ++ [second]}, #{}}
        end,
        P3 = fun(R, _Env, _Opts, _State) ->
            Order = maps:get(order, R, []),
            {ok, R#{order => Order ++ [third]}, #{}}
        end,
        Req = req([{pre_request, [plugin(P1), plugin(P2), plugin(P3)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual([first, second, third], maps:get(order, Req1))
    end}.

%%====================================================================
%% Break in middle of chain stops remaining plugins
%%====================================================================

break_stops_chain_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P1 = fun(R, _Env, _Opts, _State) ->
            {ok, R#{p1 => true}, #{}}
        end,
        P2 = fun(R, _Env, _Opts, _State) ->
            {break, R#{p2 => true}, #{}}
        end,
        P3 = fun(R, _Env, _Opts, _State) ->
            {ok, R#{p3 => true}, #{}}
        end,
        Req = req([{pre_request, [plugin(P1), plugin(P2), plugin(P3)]}]),
        {ok, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(true, maps:get(p1, Req1)),
        ?assertEqual(true, maps:get(p2, Req1)),
        ?assertNot(maps:is_key(p3, Req1))
    end}.

%%====================================================================
%% handle_reply with {reply, Body} -> 200 default
%%====================================================================

handle_reply_body_only_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, {reply, <<"body">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(200, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"body">>, maps:get(resp_body, Req1))
    end}.

%%====================================================================
%% handle_reply with {reply, Status, Body}
%%====================================================================

handle_reply_status_body_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, {reply, 201, <<"created">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(201, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"created">>, maps:get(resp_body, Req1))
    end}.

%%====================================================================
%% handle_reply with {reply, Status, Headers, Body}
%%====================================================================

handle_reply_full_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, {reply, 202, #{<<"x-custom">> => <<"val">>}, <<"accepted">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(202, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"accepted">>, maps:get(resp_body, Req1)),
        Headers = maps:get(resp_headers, Req1),
        ?assertEqual(<<"val">>, maps:get(<<"x-custom">>, Headers))
    end}.

%%====================================================================
%% handle_reply passthrough with unknown format
%%====================================================================

handle_reply_passthrough_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, something_unknown, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertNot(maps:is_key(resp_status_code, Req1))
    end}.

%%====================================================================
%% Empty post_request plugins -> sets plugin_state to post_request
%%====================================================================

empty_post_plugins_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = req([{post_request, []}]),
        Env = env(#{plugin_state => pre_request}),
        {ok, _Req1, Env1} = nova_plugin_handler:execute(Req, Env),
        ?assertEqual(post_request, maps:get(plugin_state, Env1))
    end}.

%%====================================================================
%% Stop with reply uses default 200 when no resp_status_code set
%%====================================================================

stop_reply_default_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {stop, {reply, <<"bye">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(200, maps:get(resp_status_code, Req1)),
        ?assert(meck:called(cowboy_req, reply, [200, '_']))
    end}.
