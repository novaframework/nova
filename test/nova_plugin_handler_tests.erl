-module(nova_plugin_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

-define(SETUP, fun() -> setup() end).
-define(CLEANUP, fun(S) -> cleanup(S) end).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    Tree = nova_routing_trie:new(#{use_strict => false, convert_to_binary => true}),
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
        New = case is_list(Headers) of
                  true -> maps:from_list(Headers);
                  false -> Headers
              end,
        Req#{resp_headers => maps:merge(Existing, New)}
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
%% {break, ...} is a deprecated alias of stop. It must never let the request
%% on to security and the controller.
%%====================================================================

pre_plugin_break_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {break, R#{broke => true}, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(true, maps:get(broke, Req1))
    end}.

pre_plugin_break_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {break, {reply, 403, <<"forbidden">>}, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"forbidden">>, maps:get(resp_body, Req1)),
        ?assert(meck:called(cowboy_req, reply, [403, '_']))
    end}.

%% nova_oidcc answers anonymous users with these two.
break_redirect_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {break, {redirect, ~"/login"}, R, #{}} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(302, maps:get(resp_status_code, Req1)),
        ?assertEqual(~"/login", maps:get(~"location", maps:get(resp_headers, Req1))),
        ?assert(meck:called(cowboy_req, reply, [302, '_']))
    end}.

break_status_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {break, {status, 401}, R, #{}} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(401, maps:get(resp_status_code, Req1)),
        ?assert(meck:called(cowboy_req, reply, [401, '_']))
    end}.

break_unknown_reply_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {break, {not_a_reply, 1}, R, #{}} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, Env1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertEqual(pre_request, maps:get(plugin_state, Env1)),
        ?assertEqual(fun nova_error_controller:status_code/1, maps:get(callback, Env1))
    end}.

stop_redirect_string_reply_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {stop, {redirect, "/login"}, R, #{}} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(~"/login", maps:get(~"location", maps:get(resp_headers, Req1)))
    end}.

%% A redirect target that is not valid chardata gets Nova's 500 page.
stop_redirect_invalid_location_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {stop, {redirect, [not_a_char]}, R, #{}} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

%% Control bytes and invalid UTF-8 would split the response or make Cowboy
%% crash it; they get the 500 page and a log line without the query string.
stop_redirect_with_control_bytes_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
        try
            [begin
                 P = fun(R, _Env, _Opts, _State) -> {stop, {redirect, Url}, R, #{}} end,
                 {ok, Req1, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
                 ?assertEqual(500, maps:get(resp_status_code, Req1)),
                 Logs = logged(),
                 ?assertMatch([{report, #{msg := ~"Plugin returned an unsupported reply"}} | _], Logs),
                 ?assertNot(has_text(Logs, ~"TOKEN"))
             end || Url <- [<<"/a", 0, "b?t=TOKEN">>, ~"/a\r\nset-cookie: x=1?t=TOKEN", <<"/x", 255, "?t=TOKEN">>,
                            "/a\r\nb?t=TOKEN", <<"https://user:TOKEN@host/a", 0>>]]
        after
            logger:remove_handler(?MODULE)
        end
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
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
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
%% An unknown reply is a 500, not silently dropped
%%====================================================================

handle_reply_unknown_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) ->
            {ok, something_unknown, R, #{}}
        end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
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

%%====================================================================
%% Returns that used to crash the request
%%====================================================================

%% nova_auth_rate_limit and others answer and return {stop, Req}.
stop_two_tuple_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {stop, R#{stopped => true}} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(true, maps:get(stopped, Req1))
    end}.

%% Documented in nova_plugin: {error, Reason} gives a 500 page.
error_return_renders_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(_R, _Env, _Opts, _State) -> {error, body_not_parsed} end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, Env1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertMatch(#{reason := body_not_parsed}, maps:get(crash_info, Req1)),
        ?assert(is_function(maps:get(callback, Env1)))
    end}.

unsupported_return_renders_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(_R, _Env, _Opts, _State) -> what_is_this end,
        Req = req([{pre_request, [plugin(P)]}]),
        {ok, Req1, _Env1} = nova_plugin_handler:execute(Req, env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

post_plugin_break_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P1 = fun(R, _Env, _Opts, _State) -> {break, R#{p1 => true}, #{}} end,
        P2 = fun(R, _Env, _Opts, _State) -> {ok, R#{p2 => true}, #{}} end,
        Req = req([{post_request, [plugin(P1), plugin(P2)]}]),
        {stop, Req1} = nova_plugin_handler:execute(Req, env(#{plugin_state => pre_request})),
        ?assert(maps:get(p1, Req1)),
        ?assertNot(maps:is_key(p2, Req1))
    end}.

%% The 500 page goes through the post_request middleware. The pre_request
%% plugins must not run again there, whichever way the phase failed.
pre_plugins_run_once_on_failure_test_() ->
    Failures = [fun(_R) -> {error, nope} end,
                fun(_R) -> what_is_this end,
                fun(_R) -> error(boom) end,
                fun(R) -> {break, {not_a_reply, 1}, R, #{}} end],
    {setup, ?SETUP, ?CLEANUP,
     [fun() ->
              Runs = counters:new(1, []),
              Counting = fun(R, _Env, _Opts, State) -> counters:add(Runs, 1, 1), {ok, R, State} end,
              Failing = fun(R, _Env, _Opts, _State) -> Fail(R) end,
              Req = req([{pre_request, [plugin(Counting), plugin(Failing)]}]),
              {ok, Req1, Env1} = nova_plugin_handler:execute(Req, env()),
              {ok, _Req2, _Env2} = nova_plugin_handler:execute(Req1, Env1),
              ?assertEqual(1, counters:get(Runs, 1))
      end || Fail <- Failures]}.

%% A plugin that hands its state back unchanged does not go through the
%% plugin manager's gen_server.
unchanged_state_is_not_written_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        meck:new(nova_plugin_manager, [passthrough]),
        try
            meck:expect(nova_plugin_manager, get_state, fun(_) -> {ok, #{n => 1}} end),
            Same = fun(R, _Env, _Opts, State) -> {ok, R, State} end,
            Changed = fun(R, _Env, _Opts, State) -> {ok, R, State#{n => 2}} end,
            {ok, _, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(Same)]}]), env()),
            ?assertEqual(0, meck:num_calls(nova_plugin_manager, set_state, '_')),
            {ok, _, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(Changed)]}]), env()),
            ?assertEqual(1, meck:num_calls(nova_plugin_manager, set_state, ['_', #{n => 2}]))
        after
            meck:unload(nova_plugin_manager)
        end
    end}.

%%====================================================================
%% {ok, Reply, Req, State} only hands on a reply
%%====================================================================

ok_redirect_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {ok, {redirect, ~"/login"}, R, #{}} end,
        {ok, Req1, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1)),
        ?assertNot(is_map_key(~"location", maps:get(resp_headers, Req1))),
        ?assertMatch(#{class := error, reason := unsupported_plugin_reply}, maps:get(crash_info, Req1))
    end}.

ok_status_is_a_500_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {ok, {status, 401}, R, #{}} end,
        {ok, Req1, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
        ?assertEqual(500, maps:get(resp_status_code, Req1))
    end}.

%%====================================================================
%% Reply headers may be a map or a list
%%====================================================================

reply_with_list_headers_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {stop, {reply, 403, [{~"x-reason", ~"denied"}], ~"no"}, R, #{}} end,
        {stop, Req1} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
        ?assertEqual(403, maps:get(resp_status_code, Req1)),
        ?assertEqual(~"denied", maps:get(~"x-reason", maps:get(resp_headers, Req1)))
    end}.

ok_reply_with_list_headers_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        P = fun(R, _Env, _Opts, _State) -> {ok, {reply, 200, [{~"x-a", ~"1"}], ~"b"}, R, #{}} end,
        {ok, Req1, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
        ?assertEqual(~"1", maps:get(~"x-a", maps:get(resp_headers, Req1)))
    end}.

status_with_map_and_list_headers_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Map = fun(R, _Env, _Opts, _State) -> {stop, {status, 401, #{~"www-authenticate" => ~"Bearer"}}, R, #{}} end,
        List = fun(R, _Env, _Opts, _State) -> {stop, {status, 401, [{~"www-authenticate", ~"Bearer"}]}, R, #{}} end,
        [begin
             {stop, Req1} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
             ?assertEqual(401, maps:get(resp_status_code, Req1)),
             ?assertEqual(~"Bearer", maps:get(~"www-authenticate", maps:get(resp_headers, Req1)))
         end || P <- [Map, List]]
    end}.

%%====================================================================
%% Every 500 carries crash_info the error page can render
%%====================================================================

failures_carry_crash_info_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Plugins = [fun(_R, _Env, _Opts, _State) -> what_is_this end,
                   fun(R, _Env, _Opts, _State) -> {stop, {not_a_reply, 1}, R, #{}} end,
                   fun(R, _Env, _Opts, _State) -> {ok, {redirect, ~"/"}, R, #{}} end,
                   fun(_R, _Env, _Opts, _State) -> {error, boom} end,
                   fun(_R, _Env, _Opts, _State) -> error(boom) end],
        [begin
             {ok, Req1, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
             ?assertMatch(#{class := _, reason := _}, maps:get(crash_info, Req1)),
             ?assertMatch({status, 500, _, _},
                          nova_error_controller:server_error(Req1#{headers => #{~"accept" => ~"application/json"}}))
         end || P <- Plugins]
    end}.

%%====================================================================
%% Logs and crash_info leave out request data and secrets
%%====================================================================

plugin_failures_are_redacted_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Secret = (req())#{headers => #{~"cookie" => ~"session_id=VICTIMCOOKIE"}},
        Plugins = [fun(_R, _Env, _Opts, _State) -> {error, #{password => ~"VICTIMCOOKIE"}} end,
                   fun(_R, _Env, _Opts, _State) -> {unexpected, Secret} end,
                   fun(R, _Env, _Opts, _State) -> {stop, {not_a_reply, Secret}, R, #{}} end,
                   fun(R, _Env, _Opts, _State) -> {ok, {redirect, Secret}, R, #{}} end,
                   fun(#{this_key_is_missing := _}, _Env, _Opts, _State) -> ok end],
        ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
        try
            [begin
                 Req = (req([{pre_request, [plugin(P)]}]))#{headers => #{~"cookie" => ~"session_id=VICTIMCOOKIE"}},
                 {ok, Req1, _} = nova_plugin_handler:execute(Req, env()),
                 Logs = logged(),
                 ?assertNotEqual([], Logs),
                 assert_not_leaked({Logs, maps:get(crash_info, Req1)})
             end || P <- Plugins]
        after
            logger:remove_handler(?MODULE)
        end
    end}.

%%====================================================================
%% break is warned about once per plugin module
%%====================================================================

break_is_warned_about_once_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        persistent_term:erase({nova_plugin_handler, break_warned, ?MODULE}),
        P = fun(R, _Env, _Opts, _State) -> {break, R, #{}} end,
        ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
        try
            {stop, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
            ?assertMatch([_], logged()),
            {stop, _} = nova_plugin_handler:execute(req([{pre_request, [plugin(P)]}]), env()),
            ?assertEqual([], logged())
        after
            logger:remove_handler(?MODULE)
        end
    end}.

log(#{msg := Msg}, #{config := #{test_pid := Pid}}) ->
    Pid ! {logged, Msg},
    ok.

logged() ->
    receive
        {logged, Msg} -> [Msg | logged()]
    after 50 ->
            []
    end.

assert_not_leaked(Term) ->
    Text = iolist_to_binary(io_lib:format("~p", [Term])),
    ?assertEqual(nomatch, binary:match(Text, ~"VICTIMCOOKIE")).

%% io_lib:format/2 prints a binary with a control byte as a list of integers,
%% so a text search of its output would miss a leak.
has_text(Bin, Needle) when is_binary(Bin) ->
    binary:match(Bin, Needle) =/= nomatch;
has_text(Tuple, Needle) when is_tuple(Tuple) ->
    has_text(tuple_to_list(Tuple), Needle);
has_text(Map, Needle) when is_map(Map) ->
    has_text(maps:to_list(Map), Needle);
has_text([Head | Tail], Needle) ->
    has_text(Head, Needle) orelse has_text(Tail, Needle);
has_text(_Term, _Needle) ->
    false.
