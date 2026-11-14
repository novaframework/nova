%%% The order a request passes the plugins, the security callback and the
%%% controller in, as the plugins guide describes it.
-module(nova_request_pipeline_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([
         served_request_runs_both_plugin_phases/1,
         plugin_answers_before_the_security_callback/1,
         stopped_request_runs_no_post_request_plugin/1,
         denied_request_runs_no_post_request_plugin/1,
         unmatched_path_runs_no_plugin/1,
         unmatched_method_runs_no_plugin/1,
         status_page_is_behind_the_security_of_its_group/1,
         websocket_upgrade_runs_both_plugin_phases/1
        ]).

-export([pre_request/4, post_request/4, plugin_info/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     served_request_runs_both_plugin_phases,
     plugin_answers_before_the_security_callback,
     stopped_request_runs_no_post_request_plugin,
     denied_request_runs_no_post_request_plugin,
     unmatched_path_runs_no_plugin,
     unmatched_method_runs_no_plugin,
     status_page_is_behind_the_security_of_its_group,
     websocket_upgrade_runs_both_plugin_phases
    ].

init_per_suite(Config) ->
    Page = [{"/", fun nova_request_pipeline_pages:page/1, #{methods => [get]}}],
    Host = [
            #{prefix => "/open", routes => [{"/ws", nova_test_app_ws_controller, #{protocol => ws}} | Page]},
            #{prefix => "/denied", security => fun nova_request_pipeline_pages:deny/1, routes => Page},
            #{prefix => "/stopped",
              security => fun nova_request_pipeline_pages:deny/1,
              plugins => [{pre_request, ?MODULE, #{stop => true}}, {post_request, ?MODULE, #{}}],
              routes => Page},
            #{security => fun nova_request_pipeline_pages:marked/1,
              routes => [{405, fun nova_request_pipeline_pages:method_page/1, #{}}]}
           ],
    Port = nova_test_listener:free_port(),
    ok = nova_test_listener:configure(#{port => Port, host => Host}),
    application:set_env(nova, plugins, [{pre_request, ?MODULE, #{}}, {post_request, ?MODULE, #{}}]),
    {ok, _Started} = application:ensure_all_started(nova_test_app),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    ok = nova_test_listener:stop(),
    application:set_env(nova, plugins, []),
    ok.

%% The plugin tells the process of the test case which phase it ran in.
init_per_testcase(_Case, Config) ->
    persistent_term:put(?MODULE, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    persistent_term:erase(?MODULE),
    ok.

pre_request(Req, _Env, #{stop := true}, State) ->
    ran(pre_request),
    {stop, {reply, 403, ~"stopped by a plugin"}, Req, State};
pre_request(Req, _Env, _Options, State) ->
    ran(pre_request),
    {ok, cowboy_req:set_resp_header(~"x-pipeline", ~"pre_request", Req), State}.

post_request(Req, _Env, _Options, State) ->
    ran(post_request),
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"pipeline test plugin",
      version => ~"1.0.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team <info@novaframework.org>"],
      description => ~"Counts the phases it runs in",
      options => []}.

ran(Phase) ->
    case persistent_term:get(?MODULE) of
        TestCase when is_pid(TestCase) -> TestCase ! {?MODULE, Phase}
    end.

%% The phases that ran since the test case started, or since the last call.
%% A response is read until the connection closes, which is after the
%% post_request plugins of that request.
phases() ->
    receive
        {?MODULE, Phase} -> [Phase | phases()]
    after 0 ->
        []
    end.

served_request_runs_both_plugin_phases(Config) ->
    {Status, Headers, Body} = nova_test_listener:response(?config(port, Config), "GET", "/open/", []),
    ?assertEqual({200, ~"page"}, {Status, Body}),
    ?assertEqual(~"pre_request", maps:get(~"x-pipeline", Headers)),
    ?assertEqual([pre_request, post_request], phases()).

plugin_answers_before_the_security_callback(Config) ->
    ?assertEqual({403, ~"stopped by a plugin"}, nova_test_listener:get(?config(port, Config), "/stopped/")).

stopped_request_runs_no_post_request_plugin(Config) ->
    ?assertMatch({403, _}, nova_test_listener:get(?config(port, Config), "/stopped/")),
    ?assertEqual([pre_request], phases()).

denied_request_runs_no_post_request_plugin(Config) ->
    {Status, Headers, _Body} = nova_test_listener:response(?config(port, Config), "GET", "/denied/", []),
    ?assertEqual(401, Status),
    ?assertEqual(~"pre_request", maps:get(~"x-pipeline", Headers)),
    ?assertEqual([pre_request], phases()).

unmatched_path_runs_no_plugin(Config) ->
    {Status, Headers, _Body} = nova_test_listener:response(?config(port, Config), "GET", "/nowhere", []),
    ?assertEqual(404, Status),
    ?assertNot(is_map_key(~"x-pipeline", Headers)),
    ?assertEqual([], phases()).

unmatched_method_runs_no_plugin(Config) ->
    {Status, Headers, Body} = nova_test_listener:response(?config(port, Config), "POST", "/open/",
                                                          [{"x-marked", "yes"}]),
    ?assertEqual({405, ~"method page"}, {Status, Body}),
    ?assertNot(is_map_key(~"x-pipeline", Headers)),
    ?assertEqual([], phases()).

status_page_is_behind_the_security_of_its_group(Config) ->
    {Status, Body} = nova_test_listener:request(?config(port, Config), "POST", "/open/", []),
    ?assertEqual(401, Status),
    ?assertNotEqual(~"method page", Body).

%% Over HTTP/1.1 the post_request plugins run when the upgrade is answered,
%% while the connection is open. The request after it gives them the time.
websocket_upgrade_runs_both_plugin_phases(Config) ->
    ?assertEqual([~"connected", ~"pong"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/open/ws", ~"ping", 2)),
    ?assertMatch({200, _}, nova_test_listener:get(?config(port, Config), "/open/")),
    ?assertEqual([pre_request, post_request, pre_request, post_request], phases()).
