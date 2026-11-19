%%% A status page that fails itself is not rendered again: the request gets
%%% the 500 page, or a bare 500 when that page is the one failing.
-module(nova_status_page_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([crash_renders_the_500_page/1,
         crashing_500_page_runs_once/1,
         throwing_500_page_runs_once/1,
         crashing_500_page_logs_two_errors/1,
         crashing_404_page_renders_the_500_page/1,
         throwing_404_page_ends_in_the_500_page/1,
         thrown_404_with_a_throwing_page_renders_the_500_page/1,
         both_pages_crashing_run_once_each/1,
         thrown_500_without_a_map_is_a_500/1,
         returned_status_keeps_the_request_of_the_route/1,
         crashing_security_callback_renders_the_500_page/1,
         crashing_security_callback_does_not_upgrade/1,
         crashing_plugin_renders_the_500_page/1,
         crashing_page_of_a_refused_request_runs_once/1,
         page_gets_the_same_request_on_every_path/1,
         page_after_a_failed_page_gets_its_own_status/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(OBSERVER, nova_status_page_observer).
-define(PAGES, nova_status_page_pages).

all() ->
    [crash_renders_the_500_page,
     crashing_500_page_runs_once,
     throwing_500_page_runs_once,
     crashing_500_page_logs_two_errors,
     crashing_404_page_renders_the_500_page,
     throwing_404_page_ends_in_the_500_page,
     thrown_404_with_a_throwing_page_renders_the_500_page,
     both_pages_crashing_run_once_each,
     thrown_500_without_a_map_is_a_500,
     returned_status_keeps_the_request_of_the_route,
     crashing_security_callback_renders_the_500_page,
     crashing_security_callback_does_not_upgrade,
     crashing_plugin_renders_the_500_page,
     crashing_page_of_a_refused_request_runs_once,
     page_gets_the_same_request_on_every_path,
     page_after_a_failed_page_gets_its_own_status].

init_per_suite(Config) ->
    Routes = [#{prefix => "",
                plugins => [],
                routes => [{"/crash", fun ?PAGES:crash/1, #{methods => [get]}},
                           {"/throw", fun ?PAGES:throw_500/1, #{methods => [get]}},
                           {"/gone", fun ?PAGES:throw_404/1, #{methods => [get]}},
                           {"/odd", fun ?PAGES:odd/1, #{methods => [get]}},
                           {404, fun ?PAGES:not_found/1, #{}},
                           {500, fun ?PAGES:server_error/1, #{}}]},
              #{prefix => "/guarded",
                security => fun ?PAGES:crashing_security/1,
                plugins => [],
                routes => [{"/page", fun ?PAGES:bare_404/1, #{methods => [get]}},
                           {"/ws", nova_ws_lifecycle_pages, #{protocol => ws}}]},
              #{prefix => "/failing-plugin",
                plugins => [{pre_request, ?PAGES, #{}}],
                routes => [{"/page", fun ?PAGES:bare_404/1, #{methods => [get]}}]},
              #{prefix => "/bare",
                plugins => [{post_request, ?PAGES, #{}}],
                routes => [{"/:id", fun ?PAGES:bare_404/1, #{methods => [get], extra_state => #{kept => true}}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    register(?OBSERVER, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    application:unset_env(nova, render_error_pages).

crash_renders_the_500_page(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/crash")),
    ?assertEqual([{page, 500}], observations()).

crashing_500_page_runs_once(Config) ->
    ?assertMatch({500, _}, nova_test_listener:get(?config(port, Config), "/crash", [{"x-500", "crash"}])),
    ?assertEqual([{page, 500}], observations()).

throwing_500_page_runs_once(Config) ->
    ?assertMatch({500, _}, nova_test_listener:get(?config(port, Config), "/crash", [{"x-500", "throw"}])),
    ?assertEqual([{page, 500}], observations()).

crashing_500_page_logs_two_errors(Config) ->
    {_Response, Events} =
        nova_test_log:collect(fun() ->
                                      nova_test_listener:get(?config(port, Config), "/crash", [{"x-500", "crash"}])
                              end),
    ?assertMatch([#{msg := ~"Controller crashed"}, #{msg := ~"Status page crashed", status_page := 500}],
                 nova_test_log:errors(Events)).

crashing_404_page_renders_the_500_page(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/nowhere", [{"x-404", "crash"}])),
    ?assertEqual([{page, 404}, {page, 500}], observations()).

%% When no route matches, the 404 page is what answers the request, and it
%% is a status page there too: the status it throws does not ask for it once
%% more.
throwing_404_page_ends_in_the_500_page(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/nowhere", [{"x-404", "throw"}])),
    ?assertEqual([{page, 404}, {page, 500}], observations()).

thrown_404_with_a_throwing_page_renders_the_500_page(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/gone", [{"x-404", "throw"}])),
    ?assertEqual([{page, 404}, {page, 500}], observations()).

both_pages_crashing_run_once_each(Config) ->
    ?assertMatch({500, _}, nova_test_listener:get(?config(port, Config), "/nowhere",
                                                  [{"x-404", "crash"}, {"x-500", "crash"}])),
    ?assertEqual([{page, 404}, {page, 500}], observations()).

thrown_500_without_a_map_is_a_500(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/throw")),
    ?assertEqual([{page, 500}], observations()).

%% A returned {status, Code} is sent bare. The page for that status does not
%% run, and the plugins after the controller still see the route's request.
returned_status_keeps_the_request_of_the_route(Config) ->
    ?assertEqual({404, <<>>}, nova_test_listener:get(?config(port, Config), "/bare/7")),
    ?assertEqual([{post_request, #{~"id" => ~"7"}, #{kept => true}}], observations()).

%% As for a crashing plugin or controller, and without reaching the route.
crashing_security_callback_renders_the_500_page(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/guarded/page")),
    ?assertEqual([{page, 500}], observations()).

crashing_security_callback_does_not_upgrade(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/guarded/ws")),
    ?assertEqual([{page, 500}], observations()).

crashing_plugin_renders_the_500_page(Config) ->
    ?assertEqual({500, ~"page 500"}, nova_test_listener:get(?config(port, Config), "/failing-plugin/page")),
    ?assertEqual([{page, 500}], observations()).

%% The page the router, a plugin or the security callback asked for is a
%% status page as well.
crashing_page_of_a_refused_request_runs_once(Config) ->
    Port = ?config(port, Config),
    [begin
         ?assertMatch({500, _}, nova_test_listener:get(Port, Path, Headers)),
         ?assertEqual({Path, Pages}, {Path, observations()})
     end || {Path, Headers, Pages} <- [{"/guarded/page", [{"x-500", "crash"}], [{page, 500}]},
                                       {"/failing-plugin/page", [{"x-500", "throw"}], [{page, 500}]},
                                       {"/nowhere", [{"x-404", "crash"}, {"x-500", "crash"}],
                                        [{page, 404}, {page, 500}]},
                                       {"/nowhere", [{"x-404", "throw"}], [{page, 404}, {page, 500}]}]].

%% Whichever way a status page is reached, its controller finds the status
%% in resp_status_code and in controller_data, and crash_info when something
%% failed.
page_gets_the_same_request_on_every_path(Config) ->
    [?assertEqual({Path, {Status, shape(Status, Data, Crashed)}}, {Path, shape_of(Config, Path)})
     || {Path, Status, Data, Crashed} <- [{"/nowhere", 404, #{error => "Not found in path"}, false},
                                          {"/gone", 404, #{}, true},
                                          {"/crash", 500, #{}, true},
                                          {"/throw", 500, #{}, true},
                                          {"/odd", 500, #{}, true},
                                          {"/guarded/page", 500, #{}, true},
                                          {"/failing-plugin/page", 500, #{}, true}]].

page_after_a_failed_page_gets_its_own_status(Config) ->
    ?assertEqual({500, shape(500, #{}, true)},
                 nova_test_listener:get(?config(port, Config), "/nowhere", [{"x-404", "crash"}, {"x-500", "shape"}])).

shape_of(Config, Path) ->
    nova_test_listener:get(?config(port, Config), Path, [{"x-404", "shape"}, {"x-500", "shape"}]).

shape(Status, Data, Crashed) ->
    iolist_to_binary(io_lib:format("~0p", [{Status, #{status => Status, data => Data}, Crashed}])).

observations() ->
    receive
        {observed, What} -> [What | observations()]
    after 300 ->
        []
    end.
