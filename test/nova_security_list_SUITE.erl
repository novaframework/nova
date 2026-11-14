%%% A route with the `secure` of its mount and the `security` of its route
%%% group, and a plugin listed under `plugins_after_security`: the plugin runs
%%% once, after both callbacks have allowed the request, and not at all for a
%%% request one of them refuses or crashes on.
-module(nova_security_list_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([plugin_runs_once_after_both_allow/1,
         plugin_does_not_run_for_a_refused_request/1,
         crash_of_either_callback_renders_the_500_page/1,
         websocket_plugin_runs_once_after_both_allow/1,
         websocket_is_refused_without_the_plugin/1,
         websocket_gets_the_500_page_for_a_crash/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [plugin_runs_once_after_both_allow,
     plugin_does_not_run_for_a_refused_request,
     crash_of_either_callback_renders_the_500_page,
     websocket_plugin_runs_once_after_both_allow,
     websocket_is_refused_without_the_plugin,
     websocket_gets_the_500_page_for_a_crash].

init_per_suite(Config) ->
    Routes = [{"/", fun nova_secure_layers_pages:page/1, #{methods => [get]}},
              {"/ws", nova_secure_layers_pages, #{protocol => ws}}],
    Port = nova_test_listener:start(
             #{mounted_options => #{prefix => "/mounted", secure => fun nova_secure_layers_pages:mount/1},
               mounted => [#{prefix => "/both",
                             security => fun nova_secure_layers_pages:group/1,
                             plugins => [{pre_request, nova_security_list_pages, #{}}],
                             plugins_after_security => [nova_security_list_pages],
                             routes => Routes}]}),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    ok = nova_secure_layers_pages:report_calls_to_me(),
    Config.

end_per_testcase(_Case, _Config) ->
    nova_secure_layers_pages:stop_reporting_calls().

plugin_runs_once_after_both_allow(Config) ->
    ?assertEqual({200, ~"group", [mount, group, plugin]}, get(Config, "/mounted/both/?mount=data&group=data")),
    ?assertEqual({200, ~"mount", [mount, group, plugin]}, get(Config, "/mounted/both/?mount=data&group=allow")).

plugin_does_not_run_for_a_refused_request(Config) ->
    ?assertMatch({401, _Body, [mount]}, get(Config, "/mounted/both/?mount=deny&group=allow")),
    ?assertMatch({401, _Body, [mount, group]}, get(Config, "/mounted/both/?mount=allow&group=deny")),
    ?assertMatch({302, _Body, [mount, group]}, get(Config, "/mounted/both/?mount=allow&group=redirect")).

crash_of_either_callback_renders_the_500_page(Config) ->
    {500, FromMount, [mount]} = get(Config, "/mounted/both/?mount=raise&group=allow"),
    {500, FromGroup, [mount, group]} = get(Config, "/mounted/both/?mount=data&group=raise"),
    [?assertNotEqual(nomatch, binary:match(Page, ~"Internal Server Error")) || Page <- [FromMount, FromGroup]].

websocket_plugin_runs_once_after_both_allow(Config) ->
    ?assertEqual([~"group"],
                 nova_test_listener:websocket_texts(?config(port, Config), "/mounted/both/ws?mount=data&group=data",
                                                    ~"who", 1)),
    ?assertEqual([mount, group, plugin], nova_secure_layers_pages:calls()).

websocket_is_refused_without_the_plugin(Config) ->
    ?assertEqual({401, [mount]}, handshake(Config, "/mounted/both/ws?mount=deny&group=allow")),
    ?assertEqual({403, [mount, group]}, handshake(Config, "/mounted/both/ws?mount=allow&group=deny-status")).

websocket_gets_the_500_page_for_a_crash(Config) ->
    ?assertEqual({500, [mount]}, handshake(Config, "/mounted/both/ws?mount=raise&group=allow")),
    ?assertEqual({500, [mount, group]}, handshake(Config, "/mounted/both/ws?mount=allow&group=raise")).

get(Config, Path) ->
    {Status, _Headers, Body} = nova_test_listener:response(?config(port, Config), "GET", Path,
                                                           [{"accept", "text/html"}]),
    {Status, Body, nova_secure_layers_pages:calls()}.

handshake(Config, Path) ->
    <<"HTTP/1.1 ", Status:3/binary, _/binary>> = nova_test_listener:websocket_handshake(?config(port, Config), Path),
    {binary_to_integer(Status), nova_secure_layers_pages:calls()}.
