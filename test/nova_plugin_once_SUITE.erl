%%% A plugin chain runs at most once for a request, also when a plugin of it
%%% fails and Nova renders the 500 status page, and it ends at the plugin that
%%% answers the request.
-module(nova_plugin_once_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([crashing_plugin_on_a_websocket_route/1,
         crashing_plugin_on_an_http_route/1,
         plugin_returning_an_error_on_a_websocket_route/1,
         crashing_plugin_after_the_security_callback/1,
         passing_plugins_on_an_http_route/1,
         breaking_plugin_sends_its_reply/1,
         breaking_plugin_on_a_websocket_route/1,
         breaking_plugin_that_replied_itself/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(OBSERVER, nova_plugin_once_observer).
-define(PLUGIN, nova_plugin_once_pages).

all() ->
    [crashing_plugin_on_a_websocket_route,
     crashing_plugin_on_an_http_route,
     plugin_returning_an_error_on_a_websocket_route,
     crashing_plugin_after_the_security_callback,
     passing_plugins_on_an_http_route,
     breaking_plugin_sends_its_reply,
     breaking_plugin_on_a_websocket_route,
     breaking_plugin_that_replied_itself].

init_per_suite(Config) ->
    Chain = fun(Action) ->
                    [{pre_request, ?PLUGIN, #{name => first}},
                     {pre_request, ?PLUGIN, #{name => second, action => Action}},
                     {pre_request, ?PLUGIN, #{name => third}},
                     {post_request, ?PLUGIN, #{name => last}}]
            end,
    Routes = [#{prefix => "/crash",
                plugins => Chain(crash),
                routes => [{"/ws", ?PLUGIN, #{protocol => ws}},
                           {"/page", fun ?PLUGIN:index/1, #{methods => [get]}}]},
              #{prefix => "/error",
                plugins => Chain(error),
                routes => [{"/ws", ?PLUGIN, #{protocol => ws}}]},
              #{prefix => "/late",
                security => fun ?PLUGIN:allow/1,
                plugins => Chain(crash),
                routes => [{"/ws", ?PLUGIN, #{protocol => ws}}]},
              #{prefix => "/pass",
                plugins => Chain(pass),
                routes => [{"/page", fun ?PLUGIN:index/1, #{methods => [get]}}]},
              #{prefix => "/break",
                plugins => Chain(break),
                routes => [{"/ws", ?PLUGIN, #{protocol => ws}},
                           {"/page", fun ?PLUGIN:index/1, #{methods => [get]}}]},
              #{prefix => "/replied",
                plugins => Chain(break_replied),
                routes => [{"/page", fun ?PLUGIN:index/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    register(?OBSERVER, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

crashing_plugin_on_a_websocket_route(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/crash/ws")),
    ?assertEqual([{pre_request, first}, {pre_request, second}, {post_request, last}], observations()).

crashing_plugin_on_an_http_route(Config) ->
    ?assertMatch({500, _}, nova_test_listener:get(?config(port, Config), "/crash/page")),
    ?assertEqual([{pre_request, first}, {pre_request, second}, {post_request, last}], observations()).

plugin_returning_an_error_on_a_websocket_route(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/error/ws")),
    ?assertEqual([{pre_request, first}, {pre_request, second}, {post_request, last}], observations()).

crashing_plugin_after_the_security_callback(Config) ->
    ?assertMatch(<<"HTTP/1.1 500", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/late/ws")),
    ?assertEqual([security, {pre_request, first}, {pre_request, second}, {post_request, last}], observations()).

passing_plugins_on_an_http_route(Config) ->
    ?assertMatch({200, ~"index"}, nova_test_listener:get(?config(port, Config), "/pass/page")),
    ?assertEqual([{pre_request, first}, {pre_request, second}, {pre_request, third}, {post_request, last}],
                 observations()).

%% break is the deprecated spelling of stop: the plugins after it and the
%% controller do not run, and the client gets the reply of the plugin.
breaking_plugin_sends_its_reply(Config) ->
    ?assertEqual({403, ~"refused"}, nova_test_listener:get(?config(port, Config), "/break/page")),
    ?assertEqual([{pre_request, first}, {pre_request, second}], observations()).

breaking_plugin_on_a_websocket_route(Config) ->
    ?assertMatch(<<"HTTP/1.1 403", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/break/ws")),
    ?assertEqual([{pre_request, first}, {pre_request, second}], observations()).

breaking_plugin_that_replied_itself(Config) ->
    ?assertEqual({402, ~"pay first"}, nova_test_listener:get(?config(port, Config), "/replied/page")),
    ?assertEqual([{pre_request, first}, {pre_request, second}], observations()).

%% Everything observed until nothing more has come for a while, so a second
%% run of a plugin is seen as well.
observations() ->
    receive
        {observed, What} -> [What | observations()]
    after 300 ->
        []
    end.
