%%% A plugin that first appears in routes added while Nova is running is
%%% initialised like one that was there at boot.
-module(nova_runtime_plugin_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([plugin_added_at_runtime_is_initialised/1]).

-export([page/1, init/0, pre_request/4, plugin_info/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [plugin_added_at_runtime_is_initialised].

init_per_suite(Config) ->
    [{port, nova_test_listener:start(#{})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

page(#{requests_before := Count}) ->
    {status, 200, #{}, integer_to_binary(Count)}.

init() ->
    0.

pre_request(Req, _Env, _Options, Count) ->
    {ok, Req#{requests_before => Count}, Count + 1}.

plugin_info() ->
    #{title => ~"counting plugin",
      version => ~"1.0.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team <info@novaframework.org>"],
      description => ~"Counts the requests it has seen",
      options => []}.

plugin_added_at_runtime_is_initialised(Config) ->
    Port = ?config(port, Config),
    ok = nova_router:add_routes(nova_test_app,
                                [#{prefix => "/late",
                                   plugins => [{pre_request, ?MODULE, #{}}],
                                   routes => [{"/", fun ?MODULE:page/1, #{methods => [get]}}]}]),
    ?assertEqual({200, ~"0"}, nova_test_listener:get(Port, "/late/")),
    ?assertEqual({200, ~"1"}, nova_test_listener:get(Port, "/late/")).
