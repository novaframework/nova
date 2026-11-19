%%% Every kind of route target behind a [...] catch-all.
-module(nova_catch_all_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         function_route/1,
         function_route_with_methods/1,
         function_route_keeps_its_extra_state/1,
         function_route_with_extra_state_that_is_not_a_map/1,
         inline_fun_route/1,
         mod_fun_route/1,
         static_route/1,
         websocket_route/1,
         no_trailing_segments/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     function_route,
     function_route_with_methods,
     function_route_keeps_its_extra_state,
     function_route_with_extra_state_that_is_not_a_map,
     inline_fun_route,
     mod_fun_route,
     static_route,
     websocket_route,
     no_trailing_segments
    ].

init_per_suite(Config) ->
    Host = [#{prefix => "",
              routes => [
                         {"/fun/[...]", fun nova_catch_all_pages:pathinfo/1, #{}},
                         {"/fun-get/[...]", fun nova_catch_all_pages:pathinfo/1, #{methods => [get]}},
                         {"/fun-state/[...]", fun nova_catch_all_pages:extra_state/1,
                          #{methods => [get], extra_state => #{answer => 42}}},
                         {"/fun-other-state/[...]", fun nova_catch_all_pages:extra_state/1,
                          #{methods => [get], extra_state => [answer]}},
                         {"/inline/[...]", fun(_Req) -> {status, 200, #{}, ~"inline"} end,
                          #{methods => [get]}},
                         {"/mod-fun/[...]", {nova_catch_all_pages, pathinfo}, #{methods => [get]}},
                         {"/assets/[...]", "assets"},
                         {"/ws/[...]", nova_test_app_ws_controller, #{protocol => ws}}
                        ]}],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

function_route(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"a|b|c"}, nova_test_listener:get(Port, "/fun/a/b/c")),
    ?assertEqual({200, ~"a"}, nova_test_listener:request(Port, "POST", "/fun/a", [])).

function_route_with_methods(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"a|b"}, nova_test_listener:get(Port, "/fun-get/a/b")),
    ?assertMatch({405, _}, nova_test_listener:request(Port, "POST", "/fun-get/a/b", [])).

function_route_keeps_its_extra_state(Config) ->
    {200, Body} = nova_test_listener:get(?config(port, Config), "/fun-state/a/b"),
    ?assertNotEqual(nomatch, binary:match(Body, ~"answer => 42")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"pathinfo => [<<\"a\">>,<<\"b\">>]")).

function_route_with_extra_state_that_is_not_a_map(Config) ->
    ?assertEqual({200, ~"[answer]"}, nova_test_listener:get(?config(port, Config), "/fun-other-state/a/b")).

inline_fun_route(Config) ->
    ?assertEqual({200, ~"inline"}, nova_test_listener:get(?config(port, Config), "/inline/a/b")).

mod_fun_route(Config) ->
    ?assertEqual({200, ~"a|b"}, nova_test_listener:get(?config(port, Config), "/mod-fun/a/b")).

static_route(Config) ->
    ?assertEqual({200, ~"nested file\n"},
                 nova_test_listener:get(?config(port, Config), "/assets/nested/deep.txt")).

websocket_route(Config) ->
    ?assertMatch(<<"HTTP/1.1 101", _/binary>>,
                 nova_test_listener:websocket_handshake(?config(port, Config), "/ws/room/42")).

no_trailing_segments(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"no pathinfo"}, nova_test_listener:get(Port, "/fun")),
    ?assertEqual({200, ~"no pathinfo"}, nova_test_listener:get(Port, "/fun/")).
