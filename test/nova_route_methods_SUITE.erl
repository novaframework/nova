%%% A route answers the methods it declares and no others. An atom is one of
%%% the methods Nova knows, in either case. A binary or a string is any
%%% method, in the case it is sent. Anything else stops the boot.
-module(nova_route_methods_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         every_spelling_restricts_the_route/1,
         extension_method_is_served/1,
         binary_and_string_keep_their_case/1,
         unknown_method_fails_the_boot/1,
         mistyped_method_fails_the_boot/1,
         extension_method_as_an_atom_fails_the_boot/1,
         method_that_is_not_a_token_fails_the_boot/1
        ]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     every_spelling_restricts_the_route,
     extension_method_is_served,
     binary_and_string_keep_their_case,
     unknown_method_fails_the_boot,
     mistyped_method_fails_the_boot,
     extension_method_as_an_atom_fails_the_boot,
     method_that_is_not_a_token_fails_the_boot
    ].

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

every_spelling_restricts_the_route(_Config) ->
    Spellings = [{"/atom", post}, {"/upper-atom", 'POST'}, {"/mixed-atom", 'Post'},
                 {"/binary", ~"POST"}, {"/string", "POST"}],
    Routes = [{Path, fun nova_route_methods_pages:page/1, #{methods => [Method]}} || {Path, Method} <- Spellings],
    Port = nova_test_listener:start(#{host => [#{routes => Routes}]}),
    [begin
         ?assertMatch({405, _}, nova_test_listener:get(Port, Path)),
         ?assertMatch({405, _}, nova_test_listener:request(Port, "DELETE", Path, [])),
         ?assertEqual({200, ~"page"}, nova_test_listener:request(Port, "POST", Path, []))
     end || {Path, _Method} <- Spellings].

extension_method_is_served(_Config) ->
    Methods = [{"/purge", ~"PURGE"}, {"/propfind", "PROPFIND"}, {"/query", ~"QUERY"}, {"/m-search", "M-SEARCH"}],
    Routes = [{Path, fun nova_route_methods_pages:page/1, #{methods => [Method]}} || {Path, Method} <- Methods],
    Port = nova_test_listener:start(#{host => [#{routes => Routes}]}),
    [begin
         ?assertEqual({200, ~"page"}, nova_test_listener:request(Port, Method, Path, [])),
         {Status, Headers, _Body} = nova_test_listener:response(Port, "GET", Path, []),
         ?assertEqual({405, iolist_to_binary(Method)}, {Status, maps:get(~"allow", Headers)})
     end || {Path, Method} <- Methods].

binary_and_string_keep_their_case(_Config) ->
    Routes = [{"/binary", fun nova_route_methods_pages:page/1, #{methods => [~"purge"]}},
              {"/string", fun nova_route_methods_pages:page/1, #{methods => ["Purge"]}},
              {"/upper", fun nova_route_methods_pages:page/1, #{methods => [~"PURGE"]}}],
    Port = nova_test_listener:start(#{host => [#{routes => Routes}]}),
    ?assertEqual({200, ~"page"}, nova_test_listener:request(Port, "purge", "/binary", [])),
    ?assertMatch({405, _}, nova_test_listener:request(Port, "PURGE", "/binary", [])),
    ?assertEqual({200, ~"page"}, nova_test_listener:request(Port, "Purge", "/string", [])),
    ?assertMatch({405, _}, nova_test_listener:request(Port, "PURGE", "/string", [])),
    ?assertEqual({200, ~"page"}, nova_test_listener:request(Port, "PURGE", "/upper", [])),
    ?assertMatch({405, _}, nova_test_listener:request(Port, "purge", "/upper", [])).

unknown_method_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_route_methods_pages:page/1, #{methods => [update]}}],
    ?assertMatch({error, _}, nova_test_listener:try_start(#{host => [#{routes => Routes}]})).

mistyped_method_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_route_methods_pages:page/1, #{methods => [get, pots]}}],
    ?assertMatch({error, _}, nova_test_listener:try_start(#{host => [#{routes => Routes}]})).

extension_method_as_an_atom_fails_the_boot(_Config) ->
    Routes = [{"/", fun nova_route_methods_pages:page/1, #{methods => [purge]}}],
    ?assertMatch({error, _}, nova_test_listener:try_start(#{host => [#{routes => Routes}]})).

method_that_is_not_a_token_fails_the_boot(_Config) ->
    [begin
         Routes = [{"/", fun nova_route_methods_pages:page/1, #{methods => [Method]}}],
         ?assertMatch({error, _}, nova_test_listener:try_start(#{host => [#{routes => Routes}]}), Method),
         ok = nova_test_listener:stop()
     end || Method <- [~"GET POST", "GET,POST", ~"", "", ~"GET\r\n", <<"G", 233, "T">>]].
