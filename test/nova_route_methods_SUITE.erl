%%% A route answers the methods it declares and no others, however the
%%% method is spelled, and a method Nova does not know stops the boot.
-module(nova_route_methods_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         every_spelling_restricts_the_route/1,
         unknown_method_fails_the_boot/1,
         mistyped_method_fails_the_boot/1
        ]).

-export([page/1]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     every_spelling_restricts_the_route,
     unknown_method_fails_the_boot,
     mistyped_method_fails_the_boot
    ].

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

page(_Req) ->
    {status, 200, #{}, ~"page"}.

every_spelling_restricts_the_route(_Config) ->
    Spellings = [{"/atom", post}, {"/upper-atom", 'POST'}, {"/binary", ~"POST"},
                 {"/lower-binary", ~"post"}, {"/string", "POST"}, {"/lower-string", "post"}],
    Routes = [{Path, fun ?MODULE:page/1, #{methods => [Method]}} || {Path, Method} <- Spellings],
    Port = nova_test_listener:start(#{host => [#{routes => Routes}]}),
    [begin
         ?assertMatch({405, _}, nova_test_listener:get(Port, Path)),
         ?assertMatch({405, _}, nova_test_listener:request(Port, "DELETE", Path, [])),
         ?assertEqual({200, ~"page"}, nova_test_listener:request(Port, "POST", Path, []))
     end || {Path, _Method} <- Spellings].

unknown_method_fails_the_boot(_Config) ->
    Routes = [{"/", fun ?MODULE:page/1, #{methods => [update]}}],
    ?assertMatch({error, _}, nova_test_listener:try_start(#{host => [#{routes => Routes}]})).

mistyped_method_fails_the_boot(_Config) ->
    Routes = [{"/", fun ?MODULE:page/1, #{methods => [get, pots]}}],
    ?assertMatch({error, _}, nova_test_listener:try_start(#{host => [#{routes => Routes}]})).
