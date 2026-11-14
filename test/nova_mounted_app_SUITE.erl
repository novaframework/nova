%%% A mounted application keeps the security callback it declares on its own
%%% routes, whatever the application mounting it sets in nova_apps.
-module(nova_mounted_app_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         group_callback_survives_secure_false/1,
         group_without_callback_stays_open/1
        ]).

-export([page/1, require_token/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     group_callback_survives_secure_false,
     group_without_callback_stays_open
    ].

init_per_suite(Config) ->
    Routes = [{"/", fun ?MODULE:page/1, #{methods => [get]}}],
    Mounted = [
               #{prefix => "/admin", security => fun ?MODULE:require_token/1, routes => Routes},
               #{prefix => "/open", routes => Routes}
              ],
    Port = nova_test_listener:start(#{mounted => Mounted,
                                      mounted_options => #{prefix => "/mounted", secure => false}}),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

page(_Req) ->
    {status, 200, #{}, ~"page"}.

require_token(Req) ->
    cowboy_req:header(~"authorization", Req) =:= ~"let-me-in".

group_callback_survives_secure_false(Config) ->
    Port = ?config(port, Config),
    ?assertMatch({401, _}, nova_test_listener:get(Port, "/mounted/admin/")),
    ?assertEqual({200, ~"page"},
                 nova_test_listener:get(Port, "/mounted/admin/", [{"authorization", "let-me-in"}])).

group_without_callback_stays_open(Config) ->
    ?assertEqual({200, ~"page"}, nova_test_listener:get(?config(port, Config), "/mounted/open/")).
