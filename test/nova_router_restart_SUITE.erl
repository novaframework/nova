%%% Nova stopped and started again in the same VM serves the routes of the
%%% second start only.
-module(nova_router_restart_SUITE).

-export([all/0]).

-export([
         restart_replaces_the_routing_table/1,
         runtime_listeners_keep_their_own_tables/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(PAGE, {"/", fun nova_router_restart_pages:page/1, #{methods => [get]}}).

all() ->
    [
     restart_replaces_the_routing_table,
     runtime_listeners_keep_their_own_tables
    ].

restart_replaces_the_routing_table(_Config) ->
    Before = [#{prefix => "/old", routes => [?PAGE]},
              #{prefix => "/kept", routes => [?PAGE]}],
    Port1 = nova_test_listener:start(#{host => Before}),
    try
        ok = nova_router:add_routes(runtime_only, #{prefix => "/runtime", routes => [?PAGE]}),
        ?assertMatch({200, _}, nova_test_listener:get(Port1, "/old/")),
        ?assertMatch({200, _}, nova_test_listener:get(Port1, "/kept/")),
        ?assertMatch({200, _}, nova_test_listener:get(Port1, "/runtime/"))
    after
        nova_test_listener:stop()
    end,

    After = [#{prefix => "/new", routes => [?PAGE]},
             #{prefix => "/kept", security => fun nova_router_restart_pages:require_token/1, routes => [?PAGE]}],
    Port2 = nova_test_listener:start(#{host => After}),
    try
        ?assertMatch({404, _}, nova_test_listener:get(Port2, "/old/")),
        ?assertMatch({404, _}, nova_test_listener:get(Port2, "/runtime/")),
        ?assertMatch({200, _}, nova_test_listener:get(Port2, "/new/")),
        ?assertMatch({401, _}, nova_test_listener:get(Port2, "/kept/")),
        ?assertMatch({200, _}, nova_test_listener:get(Port2, "/kept/", [{"authorization", "let-me-in"}])),
        ?assertEqual(false, lists:keyfind(runtime_only, 1, nova:get_apps()))
    after
        nova_test_listener:stop()
    end.

%% Starting from an empty table on boot must leave alone what #405 added:
%% a table per listener, and routes merged in at runtime.
runtime_listeners_keep_their_own_tables(_Config) ->
    Port = nova_test_listener:start(#{host => [#{prefix => "/host", routes => [?PAGE]}]}),
    try
        ok = nova_router:add_routes(runtime_only, #{prefix => "/runtime", routes => [?PAGE]}),
        nova_test_mounted_router:set_routes([#{prefix => "/second", routes => [?PAGE]}]),
        Second = nova_test_listener:free_port(),
        {ok, nova_test_sub_app, _Host, Second} =
            nova_sup:add_application(nova_test_sub_app, #{port => Second}),
        try
            ?assertMatch({200, _}, nova_test_listener:get(Second, "/second/")),
            ?assertMatch({404, _}, nova_test_listener:get(Second, "/host/")),
            ?assertMatch({404, _}, nova_test_listener:get(Port, "/second/")),
            ?assertMatch({200, _}, nova_test_listener:get(Port, "/host/")),
            ?assertMatch({200, _}, nova_test_listener:get(Port, "/runtime/"))
        after
            nova_sup:remove_application(nova_test_sub_app)
        end,

        %% The same application and port again: the listener starts from an
        %% empty table, not from the one its predecessor left behind.
        nova_test_mounted_router:set_routes([#{prefix => "/third", routes => [?PAGE]}]),
        {ok, nova_test_sub_app, _Host2, Second} =
            nova_sup:add_application(nova_test_sub_app, #{port => Second}),
        try
            ?assertMatch({200, _}, nova_test_listener:get(Second, "/third/")),
            ?assertMatch({404, _}, nova_test_listener:get(Second, "/second/"))
        after
            nova_sup:remove_application(nova_test_sub_app)
        end
    after
        nova_test_listener:stop()
    end.
