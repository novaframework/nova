%%% The listeners of the `listeners` configuration: each has a name, the
%%% HTTP versions it serves and the applications it serves them for, and a
%%% node that asks for what its server cannot do does not start.
-module(nova_listeners_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         listener_apps/1,
         listener_protocols/1,
         listener_names/1,
         add_to_listener/1,
         unsupported_protocol/1,
         http3_needs_tls/1,
         unknown_setting/1,
         bind_failure/1,
         server_stop/1,
         nova_stop/1,
         listener_exists/1,
         early_add/1,
         early_add_failure/1,
         early_add_routes/1,
         cowboy_ws_http2/1,
         early_add_configured/1,
         add_configured/1,
         server_restart/1,
         restart_unserved_group/1,
         kept_listener/1,
         kept_by_bind/1,
         kept_while_stopped/1,
         kept_name_configured/1,
         kept_named_configured/1,
         early_on_kept/1,
         early_names_kept/1,
         early_remove_named/1,
         readdressed_listener/1,
         address_taken/1,
         settings_ignored/1,
         listener_replaced/1,
         table_replaced/1,
         routes_changed/1,
         own_table/1,
         removed_while_built/1,
         outlived_on_listener/1,
         outlived_on_port/1,
         registry_dies_adding/1,
         registry_restart/1,
         restart_keeps_added/1,
         registry_restart_stopped/1,
         registry_first_start/1,
         registry_after_failed_start/1,
         listeners_gone/1,
         gone_while_starting/1,
         owner_gone/1,
         runtime_raise/1,
         router_dies/1,
         stuck_add/1,
         queued_add/1,
         forged_drain/1,
         stopped_while_binding/1,
         killed_while_binding/1,
         killed_before_binding/1,
         invalid_settings/1,
         moved_listener/1,
         added_during_start/1,
         early_serves_group/1,
         refused_listener/1,
         early_add_named/1,
         early_listener_exists/1,
         early_unserved_group/1,
         early_group_served/1,
         add_while_draining/1,
         concurrent_adds/1,
         start_error_secrets/1,
         no_server_warning/1,
         server_grace/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).

all() ->
    [
     listener_apps,
     listener_protocols,
     listener_names,
     add_to_listener,
     unsupported_protocol,
     http3_needs_tls,
     unknown_setting,
     bind_failure,
     server_stop,
     nova_stop,
     listener_exists,
     early_add,
     early_add_failure,
     early_add_routes,
     cowboy_ws_http2,
     early_add_configured,
     add_configured,
     server_restart,
     restart_unserved_group,
     kept_listener,
     kept_by_bind,
     kept_while_stopped,
     kept_name_configured,
     kept_named_configured,
     early_on_kept,
     early_names_kept,
     early_remove_named,
     readdressed_listener,
     address_taken,
     settings_ignored,
     listener_replaced,
     table_replaced,
     routes_changed,
     own_table,
     removed_while_built,
     outlived_on_listener,
     outlived_on_port,
     registry_dies_adding,
     registry_restart,
     restart_keeps_added,
     registry_restart_stopped,
     registry_first_start,
     registry_after_failed_start,
     listeners_gone,
     gone_while_starting,
     owner_gone,
     runtime_raise,
     router_dies,
     stuck_add,
     queued_add,
     forged_drain,
     stopped_while_binding,
     killed_while_binding,
     killed_before_binding,
     invalid_settings,
     moved_listener,
     added_during_start,
     early_serves_group,
     refused_listener,
     early_add_named,
     early_listener_exists,
     early_unserved_group,
     early_group_served,
     add_while_draining,
     concurrent_adds,
     start_error_secrets,
     no_server_warning,
     server_grace
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

listener_apps(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual({200, ~"public http1"}, nova_test_listener:get(Public, "/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Admin, "/only-public")),
    ?assertMatch({200, _}, nova_test_listener:get(Public, "/only-public")).

%% A listener without TLS that serves HTTP/2 alone is one every server has.
listener_protocols(_Config) ->
    {Public, Admin} = two_listeners([http2]),
    ?assertEqual(200, nova_test_listener:h2_get(Public, ~"/where")),
    ?assertNotMatch({200, _}, attempt(fun() -> nova_test_listener:get(Public, "/where") end)),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/where")),
    ?assertNotEqual(200, attempt(fun() -> nova_test_listener:h2_get(Admin, ~"/where") end)).

%% What a request gave, or that it could not be made.
attempt(Request) ->
    try
        Request()
    catch
        _Class:_Reason -> failed
    end.

listener_names(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual([admin, public], lists:sort(nova_sup:listeners())),
    ?assertEqual([#{app => nova_test_app, host => {0, 0, 0, 0}, port => Public, listener => public},
                  #{app => nova_test_sub_app, host => {127, 0, 0, 1}, port => Admin, listener => admin}],
                 lists:sort(nova_sup:get_started_applications())).

add_to_listener(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => []}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/where")),
    ?assertEqual({error, {no_such_listener, admin}},
                 nova_sup:add_application(nova_test_sub_app, #{listener => admin})),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/where")),
    ?assertEqual([web], nova_sup:listeners()).

unsupported_protocol(_Config) ->
    Port = nova_test_listener:free_port(),
    {Settings, Refusal} =
        case nova_test_listener:server(#{}) of
            nova_cowboy ->
                {#{tls => #{certfile => "cert.pem"}, protocols => [http1, http3]},
                 {listener_protocols_not_supported, web, [http3], nova_cowboy}};
            nova_roadrunner ->
                {#{protocols => [http1, http2]},
                 {listener_without_tls_serves_one_protocol, web, [http1, http2]}}
        end,
    {error, {_Server, {{shutdown, {failed_to_start_child, nova_listener, Reason}}, _}}} =
        nova_test_listener:try_start(#{port => Port, listeners => #{web => Settings#{port => Port}}}),
    ?assertEqual(Refusal, Reason),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)).

http3_needs_tls(_Config) ->
    Port = nova_test_listener:free_port(),
    Listeners = #{web => #{port => Port, protocols => [http3]}},
    {error, {_Server, {{shutdown, {failed_to_start_child, nova_listener, Reason}}, _}}} =
        nova_test_listener:try_start(#{port => Port, listeners => Listeners}),
    {Refused, Listener, Protocols} = case Reason of
                                         {listener_protocols_need_tls, Name, Versions} -> {tls, Name, Versions};
                                         {listener_protocols_not_supported, Name, Versions, _On} -> {server, Name, Versions}
                                     end,
    ?assert(lists:member(Refused, [tls, server])),
    ?assertEqual({web, [http3]}, {Listener, Protocols}).

unknown_setting(_Config) ->
    Port = nova_test_listener:free_port(),
    ?assertMatch({error, {_Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {unknown_listener_settings, web, [use_ssl]}}}, _}}},
                 nova_test_listener:try_start(#{port => Port, listeners => #{web => #{port => Port, use_ssl => true}}})).

bind_failure(_Config) ->
    First = nova_test_listener:free_port(),
    {ok, Taken} = gen_tcp:listen(0, [{ip, {0, 0, 0, 0}}]),
    {ok, Second} = inet:port(Taken),
    try
        Listeners = #{a => #{port => First, applications => [nova_test_app]},
                      b => #{port => Second, applications => [nova_test_sub_app]}},
        ?assertMatch({error, {_Server,
                              {{shutdown, {failed_to_start_child, nova_listener, {listener_not_started, eaddrinuse}}},
                               _}}},
                     nova_test_listener:try_start(#{port => First, listeners => Listeners, nova_apps => []})),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, First, [], 1000))
    after
        gen_tcp:close(Taken)
    end.

server_stop(_Config) ->
    {Public, Admin} = two_listeners(),
    ok = application:stop(nova_test_app),
    ok = application:stop(nova_test_listener:server(#{})),
    ?assertEqual([], nova_sup:listeners()),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Public, [], 1000)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Admin, [], 1000)),
    ?assertNotEqual(undefined, whereis(nova_sup)).

%% The owner of the listeners watches nova_sup, and takes the server
%% package down with it when nova stops under it.
nova_stop(_Config) ->
    {Public, Admin} = two_listeners(),
    Supervisor = list_to_existing_atom(atom_to_list(nova_test_listener:server(#{})) ++ "_sup"),
    Monitor = monitor(process, Supervisor),
    ok = application:stop(nova_test_app),
    ok = application:stop(nova),
    receive
        {'DOWN', Monitor, process, _Supervisor, _Reason} -> ok
    after 5000 ->
            error(server_package_still_running)
    end,
    [?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)) || Port <- [Public, Admin]].

%% A listener started for an application is named after it.
listener_exists(_Config) ->
    Port = nova_test_listener:free_port(),
    Listeners = #{nova_test_sub_app => #{port => Port, applications => [nova_test_app]}},
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => Listeners,
                                                host => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    ?assertEqual({error, {listener_exists, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, #{port => nova_test_listener:free_port()})),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Port, "/where")),
    ?assertEqual([nova_test_sub_app], nova_sup:listeners()).

%% A library that adds its application from its start/2 may be started
%% before the server package is.
early_add(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    ?assertEqual({error, {no_such_listener, admin}},
                 nova_sup:add_application(nova_test_sub_app, #{listener => admin})),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertEqual({error, {already_started, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertEqual([], nova_sup:get_started_applications()),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

early_add_failure(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Taken} = gen_tcp:listen(0, [{ip, {0, 0, 0, 0}}]),
    {ok, Other} = inet:port(Taken),
    Options = #{port => Port, listeners => #{web => #{port => Port}}, host => [#{routes => [?WHERE]}],
                nova_apps => []},
    try
        ok = nova_test_listener:configure(Options),
        {ok, _} = application:ensure_all_started(nova),
        {ok, nova_test_sub_app, _Host, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
        ?assertMatch({error, {_Server,
                              {{shutdown, {failed_to_start_child, nova_listener,
                                           {application_not_added, nova_test_sub_app,
                                            {listener_not_started, eaddrinuse}}}}, _}}},
                     nova_test_listener:boot(Options)),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)),
        ok = nova_sup:remove_application(nova_test_sub_app),
        ?assertEqual({error, not_found}, nova_sup:remove_application(nova_test_sub_app)),
        ?assertEqual({ok, Port}, nova_test_listener:boot(Options))
    after
        gen_tcp:close(Taken)
    end.

%% As does one whose routes cannot be compiled.
early_add_routes(_Config) ->
    Port = nova_test_listener:free_port(),
    Unserved = {"/where", fun nova_listeners_pages:where/1, #{methods => [get], http_versions => [http2]}},
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{routes => [Unserved]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_sub_app, _Host, Port} = nova_sup:add_application(nova_test_sub_app, #{listener => web}),
    {Booted, Events} = nova_test_log:collect(fun() -> nova_test_listener:boot(Options) end),
    ?assertMatch({error, {_Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app,
                                        {route_http_versions_not_served, "/where", [http2], web}}}}, _}}},
                 Booted),
    ?assertMatch([#{app := nova_test_sub_app}],
                 [Report || Report = #{msg := <<"An application that was added", _/binary>>}
                                <- nova_test_log:errors(Events)]),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)).

%% A library adds itself, and the listener it names lists it as well.
early_add_configured(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port, applications => [nova_test_app, nova_test_sub_app]}},
                host => [#{routes => []}],
                mounted => [#{routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    Served = {ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
    ?assertEqual(Served, nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual(Served, nova_sup:add_application(nova_test_sub_app, #{port => Port})),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    served_once(Port).

%% It is answered the same once the listeners are up.
add_configured(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port,
                                                                        applications => [nova_test_app,
                                                                                         nova_test_sub_app]}},
                                                host => [#{routes => []}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Served = {ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
    ?assertEqual(Served, nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual(Served, nova_sup:add_application(nova_test_sub_app, #{port => Port})),
    served_once(Port).

%% The application is the configuration's and not one that was added: it
%% goes when the configuration no longer lists it. One that was added is
%% refused the second time.
served_once(Port) ->
    Server = nova_test_listener:server(#{}),
    ?assertEqual([nova_test_app, nova_test_sub_app], [App || #{app := App} <- nova_sup:get_started_applications()]),
    ok = application:stop(Server),
    application:set_env(nova, listeners, #{web => #{port => Port, applications => [nova_test_app]}}),
    {ok, _} = application:ensure_all_started(Server),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/where")),
    Added = {ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
    ?assertEqual(Added, nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({error, {already_started, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")).

%% The listeners go with the server package. What was added to them is
%% kept with nova, and is added again.
server_restart(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _Host, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    ok = application:stop(Server),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ok = application:stop(Server),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)).

%% A route group that names a listener nothing has is a warning when its
%% application is added, and again when the server package comes back.
restart_unserved_group(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/admin", listener => amdin,
                                                              routes => [?WHERE]},
                                                            #{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _Host, Port} = nova_sup:add_application(nova_test_sub_app, #{listener => web}),
    ok = application:stop(Server),
    {{ok, _}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(Server) end),
    ?assertMatch([#{app := nova_test_sub_app, prefix := "/admin", listener := amdin}],
                 [Report || {warning, {report, Report = #{listener := _, prefix := _}}} <- Events]),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/admin/where")).

%% A listener that was started for an application is still that of the
%% others on it when the application is removed, and comes back as it was.
kept_listener(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Named = #{listener => nova_test_sub_app},
    {ok, nova_test_sub_app, Host, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, Host, Other} = nova_sup:add_application(nova_test_nested_app, Named),
    ok = nova_sup:remove_application(nova_test_sub_app),
    Kept = [#{app => nova_test_app, host => Host, port => Port, listener => web},
            #{app => nova_test_nested_app, host => Host, port => Other, listener => nova_test_sub_app}],
    ?assertEqual(Kept, lists:sort(nova_sup:get_started_applications())),
    Elsewhere = #{port => nova_test_listener:free_port()},
    ?assertEqual({error, {listener_exists, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, Elsewhere)),
    ok = application:stop(Server),
    ?assertEqual({error, {listener_exists, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, Elsewhere)),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual(Kept, lists:sort(nova_sup:get_started_applications())),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Other, "/sub/where")),
    ok = nova_sup:remove_application(nova_test_nested_app),
    ?assertEqual({error, {no_such_listener, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_nested_app, Named)),
    ok = application:stop(Server),
    ?assertEqual({error, {no_such_listener, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_nested_app, Named)),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual([web], nova_sup:listeners()),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)).

%% As it is for an application that was added to it by its address.
kept_by_bind(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, _, Other} = nova_sup:add_application(nova_test_nested_app, #{port => Other}),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ok = application:stop(Server),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

%% And it takes an application while the server package is stopped.
kept_while_stopped(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                deep => [#{prefix => "/deep", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Named = #{listener => nova_test_sub_app},
    {ok, nova_test_sub_app, Host, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, Host, Other} = nova_sup:add_application(nova_test_nested_app, Named),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ok = application:stop(Server),
    ?assertEqual({ok, nova_test_deep_app, Host, Other}, nova_sup:add_application(nova_test_deep_app, Named)),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/deep/where")).

%% Its name stays in use. A listener of the configuration that is given
%% it keeps the server package from starting, with the application that
%% was on the kept one.
kept_name_configured(_Config) ->
    name_configured(port).

%% As it does for an application that named the kept one.
kept_named_configured(_Config) ->
    name_configured(name).

name_configured(By) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Third = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Nested = case By of
                 port -> #{port => Other};
                 name -> #{listener => nova_test_sub_app}
             end,
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, _, Other} = nova_sup:add_application(nova_test_nested_app, Nested),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ok = application:stop(Server),
    application:set_env(nova, listeners, #{web => #{port => Port, applications => [nova_test_app]},
                                           nova_test_sub_app => #{port => Third, applications => [nova_test_app]}}),
    ?assertMatch({error, {Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_nested_app,
                                        {listener_exists, nova_test_sub_app}}}}, _}}},
                 application:ensure_all_started(Server)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Third, [], 1000)),
    ?assertEqual({error, not_found}, nova_sup:remove_application(nova_test_deep_app)).

%% An application that is added by its address while the listeners are
%% down goes on the listener that was started there, and that one comes
%% back as it was started: with its TLS, also when the application it was
%% started for is removed meanwhile.
early_on_kept(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Tls = nova_test_listener:tls(),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other, tls => Tls}),
    ok = application:stop(Server),
    ?assertEqual({ok, nova_test_nested_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_nested_app, #{port => Other})),
    ok = nova_sup:remove_application(nova_test_sub_app),
    {ok, _} = application:ensure_all_started(Server),
    ?assertMatch({200, _Headers, ~"nova_test_sub_app http1"}, nova_test_listener:https_get(Other, "/where")),
    ?assertEqual([#{app => nova_test_nested_app, host => {0, 0, 0, 0}, port => Other, listener => nova_test_sub_app}],
                 [Started || Started = #{port := On} <- nova_sup:get_started_applications(), On =:= Other]).

%% One that named it while they were down is served on it, also when it is
%% the only one left on it by the time they are back.
early_names_kept(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                deep => [#{prefix => "/deep", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Named = #{listener => nova_test_sub_app},
    {ok, nova_test_sub_app, Host, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, Host, Other} = nova_sup:add_application(nova_test_nested_app, Named),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ok = application:stop(Server),
    ?assertEqual({ok, nova_test_deep_app, Host, Other}, nova_sup:add_application(nova_test_deep_app, Named)),
    ok = nova_sup:remove_application(nova_test_nested_app),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/deep/where")),
    ok = application:stop(Server),
    ok = nova_sup:remove_application(nova_test_deep_app),
    ?assertEqual({error, {no_such_listener, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_nested_app, Named)).

%% Removing an application that was added before the listeners are up
%% keeps the listener another one named, as it does while they run.
early_remove_named(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{routes => [?WHERE]}],
                nested => [#{routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, _, Other} =
        nova_sup:add_application(nova_test_nested_app, #{listener => nova_test_sub_app}),
    ok = nova_sup:remove_application(nova_test_sub_app),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

%% An application that was added to a listener by its address is not served
%% by another that has the address by then, which the configuration made
%% for other applications and without TLS.
readdressed_listener(_Config) ->
    Port = nova_test_listener:free_port(),
    Moved = nova_test_listener:free_port(),
    Tls = nova_test_listener:tls(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port, tls => Tls}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _, Port} = nova_sup:add_application(nova_test_sub_app, #{port => Port}),
    ok = application:stop(Server),
    application:set_env(nova, listeners, #{web => #{port => Moved, tls => Tls, applications => [nova_test_app]},
                                           metrics => #{port => Port, applications => [nova_test_app]}}),
    {NotStarted, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(Server) end),
    ?assertMatch({error, {Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app, {no_such_listener, web}}}}, _}}},
                 NotStarted),
    ?assertEqual([{nova_test_sub_app, web, {{0, 0, 0, 0}, Port}, {{0, 0, 0, 0}, Moved}}],
                 [{App, Name, Was, Now}
                  || {notice, {report, #{app := App, listener := Name, added_at := Was, listener_at := Now}}}
                         <- Events]),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)),
    ok = nova_sup:remove_application(nova_test_sub_app),
    {ok, _} = application:ensure_all_started(Server),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")).

%% Nor is one that had a listener of its own put on the listener of the
%% configuration that has its address by then.
address_taken(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Tls = nova_test_listener:tls(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other, tls => Tls}),
    ok = application:stop(Server),
    application:set_env(nova, listeners, #{web => #{port => Port, applications => [nova_test_app]},
                                           metrics => #{port => Other, applications => [nova_test_app]}}),
    ?assertMatch({error, {Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app, {address_in_use, metrics}}}}, _}}},
                 application:ensure_all_started(Server)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)).

%% A listener that is there serves as it was started. An application that
%% comes with settings it would not apply is not put on it, before the
%% listeners are up and after, by its address or by its name. The settings
%% a listener was started with for an application are those it has.
settings_ignored(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                nested => [#{prefix => "/nested", routes => [?WHERE]}],
                nova_apps => []},
    Tls = nova_test_listener:tls(),
    Ignored = fun(Name, Keys) -> {error, {listener_settings_ignored, Name, Keys}} end,
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    ?assertEqual(Ignored(web, [tls]), nova_sup:add_application(nova_test_sub_app, #{port => Port, tls => Tls})),
    ?assertEqual(Ignored(web, [steps]),
                 nova_sup:add_application(nova_test_sub_app, #{listener => web, steps => [nova_router]})),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual(Ignored(web, [protocols, tls]),
                 nova_sup:add_application(nova_test_sub_app, #{port => Port, tls => Tls, protocols => [http1]})),
    ?assertEqual(Ignored(web, [use_ssl]), nova_sup:add_application(nova_test_sub_app, #{port => Port, use_ssl => false})),
    ?assertEqual(Ignored(web, [port]), nova_sup:add_application(nova_test_sub_app, #{listener => web, port => Other})),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")),
    ?assertEqual({error, {invalid_listener_setting, nova_test_sub_app, port, 0}},
                 nova_sup:add_application(nova_test_sub_app, #{port => 0})),
    {ok, nova_test_sub_app, Host, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other, tls => Tls}),
    ?assertEqual(Ignored(nova_test_sub_app, [protocols]),
                 nova_sup:add_application(nova_test_nested_app, #{port => Other, protocols => [http1]})),
    ?assertEqual(Ignored(nova_test_sub_app, [tls]),
                 nova_sup:add_application(nova_test_nested_app, #{port => Other, tls => [{verify, verify_none} | Tls]})),
    ?assertEqual({ok, nova_test_nested_app, Host, Other},
                 nova_sup:add_application(nova_test_nested_app, #{port => Other, tls => Tls})),
    ?assertMatch({200, _Headers, ~"nova_test_sub_app http1"},
                 nova_test_listener:https_get(Other, "/nested/where")),
    ?assertEqual([nova_test_app], [App || #{app := App, listener := web} <- nova_sup:get_started_applications()]).

%% The routes of an application are not added to a listener that is not
%% the one it was to be added to by the time its router has run.
listener_replaced(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 true = ets:delete(nova_listeners, {{0, 0, 0, 0}, Port}),
                                 meck:passthrough([Env])
                         end),
        ?assertEqual({error, {no_such_listener, web}},
                     nova_sup:add_application(nova_test_sub_app, #{listener => web})),
        ?assertEqual([], nova_sup:listeners())
    after
        meck:unload(nova_test_mounted_router),
        Server:stop_listener(web)
    end.

%% A listener whose routing table is replaced while the router runs is not
%% the one the application was to be added to, and the new table does not
%% list it.
%% Routes that are added to a listener as the router of an application
%% that is added to it runs are no reason not to add the application. It
%% is compiled again into the table as it is by then, and both are served.
%% Not for ever: a table that changes each time is given up on.
routes_changed(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nested => [#{prefix => "/nested", routes => [?WHERE]}],
                                                nova_apps => []}),
    [Key] = nova_router:dispatch_keys(),
    Late = fun(Prefix) ->
                   elsewhere(fun() -> nova_router:add_routes(nova_test_app, #{prefix => Prefix, routes => [?WHERE]}, Key) end)
           end,
    ok = meck:new([nova_test_mounted_router, nova_test_nested_router], [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 case put(?FUNCTION_NAME, ran) of
                                     undefined -> ok = Late("/late");
                                     ran -> ok
                                 end,
                                 meck:passthrough([Env])
                         end),
        ok = meck:expect(nova_test_nested_router, routes,
                         fun(Env) ->
                                 ok = Late("/later"),
                                 meck:passthrough([Env])
                         end),
        ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                     nova_sup:add_application(nova_test_sub_app, #{listener => web})),
        ?assertEqual(2, meck:num_calls(nova_test_mounted_router, routes, '_')),
        ?assertEqual({error, {routing_table_changed, web}},
                     nova_sup:add_application(nova_test_nested_app, #{listener => web})),
        ?assertEqual(3, meck:num_calls(nova_test_nested_router, routes, '_'))
    after
        meck:unload([nova_test_mounted_router, nova_test_nested_router])
    end,
    ?assertEqual([{200, ~"web http1"}, {200, ~"web http1"}, {200, ~"web http1"}],
                 [nova_test_listener:get(Port, Path) || Path <- ["/sub/where", "/late/where", "/later/where"]]),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/nested/where")),
    ?assertEqual([nova_test_app, nova_test_sub_app],
                 lists:sort([App || #{app := App} <- nova_sup:get_started_applications()])).

%% A router does not store into the table it is compiled into, so what is
%% stored as it runs is stored by another process.
elsewhere(Store) ->
    {Storing, Monitor} = spawn_monitor(fun() -> exit({stored, Store()}) end),
    receive
        {'DOWN', Monitor, process, Storing, {stored, Stored}} -> Stored
    after 5000 -> error(not_stored)
    end.

%% A router that adds routes to the listener its application is added to
%% would leave them served while the application is not added. It is
%% thrown at in the caller, as for its other mistakes, and nothing of it
%% is served.
own_table(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    [Key] = nova_router:dispatch_keys(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 ok = nova_router:add_routes(nova_test_app, #{prefix => "/side", routes => [?WHERE]}, Key),
                                 meck:passthrough([Env])
                         end),
        ?assertThrow({error, {routing_table_in_use, Key}},
                     nova_sup:add_application(nova_test_sub_app, #{listener => web})),
        ?assertEqual(1, meck:num_calls(nova_test_mounted_router, routes, '_'))
    after
        meck:unload(nova_test_mounted_router)
    end,
    ?assertMatch([{404, _}, {404, _}],
                 [nova_test_listener:get(Port, Path) || Path <- ["/side/where", "/sub/where"]]),
    ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()]),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")).

%% Only code outside Nova builds the table of a listener that is up. An
%% application is not taken out of the table before that one, which would
%% leave it served and said to be removed: it stays added until it is.
removed_while_built(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    {ok, nova_test_sub_app, _Host, Port} = nova_sup:add_application(nova_test_sub_app, #{listener => web}),
    [Key] = nova_router:dispatch_keys(),
    Self = self(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 Self ! building,
                                 receive go -> meck:passthrough([Env]) after 5000 -> error(not_let_go) end
                         end),
        {Builder, Monitor} = spawn_monitor(fun() -> nova_router:compile_fresh([nova_test_sub_app], Key) end),
        receive building -> ok after 5000 -> error(not_building) end,
        ?assertEqual({error, {routing_table_replaced, web}}, nova_sup:remove_application(nova_test_sub_app)),
        ?assertEqual([nova_test_app, nova_test_sub_app],
                     lists:sort([App || #{app := App} <- nova_sup:get_started_applications()])),
        Builder ! go,
        receive {'DOWN', Monitor, process, Builder, normal} -> ok after 5000 -> error(not_built) end
    after
        meck:unload(nova_test_mounted_router)
    end,
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")),
    ?assertEqual(ok, nova_sup:remove_application(nova_test_sub_app)),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")),
    ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()]).

%% The router of an application can trap exits. The process it runs in
%% then outlives the one that keeps the added applications, whose call was
%% answered that the application was not added. What it goes on to add is
%% kept by nothing, so it is not added: not to a listener that is there,
%% nor on one of its own.
outlived_on_listener(_Config) ->
    {Port, _Other} = outlived(#{listener => web}),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")).

outlived_on_port(_Config) ->
    {_Port, Other} = outlived(port),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/sub/where")).

outlived(Where) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Settings = case Where of
                   port -> #{port => Other};
                   _Listener -> Where
               end,
    Self = self(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 process_flag(trap_exit, true),
                                 Self ! {compiling, self()},
                                 receive go -> meck:passthrough([Env]) after 5000 -> error(not_let_go) end
                         end),
        {Adder, Adding} = spawn_monitor(fun() -> nova_sup:add_application(nova_test_sub_app, Settings) end),
        Worker = receive {compiling, Compiling} -> Compiling after 5000 -> error(not_compiling) end,
        Working = monitor(process, Worker),
        Registry = registry(),
        exit(Registry, kill),
        ok = registry_after(Registry),
        receive {'DOWN', Adding, process, Adder, _Reason} -> ok after 5000 -> error(still_adding) end,
        _ = sys:get_state(nova_applications, 5000),
        Worker ! go,
        receive {'DOWN', Working, process, Worker, _Why} -> ok after 5000 -> error(still_working) end
    after
        meck:unload(nova_test_mounted_router)
    end,
    ?assertEqual([web], nova_sup:listeners()),
    ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()]),
    {Port, Other}.

table_replaced(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    [Key] = nova_router:dispatch_keys(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 _ = nova_router:compile_fresh([nova_test_app], Key),
                                 meck:passthrough([Env])
                         end),
        ?assertEqual({error, {no_such_listener, web}},
                     nova_sup:add_application(nova_test_sub_app, #{listener => web}))
    after
        meck:unload(nova_test_mounted_router)
    end,
    ?assertEqual([nova_test_app], [App || {App, _Prefix} <- nova_router:compiled_apps(Key)]),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")).

%% What is added goes with the process that keeps it: an application that
%% is being added as that process goes down is not added behind its back.
registry_dies_adding(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Self = self(),
    Registry = registry(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(_Env) ->
                                 Self ! {compiling, self()},
                                 receive after infinity -> ok end
                         end),
        {Adder, Adding} = spawn_monitor(fun() -> nova_sup:add_application(nova_test_sub_app, #{listener => web}) end),
        Compiling = receive {compiling, Pid} when is_pid(Pid) -> Pid after 5000 -> error(not_compiling) end,
        Monitor = monitor(process, Compiling),
        exit(Registry, kill),
        ?assertEqual(killed, receive {'DOWN', Monitor, process, Compiling, Why} -> Why after 5000 -> alive end),
        ?assertMatch({killed, _Call}, receive {'DOWN', Adding, process, Adder, Reason} -> Reason after 5000 -> none end)
    after
        meck:unload(nova_test_mounted_router)
    end,
    ok = registry_after(Registry),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})).

%% The process that keeps the added applications can be started again
%% while the listeners are up. It says so, what was added is still served,
%% and applications are added and removed as before. How a listener was
%% started for one went with the process: the server package does not
%% start again before that application is removed.
registry_restart(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{prefix => "/nested", routes => [?WHERE]}],
                                                deep => [#{prefix => "/deep", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, _, Port} = nova_sup:add_application(nova_test_nested_app, #{listener => web}),
    {ok, nova_test_deep_app, _, Port} = nova_sup:add_application(nova_test_deep_app, #{listener => web}),
    ok = nova_sup:remove_application(nova_test_deep_app),
    Registry = registry(),
    {Unknown, Events} = nova_test_log:collect(
                          fun() ->
                                  exit(Registry, kill),
                                  ok = registry_after(Registry),
                                  gen_server:call(nova_applications, status)
                          end),
    ?assertEqual({error, unknown_request}, Unknown),
    ?assertEqual([{[nova_test_nested_app, nova_test_sub_app], [nova_test_sub_app]}],
                 [{lists:sort(Apps), Listeners}
                  || #{msg := <<"The process that keeps the applications", _/binary>>,
                       applications := Apps, listeners := Listeners} <- nova_test_log:errors(Events)]),
    ?assertEqual({ok, nova_test_deep_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_deep_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/deep/where")),
    ok = nova_sup:remove_application(nova_test_nested_app),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/nested/where")),
    ok = application:stop(Server),
    ?assertMatch({error, {Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app,
                                        {no_such_listener, nova_test_sub_app}}}}, _}}},
                 application:ensure_all_started(Server)),
    ok = nova_sup:remove_application(nova_test_sub_app),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/deep/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/nested/where")).

%% What was added is what the listeners say was, not what the configuration
%% does not list by then: an application it listed is not kept as added.
%% A listener of the configuration that has the name and the address of
%% one that was started for an application is not taken for it.
restart_keeps_added(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port,
                                                                        applications => [nova_test_app,
                                                                                         nova_test_nested_app]}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nested => [#{prefix => "/nested", routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Tls = nova_test_listener:tls(),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other, tls => Tls}),
    Web = #{port => Port, applications => [nova_test_app]},
    application:set_env(nova, listeners, #{web => Web}),
    Registry = registry(),
    exit(Registry, kill),
    ok = registry_after(Registry),
    ok = application:stop(Server),
    application:set_env(nova, listeners, #{web => Web,
                                           nova_test_sub_app => #{port => Other, applications => [nova_test_app]}}),
    ?assertMatch({error, {Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app,
                                        {no_such_listener, nova_test_sub_app}}}}, _}}},
                 application:ensure_all_started(Server)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ?assertEqual({error, not_found}, nova_sup:remove_application(nova_test_nested_app)),
    application:set_env(nova, listeners, #{web => Web}),
    {ok, _} = application:ensure_all_started(Server),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/nested/where")).

%% A listener is listed before the server binds it, and says that it is
%% bound once the server answered. The process that keeps the added
%% applications can be stopped in between, and the worker that started the
%% listener with it. No call that added the application was answered then,
%% so the one that takes its place stops the listener: the application is
%% not added, can be added again, and the server package starts again.
killed_while_binding(_Config) ->
    killed(fun(Listener, Env, Bound) ->
                   Started = meck:passthrough([Listener, Env]),
                   Bound(),
                   receive after infinity -> Started end
           end).

killed_before_binding(_Config) ->
    killed(fun(_Listener, _Env, Bound) ->
                   Bound(),
                   receive after infinity -> ok end
           end).

killed(Start) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Self = self(),
    ok = meck:new(Server, [passthrough, no_link]),
    try
        ok = meck:expect(Server, start_listener,
                         fun(Listener = #{name := nova_test_sub_app}, Env) ->
                                 Start(Listener, Env, fun() -> Self ! binding end);
                            (Listener, Env) ->
                                 meck:passthrough([Listener, Env])
                         end),
        {Adder, Monitor} = spawn_monitor(fun() -> nova_sup:add_application(nova_test_sub_app, #{port => Other}) end),
        receive binding -> ok after 5000 -> error(not_binding) end,
        Registry = registry(),
        exit(Registry, kill),
        ok = registry_after(Registry),
        receive {'DOWN', Monitor, process, Adder, _Reason} -> ok after 5000 -> error(still_adding) end,
        _ = sys:get_state(nova_applications, 5000),
        ?assertEqual([web], nova_sup:listeners()),
        ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()]),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000))
    after
        meck:unload(Server)
    end,
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")),
    ok = application:stop(Server),
    ?assertMatch({ok, _}, application:ensure_all_started(Server)),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

registry() ->
    case whereis(nova_applications) of
        Registry when is_pid(Registry) -> Registry
    end.

registry_after(Was) ->
    case whereis(nova_applications) of
        Registry when is_pid(Registry), Registry =/= Was ->
            ok;
        _Not ->
            timer:sleep(10),
            registry_after(Was)
    end.

%% When that process is started again while no listeners are up, nothing
%% says what was added. It says so, and the applications are not served.
registry_restart_stopped(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    ok = application:stop(Server),
    Registry = registry(),
    {ok, Events} = nova_test_log:collect(fun() ->
                                                 exit(Registry, kill),
                                                 registry_after(Registry)
                                         end),
    ?assertMatch([_Lost],
                 [Report || Report = #{msg := <<"The process that keeps the applications", _/binary>>}
                                <- nova_test_log:errors(Events)]),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    ?assertEqual({error, not_found}, nova_sup:remove_application(nova_test_sub_app)).

%% The first start of the process says nothing.
registry_first_start(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {{ok, _}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(nova) end),
    ?assertEqual([], nova_test_log:errors(Events)),
    ok = application:stop(nova),
    {{ok, _}, Again} = nova_test_log:collect(fun() -> application:ensure_all_started(nova) end),
    ?assertEqual([], nova_test_log:errors(Again)).

%% A start that fails leaves nothing that makes the next one say it was
%% started again.
registry_after_failed_start(_Config) ->
    Port = nova_test_listener:free_port(),
    ok = nova_test_listener:configure(#{port => Port,
                                        listeners => #{web => #{port => Port}},
                                        host => [#{routes => [?WHERE]}],
                                        nova_apps => []}),
    Squat = spawn(fun() -> receive stop -> ok end end),
    true = register(nova_watcher, Squat),
    try
        ?assertMatch({error, _}, application:ensure_all_started(nova))
    after
        Gone = monitor(process, Squat),
        Squat ! stop,
        receive {'DOWN', Gone, process, Squat, _Reason} -> ok end
    end,
    {{ok, _}, Events} = nova_test_log:collect(fun() -> application:ensure_all_started(nova) end),
    ?assertEqual([], nova_test_log:errors(Events)).

%% The listeners can be gone before the process that keeps the
%% applications is told. An application is then kept for the next start,
%% and no listener is started for it.
listeners_gone(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    true = ets:delete(nova_listeners),
    try
        ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                     nova_sup:add_application(nova_test_sub_app, #{port => Other})),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000))
    after
        Server:stop_listener(web)
    end.

%% When they go as a listener is started for an application, that one is
%% stopped with them.
gone_while_starting(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    ok = meck:new(Server, [passthrough, no_link]),
    try
        ok = meck:expect(Server, start_listener,
                         fun(Listener, Env) ->
                                 Started = meck:passthrough([Listener, Env]),
                                 true = ets:delete(nova_listeners),
                                 Started
                         end),
        ?assertEqual({error, {listener_not_started, listeners_stopped}},
                     nova_sup:add_application(nova_test_sub_app, #{port => Other})),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
        ?assertEqual(undefined, persistent_term:get({nova_dispatch, nova_test_sub_app}, undefined))
    after
        meck:unload(Server),
        Server:stop_listener(web)
    end.

%% Once the owner of the listeners is gone, an application is kept for the
%% next start.
owner_gone(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Sup = case Server of
              nova_cowboy -> nova_cowboy_sup;
              nova_roadrunner -> nova_roadrunner_sup
          end,
    Owner = case supervisor:which_children(Sup) of
                [{nova_listener, Pid, _Type, _Modules}] when is_pid(Pid) -> Pid
            end,
    Monitor = monitor(process, Owner),
    exit(Owner, kill),
    receive {'DOWN', Monitor, process, Owner, killed} -> ok end,
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    _ = application:stop(Server),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

%% What a router raises as its application is added is raised in the
%% caller, and the application can be added once its routes compile.
runtime_raise(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes, fun(_Env) -> error(no_routes) end),
        ?assertError(no_routes, nova_sup:add_application(nova_test_sub_app, #{listener => web}))
    after
        meck:unload(nova_test_mounted_router)
    end,
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")).

%% The routes of an added application are compiled in a process of their
%% own. What a router links to takes that one down, and not the process
%% that keeps the applications: the caller is told, and the next is added.
router_dies(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Registry = registry(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(_Env) ->
                                 _ = spawn_link(fun() -> exit({router_gone, [{password, "hunter2"}]}) end),
                                 receive after infinity -> ok end
                         end),
        ?assertExit({router_gone, [{password, redacted}]},
                    nova_sup:add_application(nova_test_sub_app, #{listener => web}))
    after
        meck:unload(nova_test_mounted_router)
    end,
    ?assertEqual(Registry, whereis(nova_applications)),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")).

%% A router that does not return keeps its application from being added,
%% and not the server package from stopping, or from starting again.
stuck_add(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Self = self(),
    application:set_env(nova, shutdown_drain_timeout, 200),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(_Env) ->
                                 Self ! {compiling, self()},
                                 receive after infinity -> ok end
                         end),
        {Adder, Monitor} = spawn_monitor(fun() -> nova_sup:add_application(nova_test_sub_app, #{listener => web}) end),
        Compiling = receive {compiling, Pid} when is_pid(Pid) -> Pid after 5000 -> error(not_compiling) end,
        {Stopped, Events} = nova_test_log:collect(fun() -> application:stop(Server) end),
        ?assertEqual(ok, Stopped),
        ?assertMatch([#{app := nova_test_sub_app, timeout_ms := 200}],
                     [Report || {warning, {report, Report = #{msg := <<"An application was still being added", _/binary>>}}}
                                    <- Events]),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)),
        ?assertEqual(killed, receive {'DOWN', Monitor, process, Adder, Reason} -> Reason after 5000 -> none end),
        ?assertNot(is_process_alive(Compiling)),
        {ok, _} = application:ensure_all_started(Server),
        ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
        ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()])
    after
        application:unset_env(nova, shutdown_drain_timeout),
        meck:unload(nova_test_mounted_router)
    end.

%% An application that waits behind one whose router does not return is
%% not added to listeners that stop, and its router is not waited for. It
%% is kept for the next start.
queued_add(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nested => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Registry = registry(),
    Self = self(),
    application:set_env(nova, shutdown_drain_timeout, 200),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(_Env) ->
                                 Self ! compiling,
                                 receive after infinity -> ok end
                         end),
        {Adder, Monitor} = spawn_monitor(fun() -> nova_sup:add_application(nova_test_sub_app, #{listener => web}) end),
        receive compiling -> ok after 5000 -> error(not_compiling) end,
        _ = spawn(fun() -> Self ! {queued, nova_sup:add_application(nova_test_nested_app, #{port => Other})} end),
        ok = waiting(Registry, 500),
        {ok, Events} = nova_test_log:collect(fun() -> application:stop(Server) end),
        ?assertEqual(killed, receive {'DOWN', Monitor, process, Adder, Reason} -> Reason after 5000 -> none end),
        ?assertEqual({ok, nova_test_nested_app, {0, 0, 0, 0}, Other},
                     receive {queued, Answer} -> Answer after 5000 -> none end),
        ?assertMatch([#{app := nova_test_sub_app}],
                     [Report || {warning, {report, Report = #{msg := <<"An application was still being added", _/binary>>}}}
                                    <- Events]),
        ?assertEqual([], [Report || {notice, {report, Report = #{msg := <<"Nova is running">>}}} <- Events]),
        {ok, _} = application:ensure_all_started(Server),
        ?assertEqual({200, ~"nova_test_nested_app http1"}, nova_test_listener:get(Other, "/where"))
    after
        application:unset_env(nova, shutdown_drain_timeout),
        meck:unload(nova_test_mounted_router)
    end.

%% Any process of the node can send to the one that keeps the added
%% applications. A timeout that is none sets no timer there, and the
%% application that is being added is added.
forged_drain(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nested => [#{prefix => "/nested", routes => [?WHERE]}],
                                                nova_apps => []}),
    Registry = registry(),
    Self = self(),
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 Self ! {compiling, self()},
                                 receive go -> meck:passthrough([Env]) after 5000 -> error(not_let_go) end
                         end),
        _ = spawn(fun() -> Self ! {added, catch nova_sup:add_application(nova_test_sub_app, #{listener => web})} end),
        Worker = receive {compiling, Compiling} -> Compiling after 5000 -> error(not_compiling) end,
        Registry ! {draining, alias(), -1},
        Worker ! go,
        ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                     receive {added, Added} -> Added after 5000 -> none end),
        ?assertEqual(Registry, registry())
    after
        meck:unload(nova_test_mounted_router)
    end,
    %% Nor is what no process that stops the listeners sends taken for it:
    %% the listeners are up, and an application is added to them.
    _ = [Registry ! {draining, Forged, 0} || Forged <- [no_such_name, self(), {alias(), drained}]],
    _ = sys:get_state(Registry, 5000),
    ?assertEqual(Registry, registry()),
    ?assertEqual({ok, nova_test_nested_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_nested_app, #{listener => web})),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/nested/where")).

waiting(_Registry, 0) ->
    error(nothing_waits);
waiting(Registry, Tries) ->
    case process_info(Registry, message_queue_len) of
        {message_queue_len, Waiting} when Waiting > 0 ->
            ok;
        _None ->
            timer:sleep(10),
            waiting(Registry, Tries - 1)
    end.

%% A listener that the server has bound for an application, as the one
%% that adds it is stopped with the listeners, is stopped with them.
stopped_while_binding(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    Self = self(),
    application:set_env(nova, shutdown_drain_timeout, 200),
    ok = meck:new(Server, [passthrough, no_link]),
    try
        ok = meck:expect(Server, start_listener,
                         fun(Listener = #{name := nova_test_sub_app}, Env) ->
                                 Started = meck:passthrough([Listener, Env]),
                                 Self ! bound,
                                 receive after infinity -> Started end;
                            (Listener, Env) ->
                                 meck:passthrough([Listener, Env])
                         end),
        {Adder, Monitor} = spawn_monitor(fun() -> nova_sup:add_application(nova_test_sub_app, #{port => Other}) end),
        receive bound -> ok after 5000 -> error(not_bound) end,
        ?assertEqual(ok, application:stop(Server)),
        ?assertEqual(killed, receive {'DOWN', Monitor, process, Adder, Reason} -> Reason after 5000 -> none end),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
        ?assertEqual(undefined, persistent_term:get({nova_dispatch, nova_test_sub_app}, undefined)),
        {ok, _} = application:ensure_all_started(Server),
        ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()]),
        ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000))
    after
        application:unset_env(nova, shutdown_drain_timeout),
        meck:unload(Server)
    end.

%% Settings that are not those of a listener are refused before the
%% listeners are up as they are after, and so is an application while the
%% configuration has no listeners that can be read.
invalid_settings(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{routes => [?WHERE]}],
                nova_apps => []},
    Unknown = {error, {unknown_listener_settings, nova_test_sub_app, [prot]}},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, Listeners} = application:get_env(nova, listeners),
    ?assertEqual(Unknown, nova_sup:add_application(nova_test_sub_app, #{prot => 1})),
    application:set_env(nova, listeners, #{}),
    ?assertEqual({error, {invalid_listeners, #{}}}, nova_sup:add_application(nova_test_sub_app, #{port => Port})),
    application:set_env(nova, listeners, [{web, [{port, Port}, {tls, [{password, "hunter2"}]}]}]),
    ?assertEqual({error, {invalid_listeners, [{web, [{port, Port}, {tls, [{password, redacted}]}]}]}},
                 nova_sup:add_application(nova_test_sub_app, #{port => Port})),
    application:set_env(nova, listeners, Listeners),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual(Unknown, nova_sup:add_application(nova_test_sub_app, #{prot => 1})),
    ?assertEqual([nova_test_app], [App || #{app := App} <- nova_sup:get_started_applications()]).

%% An application that was added to a listener of the configuration by its
%% address is served by that listener. When the listener is moved, no
%% other is started on the address with the settings of the application.
moved_listener(_Config) ->
    Port = nova_test_listener:free_port(),
    Moved = nova_test_listener:free_port(),
    Tls = nova_test_listener:tls(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port, tls => Tls}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    {ok, nova_test_sub_app, _, Port} = nova_sup:add_application(nova_test_sub_app, #{port => Port}),
    ok = application:stop(Server),
    application:set_env(nova, listeners, #{web => #{port => Moved, tls => Tls, applications => [nova_test_app]}}),
    ?assertMatch({error, {Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app, {no_such_listener, web}}}}, _}}},
                 application:ensure_all_started(Server)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Moved, [], 1000)).

%% An application that is added while the listeners are starting is added
%% once they are up. A router that adds one as its own routes are compiled
%% is told that it cannot.
added_during_start(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                nested => [#{prefix => "/nested", routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_sub_app, _, Port} = nova_sup:add_application(nova_test_sub_app, #{listener => web}),
    Self = self(),
    Add = fun() -> nova_sup:add_application(nova_test_nested_app, #{listener => web}) end,
    ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
    try
        ok = meck:expect(nova_test_mounted_router, routes,
                         fun(Env) ->
                                 Self ! {from_router, Add()},
                                 _ = spawn(fun() -> Self ! {from_another, Add()} end),
                                 meck:passthrough([Env])
                         end),
        {ok, Port} = nova_test_listener:boot(Options),
        ?assertEqual({error, adding_an_application}, receive {from_router, Told} -> Told after 5000 -> none end),
        ?assertEqual({ok, nova_test_nested_app, {0, 0, 0, 0}, Port},
                     receive {from_another, Added} -> Added after 5000 -> none end),
        ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/nested/where"))
    after
        meck:unload(nova_test_mounted_router)
    end.

%% A route group of the configuration is served when the listener it names
%% has its application once those that were added are.
early_serves_group(_Config) ->
    Port = nova_test_listener:free_port(),
    Side = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port, applications => [nova_test_app]},
                               side => #{port => Side, applications => [nova_test_nested_app]}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{prefix => "/admin", listener => side, routes => [?WHERE]}],
                nested => [#{routes => [?WHERE]}],
                nova_apps => [{nova_test_sub_app, #{}}]},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_sub_app, _, Side} = nova_sup:add_application(nova_test_sub_app, #{listener => side}),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"side http1"}, nova_test_listener:get(Side, "/admin/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/admin/where")).

%% What a server says of a listener it does not take is logged, and
%% returned, without the secrets of its TLS options: for a listener of the
%% configuration and for one of an application that was added.
refused_listener(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Tls = #{certfile => "/nova/no/such/cert.pem", keyfile => "/nova/no/such/key.pem", password => "hunter2"},
    Server = nova_test_refusing_server,
    {module, Server} = code:ensure_loaded(Server),
    Options = fun(Web) ->
                      #{server => Server, port => Port, listeners => #{web => Web#{port => Port}},
                        host => [#{routes => [?WHERE]}], mounted => [#{routes => [?WHERE]}], nova_apps => []}
              end,
    Refused = fun(Start) ->
                      {Result, Events} = nova_test_log:collect(Start),
                      ?assertEqual(nomatch, string:find(lists:flatten(io_lib:format("~0p", [{Result, Events}])),
                                                        "hunter2")),
                      {Result, [Msg || #{msg := Msg} <- nova_test_log:errors(Events)]}
              end,
    Start = fun() ->
                    try
                        nova_sup:start_listeners(Server)
                    after
                        ets:delete(nova_listeners)
                    end
            end,
    ok = nova_test_listener:configure(Options(#{tls => Tls})),
    {ok, _} = application:ensure_all_started(nova),
    ?assertMatch({{error, {refused, #{name := web}}}, [<<"Listener asks for", _/binary>>]}, Refused(Start)),
    ok = application:stop(nova),
    ok = nova_test_listener:configure(Options(#{})),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other, tls => Tls}),
    ?assertMatch({{error, {application_not_added, nova_test_sub_app, {refused, #{name := nova_test_sub_app}}}},
                  [<<"Listener asks for", _/binary>>, <<"An application that was added", _/binary>>]},
                 Refused(Start)).

%% An application that is added before the listeners are up can name the
%% listener that one added before it is going to start.
early_add_named(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertEqual({ok, nova_test_nested_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_nested_app, #{listener => nova_test_sub_app})),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")),
    ?assertEqual([nova_test_nested_app, nova_test_sub_app],
                 lists:sort([App || #{app := App, port := On} <- nova_sup:get_started_applications(), On =:= Other])).

%% The answer before the listeners are up is the one they give: a listener
%% that would be started for the application cannot have a name that is
%% taken.
early_listener_exists(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{nova_test_sub_app => #{port => Port, applications => [nova_test_app]}},
                host => [#{routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    Other = nova_test_listener:free_port(),
    ?assertEqual({error, {listener_exists, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Port},
                 nova_sup:add_application(nova_test_sub_app, #{port => Port})),
    ?assertEqual({error, {listener_exists, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({error, {listener_exists, nova_test_sub_app}},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})).

%% An application that was added before the listeners were up is checked
%% as the configuration is: a route group of its that no listener serves
%% stops the node.
early_unserved_group(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{prefix => "/admin", listener => amdin, routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_sub_app, _Host, Port} = nova_sup:add_application(nova_test_sub_app, #{listener => web}),
    ?assertMatch({error, {_Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {application_not_added, nova_test_sub_app,
                                        {route_group_not_served, nova_test_sub_app, "/admin", amdin}}}}, _}}},
                 nova_test_listener:boot(Options)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Port, [], 1000)).

%% A route group of an early application may name the listener that one
%% added after it starts.
early_group_served(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    Options = #{port => Port,
                listeners => #{web => #{port => Port}},
                host => [#{routes => [?WHERE]}],
                mounted => [#{routes => [?WHERE]}],
                nested => [#{prefix => "/open", routes => [?WHERE]},
                           #{prefix => "/admin", listener => nova_test_sub_app, routes => [?WHERE]}],
                nova_apps => []},
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, nova_test_nested_app, _, Port} = nova_sup:add_application(nova_test_nested_app, #{listener => web}),
    {ok, nova_test_sub_app, _, Other} = nova_sup:add_application(nova_test_sub_app, #{port => Other}),
    {ok, nova_test_nested_app, _, Other} = nova_sup:add_application(nova_test_nested_app,
                                                                    #{listener => nova_test_sub_app}),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/open/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/admin/where")),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/admin/where")).

%% Listeners that are draining take no more applications. One that is
%% added then is added when they are started again.
add_while_draining(_Config) ->
    Port = nova_test_listener:free_port(),
    Other = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{routes => [?WHERE]}],
                                                nova_apps => []}),
    Server = nova_test_listener:server(#{}),
    ok = nova_listener_owner:shutdown(Server),
    ?assertEqual({ok, nova_test_sub_app, {0, 0, 0, 0}, Other},
                 nova_sup:add_application(nova_test_sub_app, #{port => Other})),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Other, [], 1000)),
    ok = application:stop(Server),
    {ok, _} = application:ensure_all_started(Server),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

%% Applications are added one at a time, whoever asks.
concurrent_adds(_Config) ->
    Port = nova_test_listener:free_port(),
    {ok, Port} = nova_test_listener:try_start(#{port => Port,
                                                listeners => #{web => #{port => Port}},
                                                host => [#{routes => [?WHERE]}],
                                                mounted => [#{prefix => "/sub", routes => [?WHERE]}],
                                                nova_apps => []}),
    Self = self(),
    Callers = [spawn_monitor(fun() ->
                                     Self ! {added, self(), nova_sup:add_application(nova_test_sub_app,
                                                                                     #{listener => web})}
                             end) || _Caller <- lists:seq(1, 20)],
    Answers = [receive
                   {added, Pid, Answer} -> erlang:demonitor(Monitor, [flush]), Answer;
                   {'DOWN', Monitor, process, Pid, Reason} -> {down, Reason}
               end || {Pid, Monitor} <- Callers],
    ?assertEqual([{ok, nova_test_sub_app, {0, 0, 0, 0}, Port}], [Added || {ok, _, _, _} = Added <- Answers]),
    ?assertEqual(19, length([Again || {error, {already_started, nova_test_sub_app}} = Again <- Answers])),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/sub/where")),
    ok = nova_sup:remove_application(nova_test_sub_app),
    ?assertEqual({error, not_found}, nova_sup:remove_application(nova_test_sub_app)),
    ?assertMatch({404, _}, nova_test_listener:get(Port, "/sub/where")).

%% Over HTTP/2 a WebSocket is an extended CONNECT, which Cowboy takes when
%% it is told to. Without it the route could never be reached.
cowboy_ws_http2(_Config) ->
    Port = nova_test_listener:free_port(),
    Socket = {"/ws", nova_ws_lifecycle_pages, #{protocol => ws}},
    Start = fun(Settings) ->
                    nova_test_listener:try_start(#{server => nova_cowboy,
                                                   port => Port,
                                                   listeners => #{web => Settings#{port => Port,
                                                                                   protocols => [http2]}},
                                                   host => [#{routes => [?WHERE, Socket]}]})
            end,
    ?assertMatch({error, {nova_cowboy,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {websocket_route_not_served, "/ws", web}}}, _}}},
                 Start(#{})),
    ok = nova_test_listener:stop(),
    ?assertEqual({ok, Port}, Start(#{server_opts => #{enable_connect_protocol => true}})).

%% The reason is logged, and is what the node does not start with. The port
%% is taken, as OTP 27 listens with a certificate file that is not there.
start_error_secrets(_Config) ->
    {ok, Taken} = gen_tcp:listen(0, [{ip, {0, 0, 0, 0}}]),
    {ok, Port} = inet:port(Taken),
    Tls = #{certfile => "/nova/no/such/cert.pem", keyfile => "/nova/no/such/key.pem", password => "hunter2"},
    {Refused, Events} = nova_test_log:collect(
                          fun() ->
                                  nova_test_listener:try_start(#{port => Port,
                                                                 listeners => #{web => #{port => Port, tls => Tls}}})
                          end),
    ok = gen_tcp:close(Taken),
    ?assertMatch({error, _}, Refused),
    ?assertEqual(nomatch, string:find(lists:flatten(io_lib:format("~0p", [{Refused, Events}])), "hunter2")).

%% The module of the server is not there, or it is and nothing starts its
%% application.
no_server_warning(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port, listeners => #{web => #{port => Port}}, host => [#{routes => [?WHERE]}]},
    Warned = fun(Events) -> [Server || {warning, {report, #{server := Server}}} <- Events] end,
    ok = nova_test_listener:configure(Options),
    application:set_env(nova, server, nova_no_such_server),
    {{ok, _}, Missing} = nova_test_log:collect(fun() -> application:ensure_all_started(nova) end),
    ?assertEqual([nova_no_such_server], Warned(Missing)),
    ok = application:stop(nova),
    ok = nova_test_listener:configure(Options),
    {ok, _} = application:ensure_all_started(nova),
    {ok, NotStarted} = nova_test_log:collect(fun nova_app:note_no_listener/0),
    ?assertEqual([nova_test_listener:server(#{})], Warned(NotStarted)),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({ok, []}, nova_test_log:collect(fun nova_app:note_no_listener/0)).

%% The warning comes by itself once server_grace is over. It does not on a
%% node that serves nothing on purpose, and a node that was stopped before
%% then does not warn the one started after it.
server_grace(_Config) ->
    Port = nova_test_listener:free_port(),
    Options = #{port => Port, listeners => #{web => #{port => Port}}, host => [#{routes => [?WHERE]}]},
    Run = fun({Grace, Running}) ->
                  application:set_env(nova, server_grace, Grace),
                  {ok, _} = application:ensure_all_started(nova),
                  timer:sleep(Running),
                  ok = application:stop(nova)
          end,
    Warned = fun(Runs) ->
                     ok = nova_test_listener:configure(Options),
                     {_, Events} = nova_test_log:collect(fun() -> lists:foreach(Run, Runs) end),
                     [Setting || {warning, {report, #{setting := Setting, server := _}}} <- Events]
             end,
    try
        ?assertEqual([server_grace], Warned([{50, 300}])),
        ?assertEqual([], Warned([{infinity, 300}])),
        ?assertEqual([], Warned([{200, 0}, {infinity, 400}])),
        ok = nova_test_listener:configure(Options),
        application:set_env(nova, server_grace, "10s"),
        ?assertMatch({error, {nova, {{{invalid_setting, server_grace, "10s"}, _}, _}}},
                     application:ensure_all_started(nova))
    after
        application:unset_env(nova, server_grace)
    end.

two_listeners() ->
    two_listeners([http1]).

two_listeners(PublicProtocols) ->
    Public = nova_test_listener:free_port(),
    Admin = nova_test_listener:free_port(),
    Listeners = #{public => #{port => Public,
                              protocols => PublicProtocols,
                              applications => [nova_test_app]},
                  admin => #{ip => {127, 0, 0, 1},
                             port => Admin,
                             protocols => [http1],
                             applications => [nova_test_sub_app]}},
    Only = {"/only-public", fun nova_listeners_pages:where/1, #{methods => [get]}},
    {ok, Public} = nova_test_listener:try_start(#{port => Public,
                                                  listeners => Listeners,
                                                  host => [#{routes => [?WHERE, Only]}],
                                                  mounted => [#{routes => [?WHERE]}],
                                                  nova_apps => []}),
    {Public, Admin}.
