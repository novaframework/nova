%%% The listeners of the `listeners` configuration: each has a name, the
%%% HTTP versions it serves and the applications it serves them for, and a
%%% node that asks for what its server cannot do does not start.
-module(nova_listeners_SUITE).

-export([all/0, end_per_testcase/2]).

-export([
         each_listener_serves_its_own_applications/1,
         a_listener_serves_the_versions_it_names/1,
         listeners_are_known_by_name/1,
         an_application_is_added_to_a_listener_by_name/1,
         a_version_the_server_does_not_have_fails_the_boot/1,
         http3_needs_tls/1,
         a_misconfigured_listener_fails_the_boot/1,
         a_listener_that_cannot_start_leaves_none_behind/1,
         stopping_the_server_package_stops_the_listeners/1,
         stopping_nova_stops_the_listeners/1,
         an_application_does_not_take_the_name_of_a_listener/1,
         an_application_added_before_the_server_is_served_once_it_starts/1,
         an_application_added_before_the_server_that_cannot_be_served_fails_the_boot/1,
         cowboy_takes_websockets_over_http2_when_the_listener_turns_it_on/1,
         a_listener_that_cannot_start_does_not_tell_its_secrets/1,
         nova_without_a_server_package_says_so/1
        ]).

-include_lib("stdlib/include/assert.hrl").

-define(WHERE, {"/where", fun nova_listeners_pages:where/1, #{methods => [get]}}).

all() ->
    [
     each_listener_serves_its_own_applications,
     a_listener_serves_the_versions_it_names,
     listeners_are_known_by_name,
     an_application_is_added_to_a_listener_by_name,
     a_version_the_server_does_not_have_fails_the_boot,
     http3_needs_tls,
     a_misconfigured_listener_fails_the_boot,
     a_listener_that_cannot_start_leaves_none_behind,
     stopping_the_server_package_stops_the_listeners,
     stopping_nova_stops_the_listeners,
     an_application_does_not_take_the_name_of_a_listener,
     an_application_added_before_the_server_is_served_once_it_starts,
     an_application_added_before_the_server_that_cannot_be_served_fails_the_boot,
     cowboy_takes_websockets_over_http2_when_the_listener_turns_it_on,
     a_listener_that_cannot_start_does_not_tell_its_secrets,
     nova_without_a_server_package_says_so
    ].

end_per_testcase(_Case, _Config) ->
    nova_test_listener:stop().

each_listener_serves_its_own_applications(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual({200, ~"public http1"}, nova_test_listener:get(Public, "/where")),
    ?assertEqual({200, ~"admin http1"}, nova_test_listener:get(Admin, "/where")),
    ?assertMatch({404, _}, nova_test_listener:get(Admin, "/only-public")),
    ?assertMatch({200, _}, nova_test_listener:get(Public, "/only-public")).

%% A listener without TLS that serves HTTP/2 alone is one every server has.
a_listener_serves_the_versions_it_names(_Config) ->
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

listeners_are_known_by_name(_Config) ->
    {Public, Admin} = two_listeners(),
    ?assertEqual([admin, public], lists:sort(nova_sup:listeners())),
    ?assertEqual([#{app => nova_test_app, host => {0, 0, 0, 0}, port => Public, listener => public},
                  #{app => nova_test_sub_app, host => {127, 0, 0, 1}, port => Admin, listener => admin}],
                 lists:sort(nova_sup:get_started_applications())).

an_application_is_added_to_a_listener_by_name(_Config) ->
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

a_version_the_server_does_not_have_fails_the_boot(_Config) ->
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

a_misconfigured_listener_fails_the_boot(_Config) ->
    Port = nova_test_listener:free_port(),
    ?assertMatch({error, {_Server,
                          {{shutdown, {failed_to_start_child, nova_listener,
                                       {unknown_listener_settings, web, [use_ssl]}}}, _}}},
                 nova_test_listener:try_start(#{port => Port, listeners => #{web => #{port => Port, use_ssl => true}}})).

a_listener_that_cannot_start_leaves_none_behind(_Config) ->
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

stopping_the_server_package_stops_the_listeners(_Config) ->
    {Public, Admin} = two_listeners(),
    ok = application:stop(nova_test_app),
    ok = application:stop(nova_test_listener:server(#{})),
    ?assertEqual([], nova_sup:listeners()),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Public, [], 1000)),
    ?assertMatch({error, _}, gen_tcp:connect({127, 0, 0, 1}, Admin, [], 1000)),
    ?assertNotEqual(undefined, whereis(nova_sup)).

%% The owner of the listeners watches nova_sup, and takes the server
%% package down with it when nova stops under it.
stopping_nova_stops_the_listeners(_Config) ->
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
an_application_does_not_take_the_name_of_a_listener(_Config) ->
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
an_application_added_before_the_server_is_served_once_it_starts(_Config) ->
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
                 nova_sup:add_application(nova_test_sub_app, #{listener => web})),
    ?assertEqual([], nova_sup:get_started_applications()),
    {ok, Port} = nova_test_listener:boot(Options),
    ?assertEqual({200, ~"web http1"}, nova_test_listener:get(Port, "/where")),
    ?assertEqual({200, ~"nova_test_sub_app http1"}, nova_test_listener:get(Other, "/where")).

an_application_added_before_the_server_that_cannot_be_served_fails_the_boot(_Config) ->
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
        ?assertEqual({error, not_found}, nova_sup:remove_application(nova_test_sub_app))
    after
        gen_tcp:close(Taken)
    end.

%% Over HTTP/2 a WebSocket is an extended CONNECT, which Cowboy takes when
%% it is told to. Without it the route could never be reached.
cowboy_takes_websockets_over_http2_when_the_listener_turns_it_on(_Config) ->
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

%% The reason is logged, and is what the node does not start with.
a_listener_that_cannot_start_does_not_tell_its_secrets(_Config) ->
    Port = nova_test_listener:free_port(),
    Tls = #{certfile => "/nova/no/such/cert.pem", keyfile => "/nova/no/such/key.pem", password => "hunter2"},
    {Refused, Events} = nova_test_log:collect(
                          fun() ->
                                  nova_test_listener:try_start(#{port => Port,
                                                                 listeners => #{web => #{port => Port, tls => Tls}}})
                          end),
    ?assertMatch({error, _}, Refused),
    ?assertEqual(nomatch, string:find(lists:flatten(io_lib:format("~0p", [{Refused, Events}])), "hunter2")).

%% The module of the server is not there, or it is and nothing starts its
%% application.
nova_without_a_server_package_says_so(_Config) ->
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
