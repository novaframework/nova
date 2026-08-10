%%% Multi-listener suite.
%%%
%%% Boots the bootstrap application on one port, then starts a second
%%% application on a second port at runtime and checks that the two listeners
%%% serve their own routes and nothing else. Also covers attaching a second
%%% application to an already-bound port, and tearing both back down.
-module(nova_multi_listener_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         bootstrap_listener_is_registered/1,
         second_application_gets_its_own_port/1,
         listeners_do_not_serve_each_others_routes/1,
         second_application_on_a_bound_port_shares_the_listener/1,
         adding_a_started_application_again_is_an_error/1,
         removing_an_application_stops_only_its_listener/1,
         removing_an_unknown_application_is_an_error/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     bootstrap_listener_is_registered,
     second_application_gets_its_own_port,
     listeners_do_not_serve_each_others_routes,
     second_application_on_a_bound_port_shares_the_listener,
     adding_a_started_application_again_is_an_error,
     removing_an_application_stops_only_its_listener,
     removing_an_unknown_application_is_an_error
    ].

init_per_suite(Config) ->
    Port = free_port(),
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, cowboy_configuration, #{port => Port}),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    {ok, _Started} = application:ensure_all_started(nova_test_app),
    {ok, _} = application:ensure_all_started(inets),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    application:stop(nova_test_app),
    application:stop(nova),
    ok.

%%====================================================================
%% Cases
%%====================================================================

bootstrap_listener_is_registered(Config) ->
    Port = ?config(port, Config),
    Started = nova_sup:get_started_applications(),
    ?assertMatch([_ | _], [S || S = #{app := nova_test_app, port := P} <- Started, P =:= Port]),
    ?assert(lists:member(nova_listener, nova_sup:listeners())).

second_application_gets_its_own_port(_Config) ->
    Port = free_port(),
    {ok, nova_test_sub_app, _Host, Port} =
        nova_sup:add_application(nova_test_sub_app, #{port => Port}),
    try
        {200, Body} = get(Port, "/hello"),
        ?assertEqual(#{<<"app">> => <<"nova_test_sub_app">>}, json(Body))
    after
        nova_sup:remove_application(nova_test_sub_app)
    end.

%% The point of binding a second port: each listener has its own routing
%% table, so neither answers for the other.
listeners_do_not_serve_each_others_routes(Config) ->
    BootstrapPort = ?config(port, Config),
    Port = free_port(),
    {ok, _App, _Host, Port} = nova_sup:add_application(nova_test_sub_app, #{port => Port}),
    try
        %% The second listener serves its own route ...
        {200, _} = get(Port, "/hello"),
        %% ... but not the bootstrap application's.
        {404, _} = get(Port, "/json"),
        %% And the bootstrap listener is unchanged.
        {200, _} = get(BootstrapPort, "/json"),
        {404, _} = get(BootstrapPort, "/hello")
    after
        nova_sup:remove_application(nova_test_sub_app)
    end.

%% Adding an application to a host and port that is already bound must reuse
%% that listener rather than trying to bind the port twice.
second_application_on_a_bound_port_shares_the_listener(Config) ->
    BootstrapPort = ?config(port, Config),
    Before = length(nova_sup:listeners()),
    {ok, nova_test_sub_app, _Host, BootstrapPort} =
        nova_sup:add_application(nova_test_sub_app, #{port => BootstrapPort}),
    try
        ?assertEqual(Before, length(nova_sup:listeners())),
        %% Both applications now answer on the same listener.
        {200, _} = get(BootstrapPort, "/hello"),
        {200, _} = get(BootstrapPort, "/json")
    after
        nova_sup:remove_application(nova_test_sub_app)
    end,
    %% Removing the attached application leaves the listener and the other
    %% application's routes alone.
    ?assertEqual(Before, length(nova_sup:listeners())),
    {404, _} = get(BootstrapPort, "/hello"),
    {200, _} = get(BootstrapPort, "/json").

adding_a_started_application_again_is_an_error(Config) ->
    BootstrapPort = ?config(port, Config),
    ?assertEqual({error, {already_started, nova_test_app}},
                 nova_sup:add_application(nova_test_app, #{port => BootstrapPort})).

removing_an_application_stops_only_its_listener(Config) ->
    BootstrapPort = ?config(port, Config),
    Port = free_port(),
    {ok, _App, _Host, Port} = nova_sup:add_application(nova_test_sub_app, #{port => Port}),
    ?assertEqual(2, length(nova_sup:listeners())),

    ok = nova_sup:remove_application(nova_test_sub_app),
    ?assertEqual([nova_listener], nova_sup:listeners()),

    %% The port is released, and the bootstrap listener still answers.
    ?assertEqual({error, econnrefused}, connect(Port)),
    {200, _} = get(BootstrapPort, "/json").

removing_an_unknown_application_is_an_error(_Config) ->
    ?assertEqual({error, not_found}, nova_sup:remove_application(no_such_app)).

%%====================================================================
%% Helpers
%%====================================================================

get(Port, Path) ->
    Url = "http://localhost:" ++ integer_to_list(Port) ++ Path,
    {ok, {{_Version, Status, _Reason}, _Headers, Body}} =
        httpc:request(get, {Url, []}, [{autoredirect, false}], [{body_format, binary}]),
    {Status, Body}.

json(Body) ->
    {ok, Decoded} = thoas:decode(Body),
    Decoded.

connect(Port) ->
    case gen_tcp:connect("localhost", Port, [{active, false}], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.
