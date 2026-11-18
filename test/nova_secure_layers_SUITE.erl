%%% A route group with a `security` callback in an application mounted with
%%% `secure`: both callbacks have to allow a request, the one of the mount
%%% first. Every combination of what the two can answer is sent to a
%%% controller, a websocket and a plain handler module.
-module(nova_secure_layers_SUITE).

-export([all/0, groups/0, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).

-export([
         controller_needs_both/1,
         websocket_needs_both/1,
         websocket_gets_the_data/1,
         group_sees_the_data_of_the_mount/1,
         module_handler_needs_both/1,
         group_without_a_callback_has_the_one_of_the_mount/1,
         group_of_an_application_that_is_not_mounted_has_its_own/1,
         apps_of_a_route_group_need_both/1,
         nothing_is_logged_about_the_two_callbacks/1,
         override_replaces_both_for_a_controller/1,
         override_replaces_both_for_a_websocket/1,
         override_replaces_both_for_a_module_handler/1
        ]).

-include_lib("nova/include/nova_router.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MOUNT, fun nova_secure_layers_pages:mount/1).
-define(GROUP, fun nova_secure_layers_pages:group/1).
-define(OVERRIDE, fun nova_secure_layers_pages:override/1).
-define(DECISIONS, [~"allow", ~"data", ~"deny", ~"deny-headers", ~"deny-status", ~"deny-body",
                    ~"redirect", ~"redirect-binary", ~"raise", ~"unexpected"]).

all() ->
    [{group, mount_and_group}, {group, with_override}].

groups() ->
    [{mount_and_group, [], [controller_needs_both,
                            websocket_needs_both,
                            websocket_gets_the_data,
                            group_sees_the_data_of_the_mount,
                            module_handler_needs_both,
                            group_without_a_callback_has_the_one_of_the_mount,
                            group_of_an_application_that_is_not_mounted_has_its_own,
                            apps_of_a_route_group_need_both,
                            nothing_is_logged_about_the_two_callbacks]},
     {with_override, [], [override_replaces_both_for_a_controller,
                          override_replaces_both_for_a_websocket,
                          override_replaces_both_for_a_module_handler]}].

init_per_group(mount_and_group, Config) ->
    start(#{prefix => "/mounted", secure => ?MOUNT}, Config);
init_per_group(with_override, Config) ->
    start(#{prefix => "/mounted", secure => ?MOUNT, override_secure => ?OVERRIDE}, Config).

end_per_group(_Group, _Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    ok = nova_secure_layers_pages:report_calls_to_me(),
    Config.

end_per_testcase(_Case, _Config) ->
    nova_secure_layers_pages:stop_reporting_calls().

start(MountedOptions, Config) ->
    Routes = [{"/", fun nova_secure_layers_pages:page/1, #{methods => [get]}},
              {"/ws", nova_secure_layers_pages, #{protocol => ws}}],
    ok = nova_test_log:start(),
    Port = nova_test_listener:start(
             #{mounted_options => MountedOptions,
               host => [#{prefix => "/host", security => ?GROUP, routes => Routes}],
               mounted => [#{prefix => "/both", security => ?GROUP, routes => Routes},
                           #{prefix => "/mount-only", routes => Routes},
                           #{prefix => "/no-callback", security => false, routes => Routes},
                           #{prefix => "/tools", apps => [nova_test_nested_app]}],
               nested => [#{prefix => "/nested", security => ?GROUP, routes => Routes}]}),
    BootLog = nova_test_log:stop(),
    ok = add_module_handler_route("/mounted/both/cowboy", MountedOptions, #{security => ?GROUP}),
    [{port, Port}, {boot_log, BootLog} | Config].

%% The router has no route syntax for a plain handler module yet, so the
%% value such a route compiles to is put in the routing table directly, with
%% the security the router works out for the mount and the route group.
%% Compiling no applications gives the table as it is.
add_module_handler_route(Path, MountedOptions, RouteGroup) ->
    Value = #nova_module_value{app = nova_test_sub_app,
                                  handler = nova_secure_layers_pages,
                                  arguments = #{},
                                  secure = nova_router:resolve_secure(MountedOptions, RouteGroup, undefined)},
    {ok, Dispatch} = nova_routing_trie:insert('_', Path, '_', Value, nova_router:compile([]), #{}),
    persistent_term:put(nova_dispatch, Dispatch).

controller_needs_both(Config) ->
    [?assertEqual({Mount, Group, both_layers(200, Mount, Group)},
                  {Mount, Group, request(Config, "/mounted/both/", [{mount, Mount}, {group, Group}])})
     || Mount <- ?DECISIONS, Group <- ?DECISIONS].

websocket_needs_both(Config) ->
    [?assertEqual({Mount, Group, without_body(both_layers(101, Mount, Group))},
                  {Mount, Group, handshake(Config, "/mounted/both/ws", [{mount, Mount}, {group, Group}])})
     || Mount <- ?DECISIONS, Group <- ?DECISIONS].

websocket_gets_the_data(Config) ->
    [?assertEqual({Mount, Group, [auth_data(Mount, Group)]},
                  {Mount, Group, nova_test_listener:websocket_texts(?config(port, Config),
                                                                    path("/mounted/both/ws",
                                                                         [{mount, Mount}, {group, Group}]),
                                                                    ~"who", 1)})
     || Mount <- [~"allow", ~"data"], Group <- [~"allow", ~"data"]],
    ?assertEqual([mount, group, mount, group, mount, group, mount, group], nova_secure_layers_pages:calls()).

%% The callback of the group finds what the one of the mount returned in the
%% request, whatever the route leads to.
group_sees_the_data_of_the_mount(Config) ->
    Decisions = [{mount, ~"data"}, {group, ~"data-seen"}],
    Seen = ~"{group,saw,mount}",
    ?assertMatch({200, _Headers, Seen, [mount, group]}, request(Config, "/mounted/both/", Decisions)),
    ?assertMatch({200, _Headers, Seen, [mount, group]}, request(Config, "/mounted/both/cowboy", Decisions)),
    ?assertEqual([Seen],
                 nova_test_listener:websocket_texts(?config(port, Config), path("/mounted/both/ws", Decisions),
                                                    ~"who", 1)),
    ?assertEqual([~"{group,saw,none}"],
                 nova_test_listener:websocket_texts(?config(port, Config),
                                                    path("/mounted/both/ws",
                                                         [{mount, ~"allow"}, {group, ~"data-seen"}]),
                                                    ~"who", 1)).

module_handler_needs_both(Config) ->
    [?assertEqual({Mount, Group, both_layers(200, Mount, Group)},
                  {Mount, Group, request(Config, "/mounted/both/cowboy", [{mount, Mount}, {group, Group}])})
     || Mount <- ?DECISIONS, Group <- ?DECISIONS].

group_without_a_callback_has_the_one_of_the_mount(Config) ->
    [?assertEqual({Path, Mount, one_layer(200, mount, Mount)},
                  {Path, Mount, request(Config, Path, [{mount, Mount}, {group, ~"deny"}])})
     || Path <- ["/mounted/mount-only/", "/mounted/no-callback/"], Mount <- ?DECISIONS].

group_of_an_application_that_is_not_mounted_has_its_own(Config) ->
    [?assertEqual({Group, one_layer(200, group, Group)},
                  {Group, request(Config, "/host/", [{mount, ~"deny"}, {group, Group}])})
     || Group <- ?DECISIONS].

apps_of_a_route_group_need_both(Config) ->
    [?assertEqual({Mount, Group, both_layers(200, Mount, Group)},
                  {Mount, Group, request(Config, "/mounted/tools/nested/", [{mount, Mount}, {group, Group}])})
     || Mount <- ?DECISIONS, Group <- ?DECISIONS].

nothing_is_logged_about_the_two_callbacks(Config) ->
    ?assertEqual([], [Event || #{level := Level} = Event <- ?config(boot_log, Config),
                               lists:member(Level, [warning, error])]).

override_replaces_both_for_a_controller(Config) ->
    [?assertEqual({Override, one_layer(200, override, Override)},
                  {Override, request(Config, "/mounted/both/",
                                     [{override, Override}, {mount, ~"deny"}, {group, ~"deny"}])})
     || Override <- ?DECISIONS].

override_replaces_both_for_a_websocket(Config) ->
    [?assertEqual({Override, without_body(one_layer(101, override, Override))},
                  {Override, handshake(Config, "/mounted/both/ws",
                                       [{override, Override}, {mount, ~"deny"}, {group, ~"deny"}])})
     || Override <- ?DECISIONS],
    ?assertEqual([~"override"],
                 nova_test_listener:websocket_texts(?config(port, Config),
                                                    path("/mounted/both/ws",
                                                         [{override, ~"data"}, {mount, ~"data"}, {group, ~"data"}]),
                                                    ~"who", 1)).

override_replaces_both_for_a_module_handler(Config) ->
    [?assertEqual({Override, one_layer(200, override, Override)},
                  {Override, request(Config, "/mounted/both/cowboy",
                                     [{override, Override}, {mount, ~"deny"}, {group, ~"deny"}])})
     || Override <- ?DECISIONS].

%% What a request is answered with when the mount decides Mount and the
%% route group Group: the status, the headers a refusal sets, the body where
%% it is known, and the callbacks that ran.
both_layers(Allowed, Mount, Group) ->
    case {refusal(mount, Mount), refusal(group, Group)} of
        {allowed, allowed} -> {Allowed, #{}, auth_data(Mount, Group), [mount, group]};
        {allowed, {Status, Headers, Body}} -> {Status, Headers, Body, [mount, group]};
        {{Status, Headers, Body}, _NotAsked} -> {Status, Headers, Body, [mount]}
    end.

one_layer(Allowed, Layer, Decision) ->
    case refusal(Layer, Decision) of
        allowed when Decision =:= ~"data" -> {Allowed, #{}, atom_to_binary(Layer), [Layer]};
        allowed -> {Allowed, #{}, ~"none", [Layer]};
        {Status, Headers, Body} -> {Status, Headers, Body, [Layer]}
    end.

%% The controller gets the data of the group when it returns some, and
%% otherwise that of the mount.
auth_data(_Mount, ~"data") -> ~"group";
auth_data(~"data", _Group) -> ~"mount";
auth_data(_Mount, _Group) -> ~"none".

refusal(_Layer, ~"allow") -> allowed;
refusal(_Layer, ~"data") -> allowed;
refusal(_Layer, ~"deny") -> {401, #{}, any};
refusal(Layer, ~"deny-headers") -> {401, #{~"x-refused-by" => atom_to_binary(Layer)}, any};
refusal(Layer, ~"deny-status") -> {403, #{~"x-refused-by" => atom_to_binary(Layer)}, any};
refusal(Layer, ~"deny-body") -> {403, #{~"x-refused-by" => atom_to_binary(Layer)}, atom_to_binary(Layer)};
refusal(Layer, ~"redirect") -> {302, #{~"location" => <<"/login-of-", (atom_to_binary(Layer))/binary>>}, any};
refusal(Layer, ~"redirect-binary") -> refusal(Layer, ~"redirect");
refusal(_Layer, ~"raise") -> {500, #{}, any};
refusal(_Layer, ~"unexpected") -> {401, #{}, any}.

without_body({Status, Headers, _Body, Calls}) ->
    {Status, Headers, any, Calls}.

%% The response in the shape both_layers/3 gives: only the headers a
%% refusal sets, and the body only where the expected one is known.
request(Config, Path, Decisions) ->
    {Status, Headers, Body} = nova_test_listener:response(?config(port, Config), "GET", path(Path, Decisions), []),
    {Status, refusal_headers(Headers), known_body(Status, Headers, Body), nova_secure_layers_pages:calls()}.

handshake(Config, Path, Decisions) ->
    Response = nova_test_listener:websocket_handshake(?config(port, Config), path(Path, Decisions)),
    <<"HTTP/1.1 ", Status:3/binary, _/binary>> = Response,
    [Head | _Body] = binary:split(Response, ~"\r\n\r\n"),
    [_StatusLine | Lines] = binary:split(Head, ~"\r\n", [global]),
    Headers = #{string:lowercase(Name) => string:trim(Value)
                || Line <- Lines, [Name, Value] <- [binary:split(Line, ~":")]},
    {binary_to_integer(Status), refusal_headers(Headers), any, nova_secure_layers_pages:calls()}.

path(Path, Decisions) ->
    [Path, "?", lists:join("&", [[atom_to_binary(Layer), "=", Decision] || {Layer, Decision} <- Decisions])].

refusal_headers(Headers) ->
    maps:with([~"x-refused-by", ~"location"], Headers).

%% The body of a status page is not what is tested here.
known_body(200, _Headers, Body) -> Body;
known_body(403, #{~"x-refused-by" := Layer}, Layer) -> Layer;
known_body(_Status, _Headers, _Body) -> any.
