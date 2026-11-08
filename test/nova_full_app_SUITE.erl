%%% End-to-end suite for Nova.
%%%
%%% This boots a real Nova application (nova_test_app, plus nova_test_sub_app
%%% mounted under a prefix) on a real Cowboy listener and drives it over HTTP
%%% and WebSocket. It exists so that framework-level regressions - a router
%%% that will not compile, a dispatch table that cannot be looked up, a
%%% listener that never binds - fail here rather than in a downstream
%%% application.
%%%
%%% If you add a feature to Nova, add a route for it to nova_test_app_router
%%% and a case here.
-module(nova_full_app_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([forward_event/4]).

-export([
         root_route/1,
         json_route/1,
         single_binding/1,
         multiple_bindings/1,
         literal_beats_binding/1,
         all_declared_methods/1,
         any_method_route/1,
         method_not_allowed/1,
         redirect/1,
         custom_status_code/1,
         extra_state_reaches_controller/1,
         host_scoped_route/1,
         static_file_from_priv/1,
         static_file_nested/1,
         static_directory_index/1,
         static_file_missing/1,
         secure_route_rejects_anonymous/1,
         secure_route_accepts_token/1,
         auth_data_reaches_controller/1,
         pre_and_post_request_plugins_run/1,
         custom_not_found/1,
         controller_crash_is_a_500/1,
         handler_init_crash_is_a_500/1,
         websocket_echo/1,
         sub_app_mounted_under_prefix/1,
         add_and_remove_application/1,
         route_template_controller/1,
         route_template_static/1,
         route_template_sub_app/1,
         route_template_absent_on_404/1,
         route_template_websocket/1,
         telemetry_controller_and_plugins/1,
         telemetry_controller_exception/1,
         telemetry_controller_status_is_a_stop/1,
         telemetry_websocket/1,
         telemetry_websocket_exception/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     {group, routing},
     {group, static_files},
     {group, security},
     {group, plugins},
     {group, errors},
     {group, websocket},
     {group, sub_apps},
     {group, runtime_routes},
     {group, route_template},
     {group, telemetry}
    ].

groups() ->
    [
     {routing, [parallel], [
                            root_route,
                            json_route,
                            single_binding,
                            multiple_bindings,
                            literal_beats_binding,
                            all_declared_methods,
                            any_method_route,
                            method_not_allowed,
                            redirect,
                            custom_status_code,
                            extra_state_reaches_controller,
                            host_scoped_route
                           ]},
     {static_files, [parallel], [
                                 static_file_from_priv,
                                 static_file_nested,
                                 static_directory_index,
                                 static_file_missing
                                ]},
     {security, [parallel], [
                             secure_route_rejects_anonymous,
                             secure_route_accepts_token,
                             auth_data_reaches_controller
                            ]},
     {plugins, [], [
                    pre_and_post_request_plugins_run
                   ]},
     {errors, [parallel], [
                           custom_not_found,
                           controller_crash_is_a_500,
                           handler_init_crash_is_a_500
                          ]},
     {websocket, [], [
                      websocket_echo
                     ]},
     {sub_apps, [parallel], [
                             sub_app_mounted_under_prefix
                            ]},
     {runtime_routes, [], [
                           add_and_remove_application
                          ]},
     {route_template, [parallel], [
                                   route_template_controller,
                                   route_template_static,
                                   route_template_sub_app,
                                   route_template_absent_on_404,
                                   route_template_websocket
                                  ]},
     {telemetry, [], [
                      telemetry_controller_and_plugins,
                      telemetry_controller_exception,
                      telemetry_controller_status_is_a_stop,
                      telemetry_websocket,
                      telemetry_websocket_exception
                     ]}
    ].

%%====================================================================
%% Setup
%%====================================================================

init_per_suite(Config) ->
    Port = free_port(),
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, cowboy_configuration, #{port => Port}),
    application:set_env(nova, environment, test),
    application:set_env(nova, use_stacktrace, true),
    application:set_env(nova, plugins, []),
    %% nova_test_app_plugin counts the phases it runs in through this.
    persistent_term:put(nova_test_app_plugin, counters:new(2, [write_concurrency])),
    {ok, _Started} = application:ensure_all_started(nova_test_app),
    {ok, _} = application:ensure_all_started(inets),
    [{port, Port}, {base, "http://localhost:" ++ integer_to_list(Port)} | Config].

end_per_suite(_Config) ->
    persistent_term:erase(nova_test_app_plugin),
    application:stop(nova_test_app),
    application:stop(nova),
    ok.

%%====================================================================
%% Routing
%%====================================================================

root_route(Config) ->
    {200, _Headers, Body} = get(Config, "/"),
    ?assertEqual(<<"index">>, Body).

json_route(Config) ->
    {200, Headers, Body} = get(Config, "/json"),
    ?assertMatch(<<"application/json", _/binary>>, header(<<"content-type">>, Headers)),
    ?assertEqual(#{<<"ok">> => true, <<"from">> => <<"nova_test_app">>}, json(Body)).

single_binding(Config) ->
    {200, _Headers, Body} = get(Config, "/echo/42"),
    ?assertEqual(#{<<"id">> => <<"42">>}, json(Body)).

multiple_bindings(Config) ->
    {200, _Headers, Body} = get(Config, "/echo/42/comments/7"),
    ?assertEqual(#{<<"id">> => <<"42">>, <<"comment_id">> => <<"7">>}, json(Body)).

%% A literal segment must win over a binding at the same depth, and the
%% binding must still match everything else.
literal_beats_binding(Config) ->
    {200, _H1, Literal} = get(Config, "/users/new"),
    ?assertEqual(#{<<"matched">> => <<"literal">>}, json(Literal)),
    {200, _H2, Binding} = get(Config, "/users/123"),
    ?assertEqual(#{<<"id">> => <<"123">>}, json(Binding)).

%% Nova answers a bare {json, _} with 201 on POST and 200 otherwise, so the
%% expected status is method-dependent.
all_declared_methods(Config) ->
    [begin
         {Status, _Headers, Body} = request(Config, Method, "/methods"),
         ?assertEqual(expected_status(Method), Status),
         ?assertEqual(#{<<"method">> => list_to_binary(string:uppercase(atom_to_list(Method)))},
                      json(Body))
     end || Method <- [get, post, put, delete, patch]].

any_method_route(Config) ->
    [begin
         {Status, _Headers, _Body} = request(Config, Method, "/any-method"),
         ?assertEqual(expected_status(Method), Status)
     end || Method <- [get, post, put, delete]].

expected_status(post) -> 201;
expected_status(_)    -> 200.

method_not_allowed(Config) ->
    {405, Headers, _Body} = request(Config, post, "/get-only"),
    ?assertEqual(<<"GET">>, header(<<"allow">>, Headers)).

redirect(Config) ->
    {302, Headers, _Body} = get_no_redirect(Config, "/redirect"),
    ?assertEqual(<<"/json">>, header(<<"location">>, Headers)).

custom_status_code(Config) ->
    {418, _Headers, Body} = get(Config, "/teapot"),
    ?assertEqual(<<"short and stout">>, Body).

extra_state_reaches_controller(Config) ->
    {200, _Headers, Body} = get(Config, "/extra"),
    ?assertEqual(#{<<"answer">> => 42}, json(Body)).

%% Host-scoped routes are only served for their host, and the catch-all host
%% tree is not consulted for them.
host_scoped_route(Config) ->
    {200, _Headers, Body} = get(Config, "/host", [{"host", "api.localhost"}]),
    ?assertEqual(#{<<"host_scoped">> => true}, json(Body)),
    {404, _H, _B} = get(Config, "/host").

%%====================================================================
%% Static files
%%====================================================================

static_file_from_priv(Config) ->
    {200, _Headers, Body} = get(Config, "/assets/hello.txt"),
    ?assertEqual(<<"hello from priv\n">>, Body).

static_file_nested(Config) ->
    {200, _Headers, Body} = get(Config, "/assets/nested/deep.txt"),
    ?assertEqual(<<"nested file\n">>, Body).

static_directory_index(Config) ->
    {200, _Headers, Body} = get(Config, "/assets/nested/index.html"),
    ?assertMatch(<<"<html>", _/binary>>, Body).

static_file_missing(Config) ->
    {404, _Headers, _Body} = get(Config, "/assets/does-not-exist.txt").

%%====================================================================
%% Security
%%====================================================================

secure_route_rejects_anonymous(Config) ->
    {401, _Headers, _Body} = get(Config, "/secure/").

secure_route_accepts_token(Config) ->
    {200, _Headers, Body} = get(Config, "/secure/", [{"authorization", "Bearer let-me-in"}]),
    ?assertEqual(#{<<"secret">> => true}, json(Body)).

auth_data_reaches_controller(Config) ->
    {200, _Headers, Body} = get(Config, "/secure/data", [{"authorization", "Bearer let-me-in"}]),
    ?assertEqual(#{<<"user">> => <<"tester">>, <<"role">> => <<"admin">>}, json(Body)).

%%====================================================================
%% Plugins
%%====================================================================

%% pre_request can still touch the response; post_request runs after
%% nova_handler has replied, so it is observed through the plugin's counter.
pre_and_post_request_plugins_run(Config) ->
    Counters = persistent_term:get(nova_test_app_plugin),
    PreBefore = counters:get(Counters, 1),
    PostBefore = counters:get(Counters, 2),

    {200, Headers, _Body} = get(Config, "/json"),
    ?assertEqual(<<"1">>, header(<<"x-nova-pre-request">>, Headers)),

    ?assert(counters:get(Counters, 1) > PreBefore),
    ?assert(counters:get(Counters, 2) > PostBefore).

%%====================================================================
%% Errors
%%====================================================================

custom_not_found(Config) ->
    {404, _Headers, Body} = get(Config, "/no-such-route"),
    ?assertEqual(<<"custom not found">>, Body).

controller_crash_is_a_500(Config) ->
    {500, _Headers, _Body} = get(Config, "/crash").

%% A Cowboy handler route whose init crashes is a server error, not a 404.
handler_init_crash_is_a_500(Config) ->
    {500, _Headers, _Body} = get(Config, "/ws-crash").

%%====================================================================
%% WebSocket
%%====================================================================

websocket_echo(Config) ->
    {ok, Socket} = ws_connect(Config, "/ws"),
    ?assertEqual(<<"connected">>, ws_recv(Socket)),
    ok = ws_send(Socket, <<"ping">>),
    ?assertEqual(<<"pong">>, ws_recv(Socket)),
    ok = ws_send(Socket, <<"hello">>),
    ?assertEqual(<<"echo:hello">>, ws_recv(Socket)),
    gen_tcp:close(Socket).

%%====================================================================
%% Sub-applications
%%====================================================================

sub_app_mounted_under_prefix(Config) ->
    {200, _Headers, Body} = get(Config, "/sub/hello"),
    ?assertEqual(<<"nova_test_sub_app">>, maps:get(<<"app">>, json(Body))).

%%====================================================================
%% Runtime route changes
%%====================================================================

%% Adding routes at runtime must not disturb the routes already registered,
%% and removing an application must take only its own routes with it.
add_and_remove_application(Config) ->
    ok = nova_router:add_routes(runtime_app,
                                [#{routes => [{"/runtime",
                                               fun(_Req) -> {json, #{runtime => true}} end,
                                               #{methods => [get]}}]}]),
    {200, _H1, Body} = get(Config, "/runtime"),
    ?assertEqual(#{<<"runtime">> => true}, json(Body)),
    ?assertMatch({runtime_app, _}, lists:keyfind(runtime_app, 1, nova_router:compiled_apps())),

    ok = nova_router:remove_application(runtime_app),
    {404, _H2, _B2} = get(Config, "/runtime"),
    ?assertEqual(false, lists:keyfind(runtime_app, 1, nova_router:compiled_apps())),

    %% Everything the other applications registered is still there.
    {200, _H3, _B3} = get(Config, "/json"),
    {200, _H4, _B4} = get(Config, "/sub/hello"),
    {200, _H5, _B5} = get(Config, "/assets/hello.txt").

%%====================================================================
%% Route template
%%====================================================================

route_template_controller(Config) ->
    {200, Headers, _Body} = get(Config, "/echo/42/comments/7"),
    ?assertEqual(<<"/echo/:id/comments/:comment_id">>, header(<<"x-nova-route">>, Headers)).

route_template_static(Config) ->
    {200, Headers, _Body} = get(Config, "/assets/nested/deep.txt"),
    ?assertEqual(<<"/assets/[...]">>, header(<<"x-nova-route">>, Headers)).

route_template_sub_app(Config) ->
    {200, _Headers, Body} = get(Config, "/sub/hello"),
    ?assertEqual(<<"/sub/hello">>, maps:get(<<"route">>, json(Body))).

route_template_absent_on_404(Config) ->
    {404, Headers, _Body} = get(Config, "/no-such-route"),
    ?assertEqual(undefined, header(<<"x-nova-route">>, Headers)).

route_template_websocket(Config) ->
    {ok, Socket} = ws_connect(Config, "/ws"),
    ?assertEqual(<<"connected">>, ws_recv(Socket)),
    ok = ws_send(Socket, <<"route">>),
    ?assertEqual(<<"/ws">>, ws_recv(Socket)),
    gen_tcp:close(Socket).

%%====================================================================
%% Telemetry
%%====================================================================

-define(CONTROLLER_META, [app, controller, action, route, status_page, method, path, host, scheme,
                          version, peer, telemetry_span_context]).
-define(PLUGIN_META, [plugin, callback, app, route, telemetry_span_context]).
-define(WS_HANDLE_META, [callback, module, app, route, telemetry_span_context]).
-define(WS_CONNECTION_META, [module, app, route, telemetry_span_context]).
-define(EXCEPTION_META, [kind, reason, stacktrace]).

telemetry_controller_and_plugins(Config) ->
    Id = attach([[nova, controller, start], [nova, controller, stop],
                 [nova, plugin, start], [nova, plugin, stop]]),
    {200, _Headers, _Body} = get(Config, "/echo/42"),
    Route = <<"/echo/:id">>,

    {StartM, Start} = next_event([nova, controller, start], Route),
    assert_keys([monotonic_time, system_time], StartM),
    assert_keys(?CONTROLLER_META, Start),
    ?assertMatch(#{app := nova_test_app, controller := nova_test_app_controller, action := echo,
                   status_page := false,
                   method := <<"GET">>, path := <<"/echo/42">>, host := <<"localhost">>,
                   scheme := <<"http">>, version := 'HTTP/1.1', peer := {_, _}}, Start),
    ?assertNot(maps:is_key(req, Start)),

    {StopM, Stop} = next_event([nova, controller, stop], Route),
    assert_keys([duration, monotonic_time], StopM),
    assert_keys([status | ?CONTROLLER_META], Stop),
    ?assertMatch(#{status := 200, action := echo}, Stop),
    ?assertNot(maps:is_key(req, Stop)),

    [begin
         {PStartM, PStart} = next_event([nova, plugin, start], Route),
         assert_keys([monotonic_time, system_time], PStartM),
         assert_keys(?PLUGIN_META, PStart),
         ?assertMatch(#{plugin := nova_test_app_plugin, callback := Phase, app := nova_test_app}, PStart),
         {PStopM, PStop} = next_event([nova, plugin, stop], Route),
         assert_keys([duration, monotonic_time], PStopM),
         assert_keys([result | ?PLUGIN_META], PStop),
         ?assertMatch(#{callback := Phase, result := ok}, PStop)
     end || Phase <- [pre_request, post_request]],
    telemetry:detach(Id).

%% The crash is an exception on the failing span; the error page Nova renders
%% for it is a controller span of its own, owned by app nova.
telemetry_controller_exception(Config) ->
    Id = attach([[nova, controller, stop], [nova, controller, exception]]),
    {500, _Headers, _Body} = get(Config, "/crash"),
    {ExcM, Exc} = next_event([nova, controller, exception], <<"/crash">>),
    assert_keys([duration, monotonic_time], ExcM),
    assert_keys(?EXCEPTION_META ++ ?CONTROLLER_META, Exc),
    ?assertMatch(#{kind := error, reason := deliberate_crash, action := crash,
                   app := nova_test_app, status_page := false}, Exc),
    assert_no_arguments(Exc),
    {_ErrorPageM, ErrorPage} = next_event([nova, controller, stop], <<"/crash">>),
    ?assertMatch(#{app := nova, status := 500, status_page := true,
                   controller := nova_error_controller}, ErrorPage),
    telemetry:detach(Id).

telemetry_controller_status_is_a_stop(Config) ->
    Id = attach([[nova, controller, stop], [nova, controller, exception]]),
    try
        [begin
             {Status, _Headers, _Body} = get(Config, Path),
             {_StopM, Stop} = next_event([nova, controller, stop], list_to_binary(Path)),
             ?assertMatch(#{action := Action, status := Status}, Stop)
         end || {Path, Action, Status} <- [{"/forbidden", forbidden, 403}, {"/gone", gone, 410}]],
        %% Events are sent from the request process before it replies, so an
        %% exception for these spans would already be in the mailbox.
        ?assertEqual(none, receive
                               {telemetry, [nova, controller, exception], _, _} = Unexpected -> Unexpected
                           after 0 -> none
                           end)
    after
        telemetry:detach(Id)
    end.

telemetry_websocket(Config) ->
    Id = attach([[nova, websocket, connection, start], [nova, websocket, connection, stop],
                 [nova, websocket, handle, start], [nova, websocket, handle, stop]]),
    {ok, Socket} = ws_connect(Config, "/ws"),
    ?assertEqual(<<"connected">>, ws_recv(Socket)),
    ok = ws_send(Socket, <<"ping">>),
    ?assertEqual(<<"pong">>, ws_recv(Socket)),

    {ConnM, Conn} = next_event([nova, websocket, connection, start], <<"/ws">>),
    assert_keys([monotonic_time, system_time], ConnM),
    assert_keys(?WS_CONNECTION_META, Conn),
    ?assertMatch(#{module := nova_test_app_ws_controller, app := nova_test_app}, Conn),

    [begin
         {HStartM, HStart} = next_event([nova, websocket, handle, start], <<"/ws">>),
         assert_keys([monotonic_time, system_time], HStartM),
         assert_keys(?WS_HANDLE_META, HStart),
         ?assertMatch(#{callback := Callback, module := nova_test_app_ws_controller,
                        app := nova_test_app}, HStart),
         {HStopM, HStop} = next_event([nova, websocket, handle, stop], <<"/ws">>),
         assert_keys([duration, monotonic_time], HStopM),
         ?assertMatch(#{callback := Callback}, HStop)
     end || Callback <- [websocket_init, websocket_handle]],

    gen_tcp:close(Socket),
    {StopM, Stop} = next_event([nova, websocket, connection, stop], <<"/ws">>),
    assert_keys([duration, monotonic_time], StopM),
    assert_keys([reason | ?WS_CONNECTION_META], Stop),
    ?assertEqual(maps:get(telemetry_span_context, Conn), maps:get(telemetry_span_context, Stop)),
    telemetry:detach(Id).

%% nova_ws_handler turns a crash into {stop, State}; the exception event must
%% still fire, and the connection still ends with a stop.
telemetry_websocket_exception(Config) ->
    Id = attach([[nova, websocket, handle, exception], [nova, websocket, connection, stop]]),
    {ok, Socket} = ws_connect(Config, "/ws"),
    ?assertEqual(<<"connected">>, ws_recv(Socket)),
    ok = ws_send(Socket, <<"crash">>),
    {ExcM, Exc} = next_event([nova, websocket, handle, exception], <<"/ws">>),
    assert_keys([duration, monotonic_time], ExcM),
    assert_keys(?EXCEPTION_META ++ ?WS_HANDLE_META, Exc),
    ?assertMatch(#{kind := error, reason := deliberate_ws_crash, callback := websocket_handle}, Exc),
    assert_no_arguments(Exc),
    {_StopM, _Stop} = next_event([nova, websocket, connection, stop], <<"/ws">>),
    gen_tcp:close(Socket),
    telemetry:detach(Id).

attach(Events) ->
    Id = make_ref(),
    ok = telemetry:attach_many(Id, Events, fun ?MODULE:forward_event/4, self()),
    Id.

forward_event(Event, Measurements, Metadata, Pid) ->
    Pid ! {telemetry, Event, Measurements, Metadata}.

next_event(Event, Route) ->
    receive
        {telemetry, Event, Measurements, Metadata = #{route := Route}} ->
            {Measurements, Metadata}
    after 5000 ->
            ct:fail({no_event, Event, Route})
    end.

assert_no_arguments(#{stacktrace := Stacktrace}) ->
    ?assertMatch([_ | _], Stacktrace),
    ?assertEqual([], [Entry || {_, _, Arguments, _} = Entry <- Stacktrace, not is_integer(Arguments)]).

assert_keys(Expected, Map) ->
    ?assertEqual(lists:sort(Expected), lists:sort(maps:keys(Map))).

%%====================================================================
%% HTTP helpers
%%====================================================================

get(Config, Path) ->
    get(Config, Path, []).

get(Config, Path, Headers) ->
    do_request(get, url(Config, Path), Headers, [{autoredirect, false}]).

get_no_redirect(Config, Path) ->
    do_request(get, url(Config, Path), [], [{autoredirect, false}]).

request(Config, Method, Path) ->
    do_request(Method, url(Config, Path), [], [{autoredirect, false}]).

do_request(Method, Url, Headers, Options) ->
    Request =
        case Method of
            get -> {Url, Headers};
            _   -> {Url, Headers, "application/json", <<"{}">>}
        end,
    {ok, {{_Version, Status, _Reason}, RespHeaders, Body}} =
        httpc:request(Method, Request, Options, [{body_format, binary}]),
    {Status, RespHeaders, Body}.

url(Config, Path) ->
    ?config(base, Config) ++ Path.

header(Name, Headers) ->
    Lower = string:lowercase(binary_to_list(Name)),
    case lists:keyfind(Lower, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_Key, Value} -> list_to_binary(Value);
        false         -> undefined
    end.

json(Body) ->
    {ok, Decoded} = thoas:decode(Body),
    Decoded.

free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.

%%====================================================================
%% Minimal WebSocket client
%%
%% Only enough of RFC 6455 to open a connection and exchange short unmasked
%% text frames, so that the suite does not need a WebSocket client dependency.
%%====================================================================

ws_connect(Config, Path) ->
    Port = ?config(port, Config),
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Handshake = [
                 "GET ", Path, " HTTP/1.1\r\n",
                 "Host: localhost:", integer_to_list(Port), "\r\n",
                 "Upgrade: websocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Key: ", Key, "\r\n",
                 "Sec-WebSocket-Version: 13\r\n\r\n"
                ],
    ok = gen_tcp:send(Socket, Handshake),
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    case binary:match(Response, <<"101">>) of
        nomatch -> {error, {handshake_failed, Response}};
        _       -> {ok, Socket}
    end.

ws_send(Socket, Payload) ->
    Mask = crypto:strong_rand_bytes(4),
    Masked = mask(Payload, Mask, 0, <<>>),
    Length = byte_size(Payload),
    true = Length < 126,
    gen_tcp:send(Socket, <<1:1, 0:3, 1:4, 1:1, Length:7, Mask/binary, Masked/binary>>).

ws_recv(Socket) ->
    {ok, <<_Fin:1, _Rsv:3, _Opcode:4, 0:1, Length:7>>} = gen_tcp:recv(Socket, 2, 5000),
    true = Length < 126,
    case Length of
        0 -> <<>>;
        _ ->
            {ok, Payload} = gen_tcp:recv(Socket, Length, 5000),
            Payload
    end.

mask(<<>>, _Mask, _Index, Acc) ->
    Acc;
mask(<<Byte, Rest/binary>>, Mask, Index, Acc) ->
    MaskByte = binary:at(Mask, Index rem 4),
    mask(Rest, Mask, Index + 1, <<Acc/binary, (Byte bxor MaskByte)>>).
