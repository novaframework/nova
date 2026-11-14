%%% Boots nova_test_app on a real listener with routes chosen by the calling
%%% suite, and talks to it over a raw socket so request paths reach the
%%% listener untouched.
-module(nova_test_listener).

-export([start/1, stop/0, get/2, get/3, request/4, websocket_handshake/2, free_port/0]).

-spec start(map()) -> inet:port_number().
start(Options) ->
    Port = free_port(),
    application:load(nova),
    application:load(nova_test_app),
    application:load(nova_test_sub_app),
    nova_test_host_router:set_routes(maps:get(host, Options, [])),
    nova_test_mounted_router:set_routes(maps:get(mounted, Options, [])),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, cowboy_configuration,
                        #{port => Port, options => maps:get(cowboy_options, Options, #{})}),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    application:set_env(nova_test_app, router_module, nova_test_host_router),
    application:set_env(nova_test_app, nova_apps,
                        [{nova_test_sub_app, maps:get(mounted_options, Options, #{prefix => "/mounted"})}]),
    application:set_env(nova_test_sub_app, router_module, nova_test_mounted_router),
    {ok, _Started} = application:ensure_all_started(nova_test_app),
    Port.

%% Unloading puts the environment of both test applications back to what
%% their .app files declare, for the suites that run after this one.
-spec stop() -> ok.
stop() ->
    application:stop(nova_test_app),
    application:stop(nova),
    application:unload(nova_test_app),
    application:unload(nova_test_sub_app),
    application:unset_env(nova, cowboy_configuration),
    nova_test_host_router:set_routes([]),
    nova_test_mounted_router:set_routes([]),
    ok.

-spec get(inet:port_number(), iodata()) -> {100..599, binary()}.
get(Port, Path) ->
    get(Port, Path, []).

-spec get(inet:port_number(), iodata(), [{iodata(), iodata()}]) -> {100..599, binary()}.
get(Port, Path, Headers) ->
    request(Port, "GET", Path, Headers).

-spec request(inet:port_number(), iodata(), iodata(), [{iodata(), iodata()}]) -> {100..599, binary()}.
request(Port, Method, Path, Headers) ->
    Response = exchange(Port, [Method, " ", Path, " HTTP/1.1\r\n",
                               "host: localhost\r\n",
                               "connection: close\r\n",
                               "content-length: 0\r\n",
                               [[Name, ": ", Value, "\r\n"] || {Name, Value} <- Headers],
                               "\r\n"],
                        until_closed),
    <<"HTTP/1.1 ", Status:3/binary, _/binary>> = Response,
    Body = case binary:split(Response, ~"\r\n\r\n") of
               [_Head, Rest] -> Rest;
               [_Head] -> <<>>
           end,
    {binary_to_integer(Status), Body}.

-spec websocket_handshake(inet:port_number(), iodata()) -> binary().
websocket_handshake(Port, Path) ->
    exchange(Port, ["GET ", Path, " HTTP/1.1\r\n",
                    "host: localhost\r\n",
                    "upgrade: websocket\r\n",
                    "connection: Upgrade\r\n",
                    "sec-websocket-key: ", base64:encode(crypto:strong_rand_bytes(16)), "\r\n",
                    "sec-websocket-version: 13\r\n\r\n"],
             first_packet).

exchange(Port, Request, Read) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, Request),
        case Read of
            first_packet ->
                {ok, Packet} = gen_tcp:recv(Socket, 0, 5000),
                Packet;
            until_closed ->
                receive_until_closed(Socket, <<>>)
        end
    after
        gen_tcp:close(Socket)
    end.

receive_until_closed(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 10000) of
        {ok, Data} -> receive_until_closed(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.

-spec free_port() -> inet:port_number().
free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.
