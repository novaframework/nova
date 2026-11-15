%%% Boots nova_test_app on a real listener with routes chosen by the calling
%%% suite, and talks to it over a raw socket so request paths reach the
%%% listener untouched.
-module(nova_test_listener).

-export([start/1, try_start/1, configure/1, stop/0, get/2, get/3, request/4, response/4, response/5,
         websocket_handshake/2, websocket_handshake/3, websocket_texts/4, h2_websocket_connect/4, h2_get/2,
         send_and_close/2, free_port/0]).

-spec start(map()) -> inet:port_number().
start(Options) ->
    {ok, Port} = try_start(Options),
    Port.

-spec try_start(map()) -> {ok, inet:port_number()} | {error, term()}.
try_start(Options) ->
    Port = maps:get(port, Options, free_port()),
    ok = configure(Options#{port => Port}),
    case application:ensure_all_started(nova_test_app) of
        {ok, _Started} -> {ok, Port};
        {error, Reason} -> {error, Reason}
    end.

%% Sets the environment a boot reads, without starting anything.
%% nova_test_nested_app and nova_test_deep_app are not applications on
%% disk. Each is a router and the entry that mounts it: `mounted_nova_apps`
%% is the nova_apps of nova_test_sub_app, `nested_nova_apps` the one of
%% nova_test_nested_app.
-spec configure(map()) -> ok.
configure(Options) ->
    Port = maps:get(port, Options, free_port()),
    application:load(nova),
    application:load(nova_test_app),
    application:load(nova_test_sub_app),
    nova_test_host_router:set_routes(maps:get(host, Options, [])),
    nova_test_mounted_router:set_routes(maps:get(mounted, Options, [])),
    nova_test_nested_router:set_routes(maps:get(nested, Options, [])),
    nova_test_deep_router:set_routes(maps:get(deep, Options, [])),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, cowboy_configuration,
                        #{port => Port, options => maps:get(cowboy_options, Options, #{})}),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    application:set_env(nova_test_app, router_module, nova_test_host_router),
    application:set_env(nova_test_app, nova_apps,
                        maps:get(nova_apps, Options,
                                 [{nova_test_sub_app, maps:get(mounted_options, Options, #{prefix => "/mounted"})}])),
    application:set_env(nova_test_sub_app, router_module, nova_test_mounted_router),
    application:set_env(nova_test_sub_app, nova_apps, maps:get(mounted_nova_apps, Options, [])),
    application:set_env(nova_test_nested_app, router_module, nova_test_nested_router),
    application:set_env(nova_test_nested_app, nova_apps, maps:get(nested_nova_apps, Options, [])),
    application:set_env(nova_test_deep_app, router_module, nova_test_deep_router).

%% Unloading puts the environment of both test applications back to what
%% their .app files declare, for the suites that run after this one.
-spec stop() -> ok.
stop() ->
    application:stop(nova_test_app),
    application:stop(nova),
    application:unload(nova_test_app),
    application:unload(nova_test_sub_app),
    application:unset_env(nova, cowboy_configuration),
    application:unset_env(nova_test_nested_app, router_module),
    application:unset_env(nova_test_nested_app, nova_apps),
    application:unset_env(nova_test_deep_app, router_module),
    nova_test_host_router:set_routes([]),
    nova_test_mounted_router:set_routes([]),
    nova_test_nested_router:set_routes([]),
    nova_test_deep_router:set_routes([]),
    ok.

-spec get(inet:port_number(), iodata()) -> {100..599, binary()}.
get(Port, Path) ->
    get(Port, Path, []).

-spec get(inet:port_number(), iodata(), [{iodata(), iodata()}]) -> {100..599, binary()}.
get(Port, Path, Headers) ->
    request(Port, "GET", Path, Headers).

-spec request(inet:port_number(), iodata(), iodata(), [{iodata(), iodata()}]) -> {100..599, binary()}.
request(Port, Method, Path, Headers) ->
    {Status, _RespHeaders, Body} = response(Port, Method, Path, Headers),
    {Status, Body}.

%% As request/4, with the response headers under lowercase names.
-spec response(inet:port_number(), iodata(), iodata(), [{iodata(), iodata()}]) ->
          {100..599, #{binary() => binary()}, binary()}.
response(Port, Method, Path, Headers) ->
    response(Port, Method, Path, Headers, <<>>).

%% As response/4, with a request body.
-spec response(inet:port_number(), iodata(), iodata(), [{iodata(), iodata()}], iodata()) ->
          {100..599, #{binary() => binary()}, binary()}.
response(Port, Method, Path, Headers, RequestBody) ->
    Response = exchange(Port, [Method, " ", Path, " HTTP/1.1\r\n",
                               "host: localhost\r\n",
                               "connection: close\r\n",
                               "content-length: ", integer_to_list(iolist_size(RequestBody)), "\r\n",
                               [[Name, ": ", Value, "\r\n"] || {Name, Value} <- Headers],
                               "\r\n",
                               RequestBody],
                        until_closed),
    <<"HTTP/1.1 ", Status:3/binary, _/binary>> = Response,
    {Head, Body} = case binary:split(Response, ~"\r\n\r\n") of
                       [Head0, Rest] -> {Head0, Rest};
                       [Head0] -> {Head0, <<>>}
                   end,
    [_StatusLine | Lines] = binary:split(Head, ~"\r\n", [global]),
    RespHeaders = #{string:lowercase(Name) => string:trim(Value)
                    || Line <- Lines, [Name, Value] <- [binary:split(Line, ~":")]},
    {binary_to_integer(Status), RespHeaders, Body}.

-spec websocket_handshake(inet:port_number(), iodata()) -> binary().
websocket_handshake(Port, Path) ->
    websocket_handshake(Port, Path, []).

-spec websocket_handshake(inet:port_number(), iodata(), [{iodata(), iodata()}]) -> binary().
websocket_handshake(Port, Path, Headers) ->
    exchange(Port, ["GET ", Path, " HTTP/1.1\r\n",
                    "host: localhost\r\n",
                    "upgrade: websocket\r\n",
                    "connection: Upgrade\r\n",
                    "sec-websocket-key: ", base64:encode(crypto:strong_rand_bytes(16)), "\r\n",
                    [[Name, ": ", Value, "\r\n"] || {Name, Value} <- Headers],
                    "sec-websocket-version: 13\r\n\r\n"],
             first_packet).

%% Upgrades, sends one text frame and returns the text frames the server
%% answers with, up to Count of them. A close frame ends the list with `closed`.
-spec websocket_texts(inet:port_number(), iodata(), binary(), pos_integer()) -> [binary() | closed].
websocket_texts(Port, Path, Text, Count) when byte_size(Text) < 126 ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, ["GET ", Path, " HTTP/1.1\r\n",
                                   "host: localhost\r\n",
                                   "upgrade: websocket\r\n",
                                   "connection: Upgrade\r\n",
                                   "sec-websocket-key: ", base64:encode(crypto:strong_rand_bytes(16)), "\r\n",
                                   "sec-websocket-version: 13\r\n\r\n"]),
        Buffered = after_handshake(Socket, <<>>),
        Mask = crypto:strong_rand_bytes(4),
        ok = gen_tcp:send(Socket, [<<1:1, 0:3, 1:4, 1:1, (byte_size(Text)):7>>, Mask,
                                   crypto:exor(Text, binary:part(binary:copy(Mask, 32), 0, byte_size(Text)))]),
        receive_texts(Socket, Buffered, Count)
    after
        gen_tcp:close(Socket)
    end.

%% Opens a websocket the HTTP/2 way, with an extended CONNECT (RFC 8441) on
%% a clear connection, and returns the status the stream is answered with.
%% The listener needs `enable_connect_protocol` in its Cowboy options.
-spec h2_websocket_connect(inet:port_number(), iodata(), binary(), [{binary(), binary()}]) -> 100..599.
h2_websocket_connect(Port, Path, Authority, Headers) ->
    h2_request(Port, nofin, [{~":method", ~"CONNECT"},
                             {~":protocol", ~"websocket"},
                             {~":scheme", ~"http"},
                             {~":path", iolist_to_binary(Path)},
                             {~":authority", Authority},
                             {~"sec-websocket-version", ~"13"}
                            | Headers]).

%% The status of a GET over HTTP/2, whose `:path` is sent as it is given:
%% HTTP/1.1 cannot ask for a path that does not start with a slash.
-spec h2_get(inet:port_number(), binary()) -> 100..599.
h2_get(Port, Path) ->
    h2_request(Port, fin, [{~":method", ~"GET"}, {~":scheme", ~"http"}, {~":path", Path},
                           {~":authority", ~"localhost"}]).

h2_request(Port, IsFin, Headers) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, [~"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
        {HeaderBlock, _Encoder} = cow_hpack:encode(Headers),
        h2_status(Socket, <<>>, cow_http2:headers(1, IsFin, HeaderBlock))
    after
        gen_tcp:close(Socket)
    end.

%% The request is sent once the settings of the server have arrived, since
%% they are what allows an extended CONNECT.
h2_status(Socket, Buffer, Request) ->
    case cow_http2:parse(Buffer) of
        {ok, {settings, _Settings}, Rest} ->
            ok = gen_tcp:send(Socket, [cow_http2:settings_ack(), Request]),
            h2_status(Socket, Rest, Request);
        {ok, {headers, 1, _IsFin, head_fin, HeaderBlock}, _Rest} ->
            {ResponseHeaders, _Decoder} = cow_hpack:decode(HeaderBlock),
            {~":status", <<Status/binary>>} = lists:keyfind(~":status", 1, ResponseHeaders),
            binary_to_integer(Status);
        {ok, _OtherFrame, Rest} ->
            h2_status(Socket, Rest, Request);
        more ->
            {ok, <<Data/binary>>} = gen_tcp:recv(Socket, 0, 5000),
            h2_status(Socket, <<Buffer/binary, Data/binary>>, Request)
    end.

after_handshake(Socket, Acc) ->
    case binary:split(Acc, ~"\r\n\r\n") of
        [<<"HTTP/1.1 101", _/binary>>, Rest] ->
            Rest;
        [_Incomplete] ->
            {ok, <<Data/binary>>} = gen_tcp:recv(Socket, 0, 5000),
            after_handshake(Socket, <<Acc/binary, Data/binary>>)
    end.

receive_texts(_Socket, _Buffer, 0) ->
    [];
receive_texts(Socket, <<1:1, 0:3, 1:4, 0:1, Length:7, Payload:Length/binary, Rest/binary>>, Count)
  when Length < 126 ->
    [Payload | receive_texts(Socket, Rest, Count - 1)];
receive_texts(_Socket, <<1:1, 0:3, 8:4, _/binary>>, _Count) ->
    [closed];
receive_texts(Socket, Buffer, Count) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, <<Data/binary>>} -> receive_texts(Socket, <<Buffer/binary, Data/binary>>, Count);
        {error, closed} -> [closed]
    end.

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
        {ok, <<Data/binary>>} -> receive_until_closed(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.

%% Sends what is given and hangs up without waiting for an answer, as a
%% client that goes away in the middle of a request does.
-spec send_and_close(inet:port_number(), iodata()) -> ok.
send_and_close(Port, Data) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}]),
    ok = gen_tcp:send(Socket, Data),
    timer:sleep(100),
    gen_tcp:close(Socket).

-spec free_port() -> inet:port_number().
free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.
