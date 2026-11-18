%%% Regression suite for the Cowboy and Cowlib advisories that the pinned
%%% versions fix: CVE-2026-115701 (Cowboy), CVE-2026-113431 and CVE-2026-43966
%%% (Cowlib).
%%%
%%% Nova pins Cowboy exactly, so an application cannot pick up a fixed Cowboy
%%% on its own. These cases drive a real Nova listener over a raw socket and
%%% fail if the pins move back to an affected version, or if Nova's listener
%%% setup ever turns one of the protections off.
-module(nova_cowboy_advisory_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         pipelined_data_is_not_buffered_while_a_controller_runs/1,
         http2_oversized_header_list_is_refused/1,
         structured_field_strings_reject_control_bytes/1,
         controller_header_with_a_line_break_does_not_split_the_response/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(FLOOD_BYTES, (4 * 1024 * 1024)).

all() ->
    [
     pipelined_data_is_not_buffered_while_a_controller_runs,
     http2_oversized_header_list_is_refused,
     structured_field_strings_reject_control_bytes,
     controller_header_with_a_line_break_does_not_split_the_response
    ].

init_per_suite(Config) ->
    Port = nova_test_listener:free_port(),
    application:load(nova),
    application:set_env(nova, bootstrap_application, nova_test_app),
    application:set_env(nova, cowboy_configuration, #{port => Port}),
    application:set_env(nova, environment, test),
    application:set_env(nova, plugins, []),
    persistent_term:put(nova_test_app_plugin, counters:new(2, [write_concurrency])),
    {ok, _Started} = application:ensure_all_started(nova_test_app),
    ok = nova_router:add_routes(?MODULE, [#{routes => [{"/hold", fun hold/1,
                                                        #{methods => [get]}},
                                                       {"/reflect", fun reflect/1,
                                                        #{methods => [get]}}]}]),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    persistent_term:erase(nova_test_app_plugin),
    application:stop(nova_test_app),
    application:stop(nova_cowboy),
    application:stop(nova),
    ok.

%%====================================================================
%% CVE-2026-115701
%%====================================================================

%% While a controller is still working on a request, whatever else the client
%% sends on the connection has to stay in the kernel, where TCP flow control
%% pushes back on the sender, instead of piling up in the connection process.
pipelined_data_is_not_buffered_while_a_controller_runs(Config) ->
    true = register(?MODULE, self()),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config),
                                   [binary, {active, false}, {send_timeout, 500}]),
    ok = gen_tcp:send(Socket, ~"GET /hold HTTP/1.1\r\nhost: localhost\r\n\r\n"),
    Controller = receive {holding, Pid} -> Pid after 5000 -> ct:fail(controller_not_reached) end,
    %% The send times out once the kernel buffers are full, which is fine.
    _ = gen_tcp:send(Socket, binary:copy(~"X", ?FLOOD_BYTES)),
    %% A connection that kept reading would need this long to take the flood in.
    timer:sleep(1000),
    Buffered = binary_bytes(connection_pid(Socket)),
    Controller ! release,
    ?assert(Buffered < ?FLOOD_BYTES div 4, #{buffered_bytes => Buffered}),
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    ?assertMatch(<<"HTTP/1.1 200", _/binary>>, Response),
    %% Drops whatever part of the flood is still queued instead of waiting for it.
    ok = inet:setopts(Socket, [{linger, {true, 0}}]),
    gen_tcp:close(Socket).

hold(_Req) ->
    ?MODULE ! {holding, self()},
    receive release -> ok after 10000 -> ok end,
    {status, 200, #{}, ~"released"}.

%% The process that owns the server end of the client's connection.
connection_pid(Socket) ->
    {ok, Client} = inet:sockname(Socket),
    [Pid] = [Pid || Port <- erlang:ports(),
                    erlang:port_info(Port, name) =:= {name, "tcp_inet"},
                    inet:peername(Port) =:= {ok, Client},
                    {connected, Pid} <- [erlang:port_info(Port, connected)]],
    Pid.

binary_bytes(Pid) ->
    {binary, Binaries} = process_info(Pid, binary),
    lists:sum([Size || {_Id, Size, _RefCount} <- Binaries]).

%%====================================================================
%% CVE-2026-113431
%%====================================================================

%% HPACK lets a few bytes on the wire stand for a large header, so the size
%% of the frames says nothing about the size of the decoded header list. The
%% list itself is limited, to 32768 bytes unless the max_header_list_size
%% Cowboy option says otherwise, and a request over the limit is reset.
http2_oversized_header_list_is_refused(Config) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config),
                                   [binary, {active, false}]),
    ok = gen_tcp:send(Socket, [~"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
                               cow_http2:settings(#{}),
                               cow_http2:settings_ack()]),
    {Accepted, Encoder} = cow_hpack:encode(request_headers([]), cow_hpack:init()),
    ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, Accepted)),
    {{headers, Block}, Buffer} = await_response(Socket, 1, <<>>),
    {ResponseHeaders, _Decoder} = cow_hpack:decode(Block),
    ?assertEqual({~":status", ~"200"}, lists:keyfind(~":status", 1, ResponseHeaders)),

    %% cow_hpack indexes this header, so every repeat after the first is one byte.
    Padding = [{~"accept-language", binary:copy(~"a", 4000)} || _ <- lists:seq(1, 10)],
    {Oversized, _Encoder} = cow_hpack:encode(request_headers(Padding), Encoder),
    ?assert(iolist_size(Oversized) < 16384),
    ok = gen_tcp:send(Socket, cow_http2:headers(3, fin, Oversized)),
    {Refusal, _Buffer} = await_response(Socket, 3, Buffer),
    ?assertEqual({rst_stream, enhance_your_calm}, Refusal),
    gen_tcp:close(Socket).

request_headers(Extra) ->
    [{~":method", ~"GET"},
     {~":scheme", ~"http"},
     {~":authority", ~"localhost"},
     {~":path", ~"/"} | Extra].

%% The first HEADERS or RST_STREAM frame the server sends for the stream.
await_response(Socket, StreamID, Buffer) ->
    case cow_http2:parse(Buffer) of
        {ok, {headers, StreamID, _IsFin, head_fin, Block}, Rest} ->
            {{headers, Block}, Rest};
        {ok, {rst_stream, StreamID, Reason}, Rest} ->
            {{rst_stream, Reason}, Rest};
        {ok, _Frame, Rest} ->
            await_response(Socket, StreamID, Rest);
        more ->
            case gen_tcp:recv(Socket, 0, 5000) of
                {ok, Data} when is_binary(Data) ->
                    await_response(Socket, StreamID, <<Buffer/binary, Data/binary>>)
            end
    end.

%%====================================================================
%% CVE-2026-43966
%%====================================================================

%% The advisory is in the structured field builders. Nova does not call them,
%% but an application that puts their output into a response header relies on
%% them never emitting a byte that ends the header line.
structured_field_strings_reject_control_bytes(_Config) ->
    ?assertEqual(~"\"plain\"", build_string_item(~"plain")),
    [?assertError(_, build_string_item(Value))
     || Value <- [~"a\r\nb", ~"a\nb", ~"a\rb", ~"a\0b", ~"a\x7Fb"]].

build_string_item(Value) ->
    iolist_to_binary(cow_http_struct_hd:item({item, {string, Value}, []})).

%% The same class of bug one level up: a controller that copies request data
%% into a response header. Cowboy refuses to send a header value holding a
%% line break and answers 500, unless the invalid_response_headers Cowboy
%% option is set to ignore.
controller_header_with_a_line_break_does_not_split_the_response(Config) ->
    ?assertMatch(<<"HTTP/1.1 200", _/binary>>, reflect(Config, ~"plain")),
    [begin
         Response = reflect(Config, Value),
         ?assertMatch(<<"HTTP/1.1 500", _/binary>>, Response),
         ?assertEqual(nomatch, binary:match(Response, ~"x-injected"))
     end || Value <- [~"a%0d%0ax-injected:%20yes", ~"a%0ax-injected:%20yes",
                      ~"a%0dx-injected:%20yes"]].

reflect(Config, Value) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, ?config(port, Config),
                                   [binary, {active, false}]),
    ok = gen_tcp:send(Socket, [~"GET /reflect?value=", Value, ~" HTTP/1.1\r\n",
                               ~"host: localhost\r\nconnection: close\r\n\r\n"]),
    Response = recv_until_closed(Socket, <<>>),
    gen_tcp:close(Socket),
    Response.

reflect(Req) ->
    {~"value", Value} = lists:keyfind(~"value", 1, nova_req:parse_qs(Req)),
    {status, 200, #{~"x-reflected" => Value}, ~"reflected"}.

recv_until_closed(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} when is_binary(Data) -> recv_until_closed(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.
