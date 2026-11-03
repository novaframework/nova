-module(nova_stream_h_cookie_tests).
-include_lib("eunit/include/eunit.hrl").

-export([init/2, log/2]).

-define(TOKEN, "TOPSECRETTOKEN").

%% A Cookie header that cowlib cannot parse used to crash nova_stream_h:init/3,
%% and Cowboy logged the whole request, Authorization included.
malformed_cookie_header_does_not_raise_test_() ->
    [{Name, fun() ->
                    ?assertMatch({_, _}, nova_stream_h:init(1, req(Cookie), #{stream_handlers => []}))
            end}
     || {Name, Cookie} <- [{"=x", ~"=x"},
                           {"tab", <<"a\tb=c">>},
                           {"0x01", <<"a=b", 1, "c">>},
                           {"0x7F", <<"a=b", 127, "c">>}]].

malformed_cookie_header_over_a_socket_test_() ->
    {setup, fun start_listener/0, fun stop_listener/1,
     fun() ->
             ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
             try
                 {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, port(), [binary, {active, false}]),
                 ok = gen_tcp:send(Socket, [~"GET / HTTP/1.1\r\nhost: localhost\r\n",
                                            <<"authorization: Bearer " ?TOKEN "\r\n">>,
                                            ~"cookie: =x\r\nconnection: close\r\n\r\n"]),
                 {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
                 gen_tcp:close(Socket),
                 timer:sleep(200),
                 ?assertMatch(<<"HTTP/1.1 200", _/binary>>, Response),
                 ?assertEqual([], [Event || Event <- logged(),
                                            string:find(io_lib:format("~p", [Event]), ?TOKEN) =/= nomatch])
             after
                 logger:remove_handler(?MODULE)
             end
     end}.

%% Only the headers matter to nova_stream_h:init/3 here.
-spec req(binary()) -> dynamic().
req(Cookie) ->
    #{headers => #{~"cookie" => Cookie, ~"authorization" => <<"Bearer " ?TOKEN>>}}.

-spec start_listener() -> [atom()].
start_listener() ->
    {ok, Started} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, []}]}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 0}],
                                 #{env => #{dispatch => Dispatch},
                                   stream_handlers => [nova_stream_h, cowboy_stream_h]}),
    Started.

-spec stop_listener([atom()]) -> ok.
stop_listener(Started) ->
    ok = cowboy:stop_listener(?MODULE),
    _ = [application:stop(App) || App <- Started],
    ok.

-spec port() -> inet:port_number().
port() ->
    case ranch:get_port(?MODULE) of
        Port when is_integer(Port) -> Port
    end.

init(Req, State) ->
    {ok, cowboy_req:reply(200, #{}, ~"ok", Req), State}.

log(Event, #{config := #{test_pid := Pid}}) ->
    Pid ! {?MODULE, Event}.

logged() ->
    receive
        {?MODULE, Event} -> [Event | logged()]
    after 0 ->
            []
    end.
