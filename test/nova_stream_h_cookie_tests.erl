-module(nova_stream_h_cookie_tests).
-include_lib("eunit/include/eunit.hrl").

-export([init/2, init/3, log/2]).

-define(TOKEN, "TOPSECRETTOKEN").

%% A Cookie header that cowlib cannot parse used to crash nova_stream_h:init/3,
%% and Cowboy logged the whole request, Authorization included. Which bytes
%% cowlib refuses depends on its version, so every case checks what holds
%% either way: init/3 does not raise, the client's session_id cookie is not
%% overwritten, and a request whose header did not parse still gets a
%% session id.
malformed_cookie_header_test_() ->
    [{Name, fun() -> assert_session_cookie_kept(Cookie) end}
     || {Name, Cookie} <- [{"=x", ~"session_id=abc; =x"},
                           {"tab", ~"session_id=abc; a\tb=c"},
                           {"0x01", <<"session_id=abc; a=b", 1, "c">>},
                           {"0x7F", <<"session_id=abc; a=b", 127, "c">>},
                           {"101 cookies", too_many_cookies()}]].

%% These two fail to parse with every cowlib Nova has used, so the
%% unparsable path above is always exercised.
unparsable_cookie_header_test_() ->
    [{Name, ?_assertExit({request_error, _, _}, cowboy_req:parse_cookies(req(Cookie)))}
     || {Name, Cookie} <- [{"=x", ~"session_id=abc; =x"},
                           {"101 cookies", too_many_cookies()}]].

assert_session_cookie_kept(Cookie) ->
    {[{req, Req}], _} = nova_stream_h:init(1, req(Cookie), #{stream_handlers => [?MODULE]}),
    ?assertNot(is_map_key(~"session_id", maps:get(resp_cookies, Req, #{}))),
    try cowboy_req:parse_cookies(req(Cookie)) of
        _ -> ok
    catch
        exit:{request_error, _, _} -> ?assertMatch(#{nova_session_id := _}, Req)
    end.

malformed_cookie_header_over_a_socket_test_() ->
    {setup, fun start_listener/0, fun stop_listener/1,
     fun() ->
             ok = logger:add_handler(?MODULE, ?MODULE, #{config => #{test_pid => self()}}),
             try
                 {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, port(), [binary, {active, false}]),
                 ok = gen_tcp:send(Socket, [~"GET / HTTP/1.1\r\nhost: localhost\r\n",
                                            <<"authorization: Bearer " ?TOKEN "\r\n">>,
                                            ~"cookie: =x\r\nconnection: close\r\n\r\n"]),
                 Response = recv_until_closed(Socket, <<>>),
                 gen_tcp:close(Socket),
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

-spec too_many_cookies() -> binary().
too_many_cookies() ->
    iolist_to_binary(lists:join(~"; ", [~"session_id=abc" |
                                        [[~"c", integer_to_binary(N), ~"=v"] || N <- lists:seq(1, 100)]])).

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
    _ = [application:stop(App) || App <- lists:reverse(Started)],
    ok.

-spec port() -> inet:port_number().
port() ->
    case ranch:get_port(?MODULE) of
        Port when is_integer(Port) -> Port
    end.

init(Req, State) ->
    {ok, cowboy_req:reply(200, #{}, ~"ok", Req), State}.

%% As the next stream handler, hands back the request nova_stream_h passed on.
init(_StreamID, Req, _Opts) ->
    {[{req, Req}], undefined}.

%% The connection process closes the socket only after it is done with the
%% request, so anything it logged has arrived by then.
recv_until_closed(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} -> recv_until_closed(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} -> Acc
    end.

log(Event, #{config := #{test_pid := Pid}}) ->
    Pid ! {?MODULE, Event}.

logged() ->
    receive
        {?MODULE, Event} -> [Event | logged()]
    after 0 ->
            []
    end.
