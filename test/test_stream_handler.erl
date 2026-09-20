%%% A cowboy_loop handler streaming its response with
%%% cowboy_req:stream_reply/3 and cowboy_req:stream_body/3.
-module(test_stream_handler).
-behaviour(cowboy_loop).

-export([init/2, info/3]).

init(Req0, State) ->
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/plain">>}, Req0),
    self() ! {chunk, 1},
    {cowboy_loop, Req, State}.

info({chunk, N}, Req, State) when N =< 3 ->
    ok = cowboy_req:stream_body(io_lib:format("chunk~B", [N]), nofin, Req),
    erlang:send_after(20, self(), {chunk, N + 1}),
    {ok, Req, State};
info({chunk, _}, Req, State) ->
    ok = cowboy_req:stream_body(<<"done">>, fin, Req),
    {stop, Req, State}.
