%%% A plain cowboy_loop handler, routed through nova with protocol => cowboy.
-module(test_loop_handler).
-behaviour(cowboy_loop).

-export([init/2, info/3]).

init(Req, State) ->
    erlang:send_after(50, self(), reply),
    {cowboy_loop, Req, State}.

info(reply, Req, State) ->
    Body = maps:get(body, State, <<"loop-ok">>),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Body, Req),
    {stop, Req1, State}.
