%%% A plain cowboy handler (no sub-protocol), routed through nova with
%%% protocol => cowboy.
-module(test_plain_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"plain-ok">>, Req0),
    {ok, Req, State}.
