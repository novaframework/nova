%%% The controllers of nova_streamed_response_SUITE.
-module(nova_streamed_response_pages).

-export([handler/1, controller/1, crash/1, plain/1]).

handler(_Req) ->
    {test_streamed, finish}.

controller(Req0) ->
    Req = nova_req:stream_reply(200, #{~"content-type" => ~"text/plain"}, Req0),
    ok = nova_req:stream_body(~"first-chunk", nofin, Req),
    ok = nova_req:stream_body(~"last-chunk", fin, Req),
    {status, 200, #{}, <<>>, Req}.

crash(_Req) ->
    {test_streamed, crash}.

plain(_Req) ->
    {status, 200, #{}, ~"still-here"}.
