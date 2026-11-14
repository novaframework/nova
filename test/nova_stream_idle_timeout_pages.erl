%%% The controllers of nova_stream_idle_timeout_SUITE.
-module(nova_stream_idle_timeout_pages).

-export([stream_with_idle_timeout/1, stream/1]).

stream_with_idle_timeout(_Req) ->
    {test_stream, infinity}.

stream(_Req) ->
    {test_stream, listener_default}.
