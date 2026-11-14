%%% The controllers of nova_cors_SUITE.
-module(nova_cors_pages).

-export([page/1]).

page(#{method := Method}) ->
    {status, 200, #{}, <<"controller saw ", Method/binary>>}.
