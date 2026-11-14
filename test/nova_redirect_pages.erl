%%% The controller of nova_redirect_SUITE.
-module(nova_redirect_pages).

-export([back/1]).

back(Req) ->
    #{next := Next} = cowboy_req:match_qs([next], Req),
    {redirect_local, Next}.
