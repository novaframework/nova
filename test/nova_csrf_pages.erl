%%% The controllers of nova_csrf_SUITE.
-module(nova_csrf_pages).

-export([token/1, submit/1, form/1]).

token(Req) ->
    {status, 200, #{}, maps:get(csrf_token, Req, ~"no token")}.

submit(_Req) ->
    {status, 200, #{}, ~"submitted"}.

form(_Req) ->
    {ok, #{}}.
