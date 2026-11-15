%%% The controllers of nova_csrf_SUITE.
-module(nova_csrf_pages).

-export([token/1, submit/1, write/1, sign_in/1, form/1, secure/1]).

token(Req) ->
    {status, 200, #{}, maps:get(csrf_token, Req, ~"no token")}.

submit(_Req) ->
    {status, 200, #{}, ~"submitted"}.

write(Req) ->
    ok = nova_session:set(Req, ~"basket", ~"1"),
    {status, 200, #{}, ~"written"}.

sign_in(Req) ->
    {ok, SignedIn} = nova_session:renew(Req),
    ok = nova_session:set(SignedIn, ~"user", ~"1"),
    {status, 200, #{}, ~"signed in"}.

form(_Req) ->
    {ok, #{}}.

secure(_Req) ->
    nova_test_observer:observed(nova_csrf_observer, security),
    true.
