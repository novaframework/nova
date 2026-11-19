%%% The controllers of nova_csrf_SUITE.
-module(nova_csrf_pages).

-export([token/1, submit/1, write/1, sign_in/1, sign_in_in_one_call/1, who/1, form/1, secure/1]).

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

sign_in_in_one_call(Req) ->
    {ok, _SignedIn} = nova_session:sign_in(Req, ~"user", ~"alice"),
    {status, 200, #{}, ~"signed in"}.

who(Req) ->
    case nova_session:get(Req, ~"user") of
        {ok, User} -> {status, 200, #{}, User};
        {error, _NobodySignedIn} -> {status, 200, #{}, ~"anonymous"}
    end.

form(_Req) ->
    {ok, #{}}.

secure(_Req) ->
    nova_test_observer:observed(nova_csrf_observer, security),
    true.
