%%% The controllers of nova_session_race_SUITE.
-module(nova_session_race_pages).

-export([sign_in/1, sign_out/1, held_write/1, who/1]).

sign_in(Req) ->
    {ok, _SignedIn} = nova_session:sign_in(Req, ~"user", ~"alice"),
    {status, 200, #{}, ~"signed in"}.

sign_out(Req) ->
    {ok, _SignedOut} = nova_session:delete(Req),
    {status, 200, #{}, ~"signed out"}.

%% In flight until the test case says `write`. With `?read` it has read its
%% session by then.
held_write(Req) ->
    _ = nova_req:qs(Req) =:= ~"read" andalso nova_session:get(Req, ~"user"),
    nova_test_observer:observed(nova_session_race_observer, {held, self()}),
    receive
        write -> ok
    after 10000 ->
        exit(not_released)
    end,
    ok = nova_session:set(Req, ~"note", ~"written after the session ended"),
    {status, 200, #{}, ~"written"}.

who(Req) ->
    case nova_session:get(Req, ~"user") of
        {ok, User} -> {status, 200, #{}, User};
        {error, _NobodySignedIn} -> {status, 200, #{}, ~"anonymous"}
    end.
