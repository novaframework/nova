%%% The controllers of nova_session_cap_SUITE.
-module(nova_session_cap_pages).

-export([sign_in/1, who/1]).

sign_in(Req) ->
    {ok, Req1} = nova_session:renew(Req),
    #{~"user" := User} = maps:from_list(nova_req:parse_qs(Req)),
    case nova_session:set(Req1, ~"user", User) of
        ok -> {status, 200, #{}, User};
        {error, Reason} -> {status, 503, #{}, atom_to_binary(Reason)}
    end.

who(Req) ->
    case nova_session:get(Req, ~"user") of
        {ok, User} -> {status, 200, #{}, User};
        {error, _Reason} -> {status, 200, #{}, ~"nobody"}
    end.
