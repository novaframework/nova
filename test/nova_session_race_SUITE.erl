%%% A session that was ended stays ended. A request that was on its way when
%%% its client signed out, or signed in again, writes to the session after
%%% that: what it writes must not be stored under the id that was given up,
%%% which somebody else may hold. The request is held in its controller
%%% until the other one is answered, so the order is the same in every run.
-module(nova_session_race_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([request_in_flight_does_not_bring_back_a_session_that_was_signed_out/1,
         request_in_flight_does_not_bring_back_a_session_that_was_renewed/1,
         request_in_flight_does_not_start_a_session_under_an_id_that_was_renewed_away/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(SECRET, ~"a secret of thirty-two bytes or more, for the tests").
-define(PAGES, nova_session_race_pages).

all() ->
    [request_in_flight_does_not_bring_back_a_session_that_was_signed_out,
     request_in_flight_does_not_bring_back_a_session_that_was_renewed,
     request_in_flight_does_not_start_a_session_under_an_id_that_was_renewed_away].

init_per_suite(Config) ->
    Routes = [#{prefix => "/race",
                plugins => [],
                routes => [{"/sign-in", fun ?PAGES:sign_in/1, #{methods => [post]}},
                           {"/sign-out", fun ?PAGES:sign_out/1, #{methods => [post]}},
                           {"/held-write", fun ?PAGES:held_write/1, #{methods => [post]}},
                           {"/who", fun ?PAGES:who/1, #{methods => [get]}}]}],
    Port = nova_test_listener:start(#{host => Routes}),
    nova_test_settings:set(secret_key_base, ?SECRET),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_settings:unset(secret_key_base),
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    register(nova_session_race_observer, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    unregister(nova_session_race_observer),
    ok.

request_in_flight_does_not_bring_back_a_session_that_was_signed_out(Config) ->
    [begin
         {SignedIn, Id} = sign_in(Config, []),
         InFlight = held_write(Config, Path, SignedIn),
         {200, Headers, ~"signed out"} = post(Config, "/race/sign-out", [SignedIn]),
         ?assertMatch(<<"__Host-session_id=;", _/binary>>, maps:get(~"set-cookie", Headers)),
         {_Other, OtherId} = written(InFlight),
         ?assertNotEqual(Id, OtherId),
         ?assertEqual({Path, false}, {Path, nova_session_ets:session_exists(Id)}),
         ?assertEqual({error, not_found}, nova_session_ets:get_value(Id, ~"note")),
         ?assertMatch({ok, _Note}, nova_session_ets:get_value(OtherId, ~"note")),
         ?assertEqual({200, ~"anonymous"}, who(Config, SignedIn))
     end || Path <- ["/race/held-write", "/race/held-write?read"]].

request_in_flight_does_not_bring_back_a_session_that_was_renewed(Config) ->
    [begin
         {SignedIn, Id} = sign_in(Config, []),
         InFlight = held_write(Config, Path, SignedIn),
         {Again, AgainId} = sign_in(Config, [SignedIn]),
         {_Other, OtherId} = written(InFlight),
         ?assertEqual(3, length(lists:usort([Id, AgainId, OtherId]))),
         ?assertEqual({Path, false}, {Path, nova_session_ets:session_exists(Id)}),
         ?assertEqual({error, not_found}, nova_session_ets:get_value(AgainId, ~"note")),
         ?assertMatch({ok, _Note}, nova_session_ets:get_value(OtherId, ~"note")),
         ?assertEqual({200, ~"anonymous"}, who(Config, SignedIn)),
         ?assertEqual({200, ~"alice"}, who(Config, Again))
     end || Path <- ["/race/held-write", "/race/held-write?read"]].

%% The id is one a visitor was handed with a page, which no session is
%% stored under, and which somebody may have planted. Signing in moves the
%% client off it, and a request that still carries it starts no session
%% under it afterwards.
request_in_flight_does_not_start_a_session_under_an_id_that_was_renewed_away(Config) ->
    {ok, HandedOut} = nova_session:generate_session_id(),
    Visitor = {"cookie", ["__Host-session_id=", HandedOut]},
    InFlight = held_write(Config, "/race/held-write", Visitor),
    {_SignedIn, Id} = sign_in(Config, [Visitor]),
    {_Other, OtherId} = written(InFlight),
    ?assertEqual(3, length(lists:usort([HandedOut, Id, OtherId]))),
    ?assertNot(nova_session_ets:session_exists(HandedOut)),
    ?assertMatch({ok, _Note}, nova_session_ets:get_value(OtherId, ~"note")),
    ?assertEqual({error, not_found}, nova_session_ets:get_value(Id, ~"note")).

%% Sends the request from another process and returns once its controller
%% is waiting to be told to write.
held_write(Config, Path, Cookie) ->
    Test = self(),
    Client = spawn_link(fun() -> Test ! {self(), post(Config, Path, [Cookie])} end),
    receive
        {observed, {held, Controller}} -> {Client, Controller}
    after 5000 ->
        error(not_held)
    end.

%% Lets the held request write, and returns the cookie its response set.
written({Client, Controller}) ->
    Controller ! write,
    receive
        {Client, {200, Headers, ~"written"}} -> cookie(Headers)
    after 5000 ->
        error(not_written)
    end.

sign_in(Config, Headers) ->
    {200, ResponseHeaders, ~"signed in"} = post(Config, "/race/sign-in", Headers),
    cookie(ResponseHeaders).

who(Config, Cookie) ->
    nova_test_listener:get(?config(port, Config), "/race/who", [Cookie]).

post(Config, Path, Headers) ->
    nova_test_listener:response(?config(port, Config), "POST", Path, Headers, <<>>).

cookie(Headers) ->
    [<<"__Host-session_id=", Id/binary>> = SessionCookie | _Attributes] = binary:split(maps:get(~"set-cookie", Headers), ~";"),
    {{"cookie", SessionCookie}, Id}.
