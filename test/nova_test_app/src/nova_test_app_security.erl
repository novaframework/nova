%%% Security callback for the /secure prefix. Accepts a fixed bearer token and
%%% hands back auth data so the suite can check it reaches the controller.
-module(nova_test_app_security).

-export([check/1]).

check(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer let-me-in">> ->
            {true, #{user => <<"tester">>, role => <<"admin">>}};
        _ ->
            false
    end.
