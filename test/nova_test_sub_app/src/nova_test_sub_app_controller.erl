-module(nova_test_sub_app_controller).

-export([hello/1]).

hello(Req) ->
    {json, #{app => <<"nova_test_sub_app">>, route => maps:get(nova_route, Req, null)}}.
