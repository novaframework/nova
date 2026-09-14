-module(nova_test_sub_app_controller).

-export([hello/1]).

hello(_Req) ->
    {json, #{app => <<"nova_test_sub_app">>}}.
