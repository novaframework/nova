%%% Router fixture for nova_cowboy_handler_SUITE.
-module(base_app_router).

-export([routes/1, hello/1]).

routes(_Env) ->
    [#{prefix => "",
       security => false,
       routes => [
                  {"/base", fun base_app_router:hello/1, #{methods => [get]}},
                  {"/plain", test_plain_handler, #{protocol => cowboy, methods => [get]}},
                  {"/loop", test_loop_handler, #{protocol => cowboy}},
                  {"/loop-args", test_loop_handler, #{protocol => cowboy,
                                                      arguments => #{body => <<"loop-args-ok">>}}},
                  {"/stream", test_stream_handler, #{protocol => cowboy}}
                 ]}].

hello(_Req) ->
    {json, #{app => <<"base">>}}.
