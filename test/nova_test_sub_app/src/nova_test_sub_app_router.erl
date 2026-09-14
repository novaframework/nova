-module(nova_test_sub_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
    [#{prefix => "",
       routes => [
                  {"/hello", fun nova_test_sub_app_controller:hello/1, #{methods => [get]}}
                 ]}].
