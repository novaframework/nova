-module({{name}}_main_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    {ok, [{message, "Hello World!"}]}.
