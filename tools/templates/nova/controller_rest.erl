-module({{name}}_main_controller).
-export([
         index/1
        ]).

index(#{req := #{method := <<"GET">>}} = _NovaReq) ->
    {json, [{message, "Nova is running!"}]}.
