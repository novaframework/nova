-module(nova_handler).

-export([
         init/2
        ]).

-include_lib("nova/include/nova.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(Req, HandlerOpts) ->
    Handler = maps:get(nova_handler, HandlerOpts, nova_http_handler),
    try
        Handler:execute(Req, HandlerOpts)
    catch ?WITH_STACKTRACE(T, R, S)
        logger:error("Handler ~p crashed with type ~p, reason ~p", [Handler, T, R]),
        erlang:raise(T, R, S)
    end.
