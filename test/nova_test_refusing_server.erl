%%% A server that takes no listener with TLS, and says so with the listener.
-module(nova_test_refusing_server).

-export([capabilities/0, check_listener/1, start_listener/2, stop_listener/1]).

capabilities() ->
    #{protocols => [http1, http2], cleartext => [http1], websocket => [http1]}.

check_listener(#{tls := false}) -> ok;
check_listener(Listener) -> {error, {refused, Listener}}.

start_listener(_Listener, _Env) ->
    {ok, self()}.

stop_listener(_Name) ->
    ok.
