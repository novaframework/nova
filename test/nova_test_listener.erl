-module(nova_test_listener).

-export([free_port/0]).

%% A port nothing listens on, from below the range the kernel takes the
%% ports of outgoing connections and of a listen on port 0 from. A port out
%% of that range is given to another process between this call and the
%% listener that binds it, when several suites run on the machine.
-spec free_port() -> inet:port_number().
free_port() ->
    free_port(50).

free_port(Attempts) when Attempts > 0 ->
    Port = 20000 + rand:uniform(9999),
    case gen_tcp:listen(Port, [{reuseaddr, true}]) of
        {ok, Socket} ->
            ok = gen_tcp:close(Socket),
            Port;
        {error, eaddrinuse} ->
            free_port(Attempts - 1)
    end.
