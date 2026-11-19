-module(nova_redact).
-moduledoc false.

-export([tls_options/1, stacktrace/1]).

-doc """
Replaces what the TLS options of a listener keep secret, wherever they are
in `Term`: the key, its password, the keys of `certs_keys` and `sni_hosts`,
the seed of the session tickets and the like. Cowboy returns and raises the
options it was given when it cannot start a listener.
""".
-spec tls_options(term()) -> term().
tls_options({Option, Value}) ->
    case is_tls_secret(Option) of
        true -> {Option, redacted};
        false -> {tls_options(Option), tls_options(Value)}
    end;
tls_options(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([tls_options(Element) || Element <- tuple_to_list(Tuple)]);
tls_options(List) when is_list(List) ->
    tls_list(List);
tls_options(Map) when is_map(Map) ->
    #{Key => case is_tls_secret(Key) of
                 true -> redacted;
                 false -> tls_options(Value)
             end || Key := Value <- Map};
tls_options(Other) ->
    Other.

tls_list([Head | Tail]) ->
    [tls_options(Head) | tls_list(Tail)];
tls_list([]) ->
    [];
tls_list(Tail) ->
    tls_options(Tail).

-doc "Replaces the argument list a stacktrace frame may carry with its arity.".
-spec stacktrace(erlang:stacktrace()) -> erlang:stacktrace().
stacktrace(Stacktrace) ->
    [frame(Frame) || Frame <- Stacktrace].

frame({Module, Function, Args, Location}) when is_list(Args) ->
    {Module, Function, length(Args), Location};
frame(Frame) ->
    Frame.

is_tls_secret(Option) ->
    lists:member(Option, [key, password, certs_keys, sni_hosts, sni_fun, user_lookup_fun, psk_identity,
                          srp_identity, stateless_tickets_seed]).
