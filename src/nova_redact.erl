-module(nova_redact).
-moduledoc false.

-export([
         term/1,
         stacktrace/1,
         crash_info/3,
         tls_options/1
        ]).

-define(DEFAULT_FRAGMENTS, [~"passw", ~"secret", ~"token", ~"apikey", ~"authorization", ~"cookie",
                            ~"session", ~"csrf", ~"privatekey", ~"credential"]).
-define(DEFAULT_NAMES, [~"qs", ~"otp", ~"password", ~"sessionid", ~"setcookie", ~"csrftoken",
                        ~"xcsrftoken"]).
-define(MAX_KEY_SIZE, 64).

-type keys() :: #{pattern := binary:cp(), names := [binary()]}.

-doc """
Strips request data and secrets out of a term before it is logged or rendered.

A Cowboy request map anywhere inside `Term` is replaced by its method and
path, so crash reasons such as `{badmatch, Req}` or `{case_clause, {stop, Req}}`
never carry headers, cookies or a body into a log line or an error page.

In maps and in lists of `{Key, Value}` pairs (proplists, header and query
string lists), the value of a sensitive key is replaced by `redacted`. Keys
may be atoms, binaries or strings. A name is compared in lower case with `-`
and `_` removed, and is sensitive when it contains one of `passw`, `secret`,
`token`, `apikey`, `authorization`, `cookie`, `session`, `csrf`,
`privatekey`, `credential` or a key listed in `{nova, redact_keys}`, or when
it is `qs` or `otp`. That covers `access_token`, `X-Api-Key`, `clientSecret`
and `password_confirmation`. Only the first 64 bytes of a name are looked at.

A `{Key, Value}` tuple outside a list looks like an error tuple, so its value
is redacted only when the whole name is on that list, or is `password`,
`session_id`, `set-cookie`, `_csrf_token` or `x-csrf-token`: `{token, T}` is
redacted, `{invalid_token, Reason}` is not.
""".
-spec term(term()) -> term().
term(Term) ->
    term(Term, keys()).

-spec term(term(), keys()) -> term().
term(#{method := _, headers := Headers} = Req, _Keys) when is_map(Headers) ->
    (maps:with([method, path], Req))#{redacted => nova_req};
term(Map, Keys) when is_map(Map) ->
    maps:map(fun(Key, Value) -> member_value(Key, Value, Keys) end, Map);
term(List, Keys) when is_list(List) ->
    list(List, Keys);
term({Key, Value}, Keys) ->
    {term(Key, Keys), tuple_value(Key, Value, Keys)};
term(Tuple, Keys) when is_tuple(Tuple) ->
    list_to_tuple([term(Element, Keys) || Element <- tuple_to_list(Tuple)]);
term(Other, _Keys) ->
    Other.

list([{Key, Value} | Tail], Keys) ->
    [{term(Key, Keys), member_value(Key, Value, Keys)} | list(Tail, Keys)];
list([Head | Tail], Keys) ->
    [term(Head, Keys) | list(Tail, Keys)];
list([], _Keys) ->
    [];
list(Tail, Keys) ->
    term(Tail, Keys).

member_value(Key, Value, #{pattern := Pattern} = Keys) ->
    case key_name(Key) of
        undefined ->
            term(Value, Keys);
        Name ->
            case binary:match(Name, Pattern) of
                nomatch -> whole_name_value(Name, Value, Keys);
                _ -> redacted
            end
    end.

tuple_value(Key, Value, Keys) ->
    case key_name(Key) of
        undefined -> term(Value, Keys);
        Name -> whole_name_value(Name, Value, Keys)
    end.

whole_name_value(Name, Value, #{names := Names} = Keys) ->
    case lists:member(Name, Names) of
        true -> redacted;
        false -> term(Value, Keys)
    end.

-spec keys() -> keys().
keys() ->
    Fragments = ?DEFAULT_FRAGMENTS ++ configured_fragments(nova_config:get_env(redact_keys, [])),
    #{pattern => binary:compile_pattern(Fragments), names => ?DEFAULT_NAMES ++ Fragments}.

configured_fragments([Key | Rest]) ->
    case key_name(Key) of
        Name when is_binary(Name), Name =/= <<>> -> [Name | configured_fragments(Rest)];
        _ -> configured_fragments(Rest)
    end;
configured_fragments(_) ->
    [].

key_name(Key) when is_atom(Key) ->
    normalise(atom_to_binary(Key));
key_name(Key) when is_binary(Key) ->
    normalise(Key);
key_name(Key) when is_list(Key) ->
    try unicode:characters_to_binary(leading(Key, ?MAX_KEY_SIZE)) of
        Bin when is_binary(Bin) -> normalise(Bin);
        _ -> undefined
    catch
        _:_ -> undefined
    end;
key_name(_Key) ->
    undefined.

leading([Head | Tail], Count) when Count > 0 ->
    [Head | leading(Tail, Count - 1)];
leading(_Rest, _Count) ->
    [].

normalise(<<Name:?MAX_KEY_SIZE/binary, _/binary>>) ->
    normalise_bytes(Name);
normalise(Name) ->
    normalise_bytes(Name).

normalise_bytes(Name) ->
    << <<(lower(Char))>> || <<Char>> <= Name, Char =/= $-, Char =/= $_ >>.

lower(Char) when Char >= $A, Char =< $Z ->
    Char + 32;
lower(Char) ->
    Char.

-doc """
Replaces the argument list a stacktrace frame may carry with its arity.

The first frame of a `function_clause` (and of `erlang:error/2`) holds the
actual arguments, which for a controller is the whole request.
""".
-spec stacktrace(Stacktrace) -> Stacktrace when Stacktrace :: [term()].
stacktrace(Stacktrace) when is_list(Stacktrace) ->
    [frame(Frame) || Frame <- Stacktrace];
stacktrace(Other) ->
    Other.

frame({Module, Function, Args, Location}) when is_list(Args) ->
    {Module, Function, length(Args), Location};
frame(Frame) ->
    Frame.

-doc """
Builds the `crash_info` an error page is rendered from, with `Reason`
redacted. `Extra` carries the rest, such as `status_code` and an already
formatted `stacktrace`.
""".
-spec crash_info(Class :: atom(), Reason :: term(), Extra :: map()) -> map().
crash_info(Class, Reason, Extra) ->
    Extra#{class => Class, reason => term(Reason)}.

-doc """
Replaces what the TLS options of a listener keep secret, wherever they are
in `Term`: the key, its password, the keys of `certs_keys` and `sni_hosts`,
the seed of the session tickets and the like. For what a server says of a
listener it could not start, which may have the options it was given in it.
""".
-spec tls_options(term()) -> term().
tls_options({Option, Value}) ->
    case is_tls_secret(Option) of
        true -> {Option, redacted};
        false -> {tls_options(Option), tls_options(Value)}
    end;
tls_options(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([tls_options(Element) || Element <- tuple_to_list(Tuple)]);
tls_options(List) when is_list(List), length(List) >= 0 ->
    [tls_options(Element) || Element <- List];
tls_options(Map) when is_map(Map) ->
    #{Key => case is_tls_secret(Key) of
                 true -> redacted;
                 false -> tls_options(Value)
             end || Key := Value <- Map};
tls_options(Other) ->
    Other.

is_tls_secret(Option) ->
    lists:member(Option, [key, password, certs_keys, sni_hosts, sni_fun, user_lookup_fun, psk_identity,
                          srp_identity, stateless_tickets_seed]).
