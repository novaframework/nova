-module(nova_redact).
-moduledoc false.

-export([
         term/1,
         stacktrace/1,
         crash_info/3
        ]).

-define(DEFAULT_KEYS, [~"authorization", ~"cookie", ~"set-cookie", ~"password", ~"token",
                       ~"secret", ~"_csrf_token", ~"x-csrf-token", ~"session_id", ~"qs"]).
-define(MAX_KEY_SIZE, 64).

-doc """
Strips request data and secrets out of a term before it is logged or rendered.

A Cowboy request map anywhere inside `Term` is replaced by its method and
path, so crash reasons such as `{badmatch, Req}` or `{case_clause, {stop, Req}}`
never carry headers, cookies or a body into a log line or an error page.

In maps and in `{Key, Value}` pairs (proplists, header and query string
lists), the value of a sensitive key is replaced by `redacted`. Keys may be
atoms, binaries or strings and match case-insensitively: `authorization`,
`cookie`, `set-cookie`, `password`, `token`, `secret`, `_csrf_token`,
`x-csrf-token`, `session_id`, `qs`, plus any listed in `{nova, redact_keys}`.
""".
-spec term(term()) -> term().
term(Term) ->
    term(Term, redact_keys()).

term(#{pid := Pid, streamid := _, headers := _} = Req, _Keys) when is_pid(Pid) ->
    (maps:with([method, path], Req))#{redacted => cowboy_req};
term(Map, Keys) when is_map(Map) ->
    maps:map(fun(Key, Value) -> pair_value(Key, Value, Keys) end, Map);
term(List, Keys) when is_list(List) ->
    list(List, Keys);
term({Key, Value}, Keys) ->
    {term(Key, Keys), pair_value(Key, Value, Keys)};
term(Tuple, Keys) when is_tuple(Tuple) ->
    list_to_tuple([term(Element, Keys) || Element <- tuple_to_list(Tuple)]);
term(Other, _Keys) ->
    Other.

list([Head | Tail], Keys) ->
    [term(Head, Keys) | list(Tail, Keys)];
list([], _Keys) ->
    [];
list(Tail, Keys) ->
    term(Tail, Keys).

pair_value(Key, Value, Keys) ->
    case lists:member(key_name(Key), Keys) of
        true -> redacted;
        false -> term(Value, Keys)
    end.

redact_keys() ->
    Extra = case nova_config:get_env(redact_keys, []) of
                List when is_list(List) -> [key_name(Key) || Key <- List];
                _ -> []
            end,
    ?DEFAULT_KEYS ++ [Key || Key <- Extra, is_binary(Key)].

key_name(Key) when is_atom(Key) ->
    key_name(atom_to_binary(Key));
key_name(Key) when is_binary(Key), byte_size(Key) =< ?MAX_KEY_SIZE ->
    string:lowercase(Key);
key_name(Key) when is_list(Key), length(Key) =< ?MAX_KEY_SIZE ->
    try unicode:characters_to_binary(Key) of
        Bin when is_binary(Bin) -> string:lowercase(Bin);
        _ -> undefined
    catch
        _:_ -> undefined
    end;
key_name(_Key) ->
    undefined.

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
