-module(nova_json).
-moduledoc false.

-export([
         encode/1,
         decode/1
        ]).

-include_lib("kernel/include/logger.hrl").

-doc """
Encodes `Term` with the configured `json_lib`. With the default, OTP's
`json`, a non-empty list whose first element is a pair is encoded as an
object, so `[{a, 1}]` is `{"a":1}`.
""".
-spec encode(Term :: term()) -> iodata().
encode(Term) ->
    case json_lib() of
        json -> json:encode(Term, fun encode_value/2);
        Lib -> Lib:encode(Term)
    end.

-spec encode_value(term(), json:encoder()) -> iodata().
encode_value([{_, _} | _] = List, Encode) ->
    json:encode_key_value_list(pairs(List), Encode);
encode_value(Value, Encode) ->
    json:encode_value(Value, Encode).

-spec pairs([term()]) -> [{term(), term()}].
pairs([{_, _} = Pair | Rest]) -> [Pair | pairs(Rest)];
pairs([]) -> [];
pairs([Other | _]) -> error({unsupported_type, Other}).

-doc """
Decodes `Json` with the configured `json_lib` and returns `{ok, Term}` or
`{error, Reason}` whatever the library returns. Libraries that return the
term itself or raise on bad input, such as OTP's `json`, work as they are.
""".
-spec decode(Json :: iodata()) -> {ok, term()} | {error, term()}.
decode(Json) ->
    Lib = json_lib(),
    try Lib:decode(Json) of
        {ok, Term} -> {ok, Term};
        {error, Reason} -> {error, Reason};
        Term -> {ok, Term}
    catch
        Class:Reason ->
            ?LOG_DEBUG(#{msg => ~"JSON decode failed", json_lib => Lib, class => Class}),
            {error, Reason}
    end.

-doc """
The configured JSON library: `json_lib` from the bootstrap application's
environment, then from `nova`'s, and OTP's `json` when neither sets it.
""".
-spec json_lib() -> module().
json_lib() ->
    case nova_config:get_env(json_lib, json) of
        Lib when is_atom(Lib) -> Lib;
        _ -> json
    end.
