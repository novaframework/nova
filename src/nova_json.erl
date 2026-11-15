-module(nova_json).
-moduledoc false.

-export([
         encode/1,
         decode/1,
         try_decode/1
        ]).

-export_type([why/0]).

-doc """
What may be logged about an input that did not decode: the kind of error,
and where in the input it is when OTP's `json` found it. It has no byte of
the input.
""".
-type why() :: #{error := atom(), position => non_neg_integer()}.

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
    case try_decode(Json) of
        {error, Reason, _Why} -> {error, Reason};
        Decoded -> Decoded
    end.

-doc """
As `decode/1`, with what may be logged about an input that does not decode
next to the reason. The reason of a library can hold bytes of the input, so
it is for the caller and not for a log.
""".
-spec try_decode(Json :: iodata()) -> {ok, term()} | {error, Reason :: term(), why()}.
try_decode(Json) ->
    Lib = json_lib(),
    try Lib:decode(Json) of
        {ok, Term} -> {ok, Term};
        {error, Reason} -> {error, Reason, #{error => kind(Reason)}};
        Term -> {ok, Term}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_DEBUG(#{msg => ~"JSON decode failed", json_lib => Lib, class => Class}),
            {error, Reason, maps:merge(#{error => kind(Reason)}, position(Stacktrace))}
    end.

%% The name a library gives an error, which is an atom or the first element
%% of a tuple. Anything else may be the input itself.
-spec kind(Reason :: term()) -> atom().
kind(Reason) when is_atom(Reason) ->
    Reason;
kind(Reason) when is_tuple(Reason), tuple_size(Reason) > 0, is_atom(element(1, Reason)) ->
    element(1, Reason);
kind(_Reason) ->
    invalid_json.

%% OTP's json says where it stopped in the error info of the exception.
-spec position(Stacktrace :: [tuple()]) -> #{position => non_neg_integer()}.
position([{_Module, _Function, _Arguments, Info} | _]) when is_list(Info) ->
    case proplists:get_value(error_info, Info) of
        #{cause := #{position := Position}} when is_integer(Position), Position >= 0 -> #{position => Position};
        _NoPosition -> #{}
    end;
position(_Stacktrace) ->
    #{}.

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
