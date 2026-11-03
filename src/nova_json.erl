-module(nova_json).
-moduledoc false.

-export([
         encode/1,
         decode/1
        ]).

-include_lib("kernel/include/logger.hrl").

-doc "Encodes `Term` with the configured `json_lib`.".
-spec encode(Term :: term()) -> iodata().
encode(Term) ->
    Lib = json_lib(),
    Lib:encode(Term).

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
environment, then from `nova`'s, and `thoas` when neither sets it.
""".
-spec json_lib() -> module().
json_lib() ->
    case nova_config:get_env(json_lib, thoas) of
        Lib when is_atom(Lib) -> Lib;
        _ -> thoas
    end.
