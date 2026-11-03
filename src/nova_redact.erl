-module(nova_redact).
-moduledoc false.

-export([
         term/1,
         stacktrace/1
        ]).

-doc """
Strips request data out of a term before it is logged or rendered.

A Cowboy request map anywhere inside `Term` is replaced by its method and
path, so crash reasons such as `{badmatch, Req}` or `{case_clause, {stop, Req}}`
never carry headers, cookies or a body into a log line or an error page.
""".
-spec term(Term) -> Term when Term :: term().
term(#{pid := Pid, streamid := _, headers := _} = Req) when is_pid(Pid) ->
    (maps:with([method, path], Req))#{redacted => cowboy_req};
term(Map) when is_map(Map) ->
    maps:map(fun(_Key, Value) -> term(Value) end, Map);
term(List) when is_list(List) ->
    try lists:map(fun term/1, List)
    catch
        error:function_clause -> List
    end;
term(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([term(Element) || Element <- tuple_to_list(Tuple)]);
term(Other) ->
    Other.

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
