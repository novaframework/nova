-module(nova_error_controller).
-moduledoc false.
-export([
         not_found/1,
         server_error/1,
	 status_code/1
        ]).

-ifdef(TEST).
-export([format_arity/1, format_stacktrace/1, response_type/1]).
-endif.


status_code(Req) ->
    {status, maps:get(resp_status_code, Req, 200)}.

not_found(Req) ->
    page(404,
         #{message => ~"Resource not found"},
         #{status => "Could not find the page you were looking for",
           title => "404 Not found",
           message => "We could not find the page you were looking for"},
         Req).

server_error(#{crash_info := #{status_code := StatusCode} = CrashInfo} = Req) ->
    Variables = #{status => maps:get(status, CrashInfo, undefined),
                  title => maps:get(title, CrashInfo, undefined),
                  message => maps:get(message, CrashInfo, undefined),
                  extra_msg => maps:get(extra_msg, CrashInfo, undefined),
                  stacktrace => format_stacktrace(maps:get(stacktrace, CrashInfo, []))},
    render(StatusCode, Variables, Req);
server_error(#{crash_info := #{class := Class, reason := Reason}} = Req) ->
    Stacktrace = maps:get(stacktrace, Req, []),
    Variables = #{status => "Internal Server Error",
                  title => "500 Internal Server Error",
                  message => "Something internal crashed. Please take a look!",
                  extra_msg => io_lib:format("Class: ~p<br /> Reason: ~p", [Class, Reason]),
                  stacktrace => format_stacktrace(Stacktrace)},

    case nova:is_dev_environment() of
        true ->
            render(500, Variables, Req);
        false ->
            {status, 500}
    end.

render(StatusCode, Variables, Req) ->
    page(StatusCode, Variables, Variables, Req).

page(StatusCode, Json, Variables, Req) ->
    case application:get_env(nova, render_error_pages, true) =:= true andalso response_type(Req) of
        false ->
            {status, StatusCode};
        json ->
            {status, StatusCode, #{~"content-type" => ~"application/json"}, nova_json:encode(Json)};
        html ->
            {ok, Body} = nova_basic_handler:render_dtl(nova_error_dtl, Variables, []),
            {status, StatusCode, #{~"content-type" => ~"text/html"}, Body};
        none ->
            {status, StatusCode}
    end.

response_type(Req) ->
    try cowboy_req:parse_header(~"accept", Req) of
        undefined -> json;
        Ranges when is_list(Ranges) -> preferred_type(lists:enumerate(Ranges))
    catch
        exit:{request_error, {header, ~"accept"}, _Message} -> json
    end.

preferred_type(Ranges) ->
    case {quality(json, Ranges), quality(html, Ranges)} of
        {{0, _}, {0, _}} -> none;
        {{JsonQuality, _}, {HtmlQuality, _}} when JsonQuality > HtmlQuality -> json;
        {{Quality, JsonAt}, {Quality, HtmlAt}} when JsonAt =< HtmlAt -> json;
        _ -> html
    end.

quality(Type, Ranges) ->
    case [{Rank, At, Quality} || {At, {Range, Quality, _Extensions}} <- Ranges, Rank <- rank(Type, Range)] of
        [] ->
            {0, 0};
        Covering ->
            {_Rank, At, Quality} = lists:min(Covering),
            {Quality, At}
    end.

rank(json, {~"application", ~"json", _Parameters}) -> [1];
rank(json, {~"application", ~"*", _Parameters}) -> [2];
rank(html, {~"text", ~"html", _Parameters}) -> [1];
rank(html, {~"text", ~"*", _Parameters}) -> [2];
rank(_Type, {~"*", ~"*", _Parameters}) -> [3];
rank(_Type, _Range) -> [].

format_stacktrace(not_enabled) ->
    logger:warning(<<"Stacktrace disabled. If you want to enable stacktraces call nova:stracktrace(true) or update your sys.config - Read more in the docs">>),
    [];
format_stacktrace([]) -> [];
format_stacktrace([{Mod, Func, Arity, TraceOpts}|Tl]) ->
    File = case proplists:get_value(file, TraceOpts) of
                undefined -> undefined;
                F -> list_to_binary(F)
           end,
    Line = proplists:get_value(line, TraceOpts),
    Formated = #{module => erlang:atom_to_binary(Mod, utf8),
                 function => erlang:atom_to_binary(Func, utf8),
                 arity => format_arity(Arity, []),
                 file => File,
                 line => Line},
    [Formated|format_stacktrace(Tl)];
format_stacktrace([Hd|Tl]) ->
    logger:warning(<<"Could not format stacktrace line: ~p">>, [Hd]),
    format_stacktrace(Tl).



format_arity(Arity) when is_pid(Arity) -> list_to_binary(pid_to_list(Arity));
format_arity(Arity) when is_function(Arity) -> <<"fun">>;
format_arity(Arity) -> Arity.

format_arity([], Acc) ->
    Acc;
format_arity([Head, Tail], Acc) ->
    Formated = format_arity(Head),
    format_arity(Tail, [Formated | Acc]);
format_arity(Arity, _) when is_function(Arity)->
    <<"fun">>;
format_arity(Arity, _) ->
    Arity.
