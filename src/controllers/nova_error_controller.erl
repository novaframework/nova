-module(nova_error_controller).
-moduledoc false.
-export([
         not_found/1,
         server_error/1,
	 status_code/1
        ]).


status_code(Req) ->
    {status, maps:get(resp_status_code, Req, 200)}.

not_found(Req) ->
    Type = case cowboy_req:header(~"accept", Req) of
               undefined -> json;
               _ -> preferred_type(Req)
           end,
    case Type of
        json ->
            Json = nova_json:encode(#{message => ~"Resource not found"}),
            {status, 404, #{<<"content-type">> => <<"application/json">>}, Json};
        html ->
            Variables = #{status => "Could not find the page you were looking for",
                          title => "404 Not found",
                          message => "We could not find the page you were looking for"},
            {ok, Body} = nova_error_dtl:render(Variables),
            {status, 404, #{<<"content-type">> => <<"text/html">>}, Body};
        none ->
            {status, 404, #{<<"content-type">> => <<"text/html">>}, <<>>}
    end.

server_error(#{crash_info := #{status_code := StatusCode} = CrashInfo} = Req) ->
    Variables = #{status => maps:get(status, CrashInfo, ~"Internal Server Error"),
                  title => maps:get(title, CrashInfo, ~"500 Internal Server Error"),
                  message => maps:get(message, CrashInfo, ~"Something internal crashed. Please take a look!"),
                  extra_msg => maps:get(extra_msg, CrashInfo, ~""),
                  stacktrace => format_stacktrace(maps:get(stacktrace, CrashInfo, []))},
    case application:get_env(nova, render_error_pages, true) of
        true ->
            case preferred_type(Req) of
                json ->
                    Json = nova_json:encode(Variables),
                    {status, StatusCode, #{<<"content-type">> => <<"application/json">>}, Json};
                html ->
                    {ok, Body} = nova_error_dtl:render(Variables),
                    {status, StatusCode, #{<<"content-type">> => <<"text/html">>}, Body};
                none ->
                    {status, StatusCode, #{<<"content-type">> => <<"text/html">>}, <<>>}
            end;
        _ ->
            {status, StatusCode}
    end;
server_error(#{crash_info := #{class := Class, reason := Reason}} = Req) ->
    Stacktrace = maps:get(stacktrace, Req, []),
    Variables = #{status => "Internal Server Error",
                  title => "500 Internal Server Error",
                  message => "Something internal crashed. Please take a look!",
                  extra_msg => io_lib:format("Class: ~p<br /> Reason: ~p", [Class, Reason]),
                  stacktrace => format_stacktrace(Stacktrace)},

    case nova:get_environment() of
        dev ->
            %% We do show a proper error response
            case preferred_type(Req) of
                json ->
                    Json = nova_json:encode(Variables),
                    {status, 500, #{<<"content-type">> => <<"application/json">>}, Json};
                html ->
                    {ok, Body} = nova_error_dtl:render(Variables),
                    {status, 500, #{<<"content-type">> => <<"text/html">>}, Body};
                none ->
                    {status, 500, #{<<"content-type">> => <<"text/html">>}, <<>>}
            end;
        _ ->
            {status, 500}
    end.


%% Media ranges as browsers and HTTP clients send them, for example
%% "text/html,application/xhtml+xml;q=0.9" or "application/json; charset=utf-8".
preferred_type(Req) ->
    case cowboy_req:header(~"accept", Req) of
        undefined ->
            none;
        Accept ->
            Types = [string:trim(hd(binary:split(Range, ~";"))) || Range <- binary:split(Accept, ~",", [global])],
            case {lists:member(~"application/json", Types), lists:member(~"text/html", Types)} of
                {true, _} -> json;
                {_, true} -> html;
                _ -> none
            end
    end.

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

-ifdef(TEST).
-compile(export_all).
-endif.
