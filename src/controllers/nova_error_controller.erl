-module(nova_error_controller).
-export([
         not_found/1,
         server_error/1
        ]).

not_found(Req) ->
    %% Check the accept-headers
    case cowboy_req:header(<<"accept">>, Req) of
        <<"application/json">> ->
            %% Render a json response
            JsonLib = nova:get_env(json_lib, thoas),
            Json = erlang:apply(JsonLib, encode, [#{message => "Resource not found"}]),
            {status, 404, #{<<"content-type">> => <<"application/json">>}, Json};
        _ ->
            %% Just assume HTML
            Variables = #{status => "Could not find the page you were looking for",
                          title => "404 Not found",
                          message => "We could not find the page you were looking for"},
            {ok, Body} = nova_error_dtl:render(Variables),
            {status, 404, #{<<"content-type">> => <<"text/html">>}, Body}
    end.

server_error(#{crash_info := #{status_code := StatusCode} = CrashInfo} = Req) ->
    Variables = #{status => maps:get(status, CrashInfo, undefined),
                  title => maps:get(title, CrashInfo, undefined),
                  message => maps:get(message, CrashInfo, undefined),
                  extra_msg => maps:get(extra_msg, CrashInfo, undefined),
                  stacktrace => format_stacktrace(maps:get(stacktrace, CrashInfo, []))},
    case application:get_env(nova, render_error_pages, true) of
        true ->
            case cowboy_req:header(<<"accept">>, Req) of
                <<"application/json">> ->
                    JsonLib = nova:get_env(json_lib, thoas),
                    Json = erlang:apply(JsonLib, encode, [Variables]),
                    {status, StatusCode, #{<<"content-type">> => <<"application/json">>}, Json};
                _ ->
                    {ok, Body} = nova_error_dtl:render(Variables),
                    {status, StatusCode, #{<<"content-type">> => <<"text/html">>}, Body}
            end;
        _ ->
            {status, StatusCode}
    end;
server_error(#{crash_info := #{stacktrace := Stacktrace, class := Class, reason := Reason}} = Req) ->
    Variables = #{status => "Internal Server Error",
                  title => "500 Internal Server Error",
                  message => "Something internal crashed. Please take a look!",
                  extra_msg => io_lib:format("Class: ~p<br /> Reason: ~p", [Class, Reason]),
                  stacktrace => format_stacktrace(Stacktrace)},

    case nova:get_environment() of
        dev ->
            %% We do show a proper error response
            case cowboy_req:header(<<"accept">>, Req) of
                <<"application/json">> ->
                    JsonLib = nova:get_env(json_lib, thoas),
                    Json = erlang:apply(JsonLib, encode, [Variables]),
                    {status, 500, #{<<"content-type">> => <<"application/json">>}, Json};
                _ ->
                    {ok, Body} = nova_error_dtl:render(Variables),
                    {status, 500, #{<<"content-type">> => <<"text/html">>}, Body}
            end;
        _ ->
            {status, 500}
    end.


format_stacktrace([]) -> [];
format_stacktrace([{Mod, Func, Arity, [{file, File}, {line, Line}]}|Tl]) ->
    [#{module => erlang:atom_to_binary(Mod, utf8),
       function => erlang:atom_to_binary(Func, utf8),
       arity => Arity,
       file => erlang:list_to_binary(File),
       line => Line}|format_stacktrace(Tl)];
format_stacktrace([Hd|Tl]) ->
    logger:warning("Could not format stacktrace line: ~p", [Hd]),
    format_stacktrace(Tl).
