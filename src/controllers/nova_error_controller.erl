-module(nova_error_controller).
-moduledoc false.
-export([
         not_found/1,
         server_error/1,
	 status_code/1
        ]).

-define(MAX_ACCEPT, 1024).


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
    Variables = debug_details(#{status => maps:get(status, CrashInfo, ~"Internal Server Error"),
                                title => maps:get(title, CrashInfo, ~"500 Internal Server Error"),
                                message => maps:get(message, CrashInfo,
                                                    ~"Something internal crashed. Please take a look!")},
                              maps:get(extra_msg, CrashInfo, ~""),
                              maps:get(stacktrace, CrashInfo, [])),
    case application:get_env(nova, render_error_pages, true) of
        true ->
            render(StatusCode, Variables, Req);
        _ ->
            {status, StatusCode}
    end;
server_error(#{crash_info := #{class := Class, reason := Reason} = CrashInfo} = Req) ->
    case show_details() of
        true ->
            Variables = debug_details(#{status => ~"Internal Server Error",
                                        title => ~"500 Internal Server Error",
                                        message => ~"Something internal crashed. Please take a look!"},
                                      iolist_to_binary(io_lib:format("Class: ~p~nReason: ~p", [Class, Reason])),
                                      maps:get(stacktrace, CrashInfo, [])),
            render(500, Variables, Req);
        false ->
            {status, 500}
    end.

render(StatusCode, Variables, Req) ->
    case preferred_type(Req) of
        json ->
            Json = nova_json:encode(Variables),
            {status, StatusCode, #{~"content-type" => ~"application/json"}, Json};
        html ->
            {ok, Body} = nova_error_dtl:render(Variables),
            {status, StatusCode, #{~"content-type" => ~"text/html"}, Body};
        none ->
            {status, StatusCode, #{~"content-type" => ~"text/html"}, <<>>}
    end.

%% Crash details can carry anything a controller or plugin returned, so they
%% are only shown when the environment is explicitly set to dev; an unset
%% environment shows none.
debug_details(Variables, ExtraMsg, Stacktrace) ->
    case show_details() of
        true -> Variables#{extra_msg => ExtraMsg, stacktrace => format_stacktrace(Stacktrace)};
        false -> Variables
    end.

show_details() ->
    application:get_env(nova, environment) =:= {ok, dev}.

%% Media ranges as browsers and HTTP clients send them, for example
%% "text/html,application/xhtml+xml;q=0.9" or "application/json; charset=utf-8".
%% Cowboy joins repeated accept headers, so only the first ?MAX_ACCEPT bytes
%% are looked at, and every step is linear in them.
preferred_type(Req) ->
    case cowboy_req:header(~"accept", Req) of
        undefined ->
            none;
        Accept ->
            Head = binary:part(Accept, 0, min(byte_size(Accept), ?MAX_ACCEPT)),
            Types = [media_type(Range) || Range <- binary:split(Head, ~",", [global])],
            case {lists:member(~"application/json", Types), lists:member(~"text/html", Types)} of
                {true, _} -> json;
                {_, true} -> html;
                _ -> none
            end
    end.

%% Media types are case-insensitive (RFC 9110 8.3.1).
media_type(Range) ->
    [Type | _] = binary:split(Range, ~";"),
    string:lowercase(trim_ows(Type)).

trim_ows(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t ->
    trim_ows(Rest);
trim_ows(Bin) ->
    trim_trailing_ows(Bin, byte_size(Bin)).

trim_trailing_ows(Bin, 0) ->
    binary:part(Bin, 0, 0);
trim_trailing_ows(Bin, Size) ->
    case binary:at(Bin, Size - 1) of
        C when C =:= $\s; C =:= $\t -> trim_trailing_ows(Bin, Size - 1);
        _ -> binary:part(Bin, 0, Size)
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
