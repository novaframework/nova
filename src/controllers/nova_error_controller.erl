-module(nova_error_controller).
-moduledoc false.
-include_lib("kernel/include/logger.hrl").
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
    Variables = debug_details(#{status => maps:get(status, CrashInfo, ~"Internal Server Error"),
                                title => maps:get(title, CrashInfo, ~"500 Internal Server Error"),
                                message => maps:get(message, CrashInfo,
                                                    ~"Something internal crashed. Please take a look!")},
                              maps:get(extra_msg, CrashInfo, ~""),
                              stacktrace(CrashInfo)),
    render(StatusCode, Variables, Req);
server_error(#{crash_info := #{class := Class, reason := Reason} = CrashInfo} = Req) ->
    Variables = #{status => ~"Internal Server Error",
                  title => ~"500 Internal Server Error",
                  message => ~"Something internal crashed. Please take a look!"},
    case show_details() of
        true ->
            Details = iolist_to_binary(io_lib:format("Class: ~p~nReason: ~p", [Class, Reason])),
            render(500, debug_details(Variables, Details, maps:get(stacktrace, CrashInfo, [])), Req);
        false ->
            render(500, Variables, Req)
    end;
server_error(Req) ->
    server_error(Req#{crash_info => #{status_code => 500}}).

render(StatusCode, Variables, Req) ->
    page(StatusCode, Variables, Variables, Req).

%% Every page of Nova leaves from here, and this is the one place that reads
%% render_error_pages: with it off the status is sent bare to every client,
%% whatever form it accepts. The status routes of an application are not
%% pages of Nova and run either way.
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
            {status, StatusCode, #{~"content-type" => ~"text/html"}, <<>>}
    end.

%% A client that sends no accept header gets JSON, as does one whose header
%% cannot be read. One that accepts neither JSON nor HTML gets no body.
response_type(Req) ->
    try cowboy_req:parse_header(~"accept", Req) of
        undefined -> json;
        Ranges when is_list(Ranges) -> preferred_type(lists:enumerate(Ranges))
    catch
        exit:{request_error, {header, ~"accept"}, _Message} -> json
    end.

%% The type the client gives the higher quality wins, and of two it likes
%% the same the one it names first. JSON wins when one range covers both, as
%% "*/*" does.
preferred_type(Ranges) ->
    case {quality(json, Ranges), quality(html, Ranges)} of
        {{0, _}, {0, _}} -> none;
        {{JsonQuality, _}, {HtmlQuality, _}} when JsonQuality > HtmlQuality -> json;
        {{Quality, JsonAt}, {Quality, HtmlAt}} when JsonAt =< HtmlAt -> json;
        _ -> html
    end.

%% The quality of the most specific range that covers the type, and where
%% in the header that range is (RFC 9110 12.5.1). A quality of 0 excludes
%% the type.
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

%% Crash details can carry anything a controller or plugin returned, so they
%% are only shown when the environment is explicitly set to dev; an unset
%% environment shows none.
debug_details(Variables, ExtraMsg, Stacktrace) ->
    case show_details() of
        true -> Variables#{extra_msg => ExtraMsg, stacktrace => format_stacktrace(Stacktrace)};
        false -> Variables
    end.

show_details() ->
    nova:is_dev_environment().

%% nova_handler and nova_security_handler take the stacktrace of a crash
%% only when stacktraces are on.
stacktrace(#{stacktrace := Stacktrace}) ->
    Stacktrace;
stacktrace(#{class := _Class}) ->
    not_enabled;
stacktrace(_CrashInfo) ->
    [].

format_stacktrace(not_enabled) ->
    Key = {?MODULE, stacktrace_hint},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?LOG_WARNING(#{msg => ~"Stacktraces are disabled. Call nova:use_stacktrace(true) or set {use_stacktrace, true} for nova in sys.config to have them"})
    end,
    [];
format_stacktrace([]) -> [];
format_stacktrace([{Mod, Func, Arity, TraceOpts}|Tl]) ->
    File = case proplists:get_value(file, TraceOpts) of
                undefined -> undefined;
                F -> unicode:characters_to_binary(F)
           end,
    Line = proplists:get_value(line, TraceOpts),
    Formated = #{module => erlang:atom_to_binary(Mod, utf8),
                 function => erlang:atom_to_binary(Func, utf8),
                 arity => format_arity(Arity),
                 file => File,
                 line => Line},
    [Formated|format_stacktrace(Tl)];
format_stacktrace([#{module := Mod, function := Func, arity := Arity} = Frame|Tl]) ->
    File = case maps:get(file, Frame, undefined) of
               F when is_list(F) -> unicode:characters_to_binary(F);
               F -> F
           end,
    Formated = #{module => erlang:atom_to_binary(Mod, utf8),
                 function => erlang:atom_to_binary(Func, utf8),
                 arity => format_arity(Arity),
                 file => File,
                 line => maps:get(line, Frame, undefined)},
    [Formated|format_stacktrace(Tl)];
format_stacktrace([Hd|Tl]) ->
    ?LOG_WARNING(#{msg => ~"Could not format stacktrace line", frame => nova_redact:term(Hd)}),
    format_stacktrace(Tl).

format_arity(Args) when is_list(Args) -> length(Args);
format_arity(Arity) -> Arity.
