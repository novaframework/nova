-module(nova_error_controller).
-moduledoc false.
-include_lib("kernel/include/logger.hrl").
-export([
         not_found/1,
         server_error/1,
	 status_code/1
        ]).

-ifdef(TEST).
-export([format_arity/1, format_stacktrace/1, preferred_type/1, trim_ows/1]).
-endif.

-define(MAX_ACCEPT, 1024).


status_code(Req) ->
    {status, maps:get(resp_status_code, Req, 200)}.

not_found(Req) ->
    case response_type(Req) of
        json ->
            Json = nova_json:encode(#{message => ~"Resource not found"}),
            {status, 404, #{<<"content-type">> => <<"application/json">>}, Json};
        html ->
            Variables = #{status => "Could not find the page you were looking for",
                          title => "404 Not found",
                          message => "We could not find the page you were looking for"},
            {ok, Body} = nova_basic_handler:render_dtl(nova_error_dtl, Variables, []),
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
    case response_type(Req) of
        json ->
            Json = nova_json:encode(Variables),
            {status, StatusCode, #{~"content-type" => ~"application/json"}, Json};
        html ->
            html_page(StatusCode, Variables);
        none ->
            {status, StatusCode, #{~"content-type" => ~"text/html"}, <<>>}
    end.

%% render_error_pages is about the HTML page only.
html_page(StatusCode, Variables) ->
    case application:get_env(nova, render_error_pages, true) of
        true ->
            {ok, Body} = nova_basic_handler:render_dtl(nova_error_dtl, Variables, []),
            {status, StatusCode, #{~"content-type" => ~"text/html"}, Body};
        _ ->
            {status, StatusCode}
    end.

%% A client that sends no accept header gets JSON. One that sends a header
%% naming neither JSON nor HTML gets no body.
response_type(Req) ->
    case cowboy_req:header(~"accept", Req) of
        undefined -> json;
        Accept -> preferred_type(Accept)
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
    nova:is_dev_environment().

%% nova_handler and nova_security_handler take the stacktrace of a crash
%% only when stacktraces are on.
stacktrace(#{stacktrace := Stacktrace}) ->
    Stacktrace;
stacktrace(#{class := _Class}) ->
    not_enabled;
stacktrace(_CrashInfo) ->
    [].

%% Media ranges as browsers and HTTP clients send them, for example
%% "text/html,application/xhtml+xml;q=0.9" or "application/json; charset=utf-8".
%% Cowboy joins repeated accept headers, so only the first ?MAX_ACCEPT bytes
%% are looked at, and every step is linear in them.
preferred_type(Accept) ->
    Head = binary:part(Accept, 0, min(byte_size(Accept), ?MAX_ACCEPT)),
    Types = [media_type(Range) || Range <- binary:split(Head, ~",", [global])],
    case {lists:member(~"application/json", Types), lists:member(~"text/html", Types)} of
        {true, _} -> json;
        {_, true} -> html;
        _ -> none
    end.

%% Media types are case-insensitive (RFC 9110 8.3.1).
media_type(Range) ->
    [Type | _] = binary:split(Range, ~";"),
    string:lowercase(trim_ows(Type)).

trim_ows(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t ->
    trim_ows(Rest);
trim_ows(Bin) ->
    trim_trailing_ows(Bin, byte_size(Bin)).

trim_trailing_ows(_Bin, 0) ->
    <<>>;
trim_trailing_ows(Bin, Size) ->
    case binary:at(Bin, Size - 1) of
        C when C =:= $\s; C =:= $\t -> trim_trailing_ows(Bin, Size - 1);
        _ -> binary:part(Bin, 0, Size)
    end.

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
