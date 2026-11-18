-module(nova_req).
-moduledoc """
The request, and the response to it.

A controller, a plugin and a security callback get the request as a map and
read it, build the response on it and send it with the functions here. They
work the same on every server Nova runs on.

## The request map

These keys are always there, and can be matched on:

- `method`: `~"GET"`, in uppercase
- `scheme`: `~"http"` or `~"https"`
- `host`, `port`, `path` and `qs`, the raw query string
- `version`: `'HTTP/1.0'`, `'HTTP/1.1'`, `'HTTP/2'` or `'HTTP/3'`. See
  `http_version/1`.
- `headers`: a map of lowercase names to values. A header sent more than once
  has its values joined with `, `, and `cookie` with `; `.
- `peer`: `{IP, Port}` of the client
- `has_body` and `body_length`, which is `undefined` when the client did not
  say

Nova adds `bindings`, `listener` and, as the request goes through the
pipeline, keys such as `auth_data`, `json` and `params`. The keys that start
with `resp_`, `has_sent_resp` and whatever else is in the map belong to Nova
and to the server.

## Sending the response

A controller usually returns its response. One that sends it itself calls
`reply/2,3,4`, or `stream_reply/2,3` followed by `stream_body/3`, and hands
back the request those return.

A request is answered once. A second answer is not sent, and is logged.
""".

-export([
         method/1,
         version/1,
         http_version/1,
         scheme/1,
         host/1,
         port/1,
         path/1,
         qs/1,
         peer/1,
         listener/1,
         parse_qs/1,
         binding/2,
         binding/3,
         bindings/1,
         header/2,
         header/3,
         headers/1,
         parse_header/2,
         parse_header/3,
         parse_cookies/1
        ]).

-export([
         has_body/1,
         body_length/1,
         read_body/1,
         read_body/2,
         read_urlencoded_body/1,
         read_urlencoded_body/2,
         read_part/1,
         read_part/2,
         read_part_body/1,
         read_part_body/2
        ]).

-export([
         set_resp_cookie/3,
         set_resp_cookie/4,
         resp_header/2,
         resp_header/3,
         resp_headers/1,
         set_resp_header/3,
         set_resp_headers/2,
         has_resp_header/2,
         delete_resp_header/2,
         set_resp_body/2,
         has_resp_body/1,
         inform/2,
         inform/3,
         reply/2,
         reply/3,
         reply/4,
         stream_reply/2,
         stream_reply/3,
         stream_body/3,
         stream_trailers/2,
         response_begun/1
        ]).

-export_type([req/0, http_version/0, http_headers/0, http_status/0, resp_body/0,
              cookie_opts/0, read_body_opts/0]).

-include_lib("kernel/include/logger.hrl").

-type req() :: map().
-type http_version() :: nova_server:protocol().
-type http_headers() :: #{binary() => iodata()}.
-type http_status() :: 100..599 | binary().
-type resp_body() :: nova_server:body().
-type read_body_opts() :: nova_server:read_body_opts().
-type cookie_opts() :: #{domain => binary(),
                         http_only => boolean(),
                         max_age => non_neg_integer(),
                         path => binary(),
                         same_site => default | none | strict | lax,
                         secure => boolean()}.

-define(READ_URLENCODED_LENGTH, 64000).
-define(READ_URLENCODED_PERIOD, 5000).
-define(READ_PART_LENGTH, 64000).
-define(READ_PART_PERIOD, 5000).
-define(READ_PART_BODY_LENGTH, 8000000).
-define(MAX_PART_HEADER_BLOCK, 2048).
-define(DROPPED_INTERVAL, 1000).

%%%===================================================================
%%% Request
%%%===================================================================

-spec method(req()) -> binary().
method(#{method := Method}) -> Method.

-spec version(req()) -> atom().
version(#{version := Version}) -> Version.

-doc "The HTTP version of the request as Nova names it in a listener's `protocols`.".
-spec http_version(req()) -> http_version().
http_version(#{version := 'HTTP/3'}) -> http3;
http_version(#{version := 'HTTP/2'}) -> http2;
http_version(#{version := _}) -> http1.

-spec scheme(req()) -> binary().
scheme(#{scheme := Scheme}) -> Scheme.

-spec host(req()) -> binary().
host(#{host := Host}) -> Host.

-spec port(req()) -> inet:port_number().
port(#{port := Port}) -> Port.

-spec path(req()) -> binary().
path(#{path := Path}) -> Path.

-spec qs(req()) -> binary().
qs(#{qs := Qs}) -> Qs.

-spec peer(req()) -> {inet:ip_address(), inet:port_number()}.
peer(#{peer := Peer}) -> Peer.

-doc "The name of the listener the request came in on.".
-spec listener(req()) -> atom() | undefined.
listener(Req) -> maps:get(listener, Req, undefined).

-doc "The query string as a list of names and values. A name without `=` has the value `true`.".
-spec parse_qs(req()) -> [{binary(), binary() | true}].
parse_qs(#{qs := Qs}) ->
    try
        cow_qs:parse_qs(Qs)
    catch
        _:_:Stacktrace ->
            erlang:raise(exit, {request_error, qs,
                                'Malformed query string; application/x-www-form-urlencoded expected.'},
                         Stacktrace)
    end.

-spec binding(binary() | atom(), req()) -> term().
binding(Name, Req) ->
    binding(Name, Req, undefined).

-spec binding(binary() | atom(), req(), Default) -> term() | Default.
binding(Name, #{bindings := Bindings}, Default) ->
    maps:get(Name, Bindings, Default);
binding(_Name, _Req, Default) ->
    Default.

-spec bindings(req()) -> map().
bindings(Req) -> maps:get(bindings, Req, #{}).

-spec header(binary(), req()) -> binary() | undefined.
header(Name, Req) ->
    header(Name, Req, undefined).

-spec header(binary(), req(), Default) -> binary() | Default.
header(Name, #{headers := Headers}, Default) ->
    maps:get(Name, Headers, Default).

-spec headers(req()) -> #{binary() => binary()}.
headers(#{headers := Headers}) -> Headers.

-doc """
A request header, parsed. `Name` is one of `accept`, `accept-charset`,
`accept-encoding`, `accept-language`, `authorization`, `content-length`,
`content-type`, `cookie`, `if-match`, `if-modified-since`, `if-none-match`,
`if-range`, `if-unmodified-since`, `origin`, `range` and `x-forwarded-for`.
A header that does not parse is a request error, answered 400.
""".
-spec parse_header(binary(), req()) -> term().
parse_header(Name, Req) ->
    parse_header(Name, Req, undefined).

-spec parse_header(binary(), req(), Default) -> term() | Default.
parse_header(Name, Req, Default) ->
    Parse = header_parser(Name),
    case header(Name, Req) of
        undefined ->
            Default;
        Value ->
            try
                Parse(Value)
            catch
                _:_:Stacktrace ->
                    erlang:raise(exit, {request_error, {header, Name},
                                        'Malformed header. Please consult the relevant specification.'},
                                 Stacktrace)
            end
    end.

header_parser(~"accept") -> fun cow_http_hd:parse_accept/1;
header_parser(~"accept-charset") -> fun cow_http_hd:parse_accept_charset/1;
header_parser(~"accept-encoding") -> fun cow_http_hd:parse_accept_encoding/1;
header_parser(~"accept-language") -> fun cow_http_hd:parse_accept_language/1;
header_parser(~"authorization") -> fun cow_http_hd:parse_authorization/1;
header_parser(~"content-length") -> fun cow_http_hd:parse_content_length/1;
header_parser(~"content-type") -> fun cow_http_hd:parse_content_type/1;
header_parser(~"cookie") -> fun cow_cookie:parse_cookie/1;
header_parser(~"if-match") -> fun cow_http_hd:parse_if_match/1;
header_parser(~"if-modified-since") -> fun cow_http_hd:parse_if_modified_since/1;
header_parser(~"if-none-match") -> fun cow_http_hd:parse_if_none_match/1;
header_parser(~"if-range") -> fun cow_http_hd:parse_if_range/1;
header_parser(~"if-unmodified-since") -> fun cow_http_hd:parse_if_unmodified_since/1;
header_parser(~"origin") -> fun cow_http_hd:parse_origin/1;
header_parser(~"range") -> fun cow_http_hd:parse_range/1;
header_parser(~"x-forwarded-for") -> fun cow_http_hd:parse_x_forwarded_for/1;
header_parser(Name) -> error({unsupported_header, Name}).

-spec parse_cookies(req()) -> [{binary(), binary()}].
parse_cookies(Req) ->
    parse_header(~"cookie", Req, []).

%%%===================================================================
%%% Request body
%%%===================================================================

-spec has_body(req()) -> boolean().
has_body(Req) -> maps:get(has_body, Req, false).

-spec body_length(req()) -> non_neg_integer() | undefined.
body_length(Req) -> maps:get(body_length, Req, undefined).

-spec read_body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req :: req().
read_body(Req) ->
    read_body(Req, #{}).

-doc """
Read the body, or the next piece of it: `{more, Data, Req}` when there is
more to read. See `nova_server` for the options.
""".
-spec read_body(Req, read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
              when Req :: req().
read_body(Req, Opts) ->
    (nova_server:of_req(Req)):read_body(Req, Opts).

-spec read_urlencoded_body(Req) -> {ok, [{binary(), binary() | true}], Req} when Req :: req().
read_urlencoded_body(Req) ->
    read_urlencoded_body(Req, #{}).

-doc "Read a form body. One that is larger than `length`, 64000 bytes unless set, is answered 413.".
-spec read_urlencoded_body(Req, read_body_opts()) -> {ok, [{binary(), binary() | true}], Req}
              when Req :: req().
read_urlencoded_body(Req0, Opts) ->
    Defaults = #{length => ?READ_URLENCODED_LENGTH, period => ?READ_URLENCODED_PERIOD},
    case read_body(Req0, maps:merge(Defaults, Opts)) of
        {ok, Body, Req} ->
            try
                {ok, cow_qs:parse_qs(Body), Req}
            catch
                _:_:Stacktrace ->
                    erlang:raise(exit, {request_error, urlencoded_body,
                                        'Malformed body; application/x-www-form-urlencoded expected.'},
                                 Stacktrace)
            end;
        {more, Body, _Req} ->
            Length = maps:get(length, Opts, ?READ_URLENCODED_LENGTH),
            Reason = case byte_size(Body) > Length of
                         true -> payload_too_large;
                         false -> timeout
                     end,
            exit({request_error, Reason,
                  'The request body is too large or was not received in time.'})
    end.

-spec read_part(Req) -> {ok, #{binary() => binary()}, Req} | {done, Req} when Req :: req().
read_part(Req) ->
    read_part(Req, #{length => ?READ_PART_LENGTH, period => ?READ_PART_PERIOD}).

-doc """
Read the headers of the next part of a `multipart` body, or `done` after the
last. `read_part_body/1,2` reads the part.
""".
-spec read_part(Req, read_body_opts()) -> {ok, #{binary() => binary()}, Req} | {done, Req}
              when Req :: req().
read_part(Req = #{multipart := _}, Opts) ->
    {Data, Req0} = stream_multipart(Req, Opts, headers),
    read_part(Data, Opts, Req0);
read_part(Req, Opts) ->
    read_part(init_multipart(Req), Opts).

read_part(Buffer, Opts, Req = #{multipart := {Boundary, _}}) ->
    try cow_multipart:parse_headers(Buffer, Boundary) of
        more ->
            read_part_more(Buffer, Opts, Req);
        {more, Buffer0} ->
            read_part_more(Buffer0, Opts, Req);
        {ok, Headers0, Rest} ->
            Headers = maps:from_list(Headers0),
            case map_size(Headers) =:= length(Headers0) of
                true ->
                    {ok, Headers, Req#{multipart => {Boundary, Rest}}};
                false ->
                    exit({request_error, {multipart, headers},
                          'Malformed body; a multipart header is there twice.'})
            end;
        {done, _Epilogue} ->
            {done, Req#{multipart => done}}
    catch
        error:_:Stacktrace ->
            erlang:raise(exit, {request_error, {multipart, headers},
                                'Malformed body; multipart expected.'}, Stacktrace)
    end.

read_part_more(Buffer, _Opts, _Req) when byte_size(Buffer) > ?MAX_PART_HEADER_BLOCK ->
    exit({request_error, {multipart, headers},
          'Malformed body; multipart header block too large.'});
read_part_more(Buffer, Opts, Req0) ->
    {Data, Req} = stream_multipart(Req0, Opts, headers),
    read_part(<<Buffer/binary, Data/binary>>, Opts, Req).

-spec read_part_body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req :: req().
read_part_body(Req) ->
    read_part_body(Req, #{}).

-spec read_part_body(Req, read_body_opts()) -> {ok, binary(), Req} | {more, binary(), Req}
              when Req :: req().
read_part_body(Req = #{multipart := _}, Opts) ->
    read_part_body(<<>>, Opts, Req, <<>>);
read_part_body(Req, Opts) ->
    read_part_body(init_multipart(Req), Opts).

read_part_body(Buffer, Opts, Req = #{multipart := {Boundary, _}}, Acc) ->
    Length = maps:get(length, Opts, ?READ_PART_BODY_LENGTH),
    case byte_size(Acc) > Length of
        true ->
            {more, Acc, Req#{multipart => {Boundary, Buffer}}};
        false ->
            {Data, Req0} = stream_multipart(Req, Opts, body),
            case cow_multipart:parse_body(<<Buffer/binary, Data/binary>>, Boundary) of
                {ok, Body} ->
                    read_part_body(<<>>, Opts, Req0, <<Acc/binary, Body/binary>>);
                {ok, Body, Rest} ->
                    read_part_body(Rest, Opts, Req0, <<Acc/binary, Body/binary>>);
                done ->
                    {ok, Acc, Req0};
                {done, Body} ->
                    {ok, <<Acc/binary, Body/binary>>, Req0};
                {done, Body, Rest} ->
                    {ok, <<Acc/binary, Body/binary>>, Req0#{multipart => {Boundary, Rest}}}
            end
    end.

init_multipart(Req) ->
    case parse_header(~"content-type", Req) of
        {~"multipart", _SubType, Params} ->
            case lists:keyfind(~"boundary", 1, Params) of
                {_, Boundary} ->
                    Req#{multipart => {Boundary, <<>>}};
                false ->
                    exit({request_error, {multipart, boundary},
                          'Missing boundary parameter for multipart media type.'})
            end;
        _NotMultipart ->
            exit({request_error, {multipart, content_type},
                  'Malformed body; multipart expected.'})
    end.

stream_multipart(Req = #{multipart := done}, _Opts, _Type) ->
    {<<>>, Req};
stream_multipart(Req = #{multipart := {_Boundary, <<>>}}, Opts, Type) ->
    case read_body(Req, Opts) of
        {more, Data, Req0} ->
            {Data, Req0};
        {ok, <<>>, _Req0} ->
            exit({request_error, {multipart, Type}, 'Malformed body; multipart expected.'});
        {ok, Data, Req0} ->
            {Data, Req0}
    end;
stream_multipart(Req = #{multipart := {Boundary, Buffer}}, _Opts, _Type) ->
    {Buffer, Req#{multipart => {Boundary, <<>>}}}.

%%%===================================================================
%%% Response
%%%===================================================================

-spec set_resp_cookie(binary(), iodata(), Req) -> Req when Req :: req().
set_resp_cookie(Name, Value, Req) ->
    set_resp_cookie(Name, Value, Req, #{}).

-doc """
Set a cookie on the response. A name or a value with a character a cookie
cannot hold raises.
""".
-spec set_resp_cookie(binary(), iodata(), Req, cookie_opts()) -> Req when Req :: req().
set_resp_cookie(Name, Value, Req, Opts) ->
    Cookie = cow_cookie:setcookie(Name, Value, Opts),
    Cookies = maps:get(resp_cookies, Req, #{}),
    Req#{resp_cookies => Cookies#{Name => Cookie}}.

-spec resp_header(binary(), req()) -> iodata() | undefined.
resp_header(Name, Req) ->
    resp_header(Name, Req, undefined).

-spec resp_header(binary(), req(), Default) -> iodata() | Default.
resp_header(Name, Req, Default) ->
    maps:get(Name, maps:get(resp_headers, Req, #{}), Default).

-spec resp_headers(req()) -> http_headers().
resp_headers(Req) ->
    maps:get(resp_headers, Req, #{}).

-spec set_resp_header(binary(), iodata(), Req) -> Req when Req :: req().
set_resp_header(~"set-cookie", _Value, _Req) ->
    exit(cookie_as_header());
set_resp_header(Name, Value, Req) ->
    Headers = maps:get(resp_headers, Req, #{}),
    Req#{resp_headers => Headers#{Name => Value}}.

-spec set_resp_headers(http_headers(), Req) -> Req when Req :: req().
set_resp_headers(#{~"set-cookie" := _}, _Req) ->
    exit(cookie_as_header());
set_resp_headers(Headers, Req) ->
    Req#{resp_headers => maps:merge(maps:get(resp_headers, Req, #{}), Headers)}.

-spec has_resp_header(binary(), req()) -> boolean().
has_resp_header(Name, Req) ->
    is_map_key(Name, maps:get(resp_headers, Req, #{})).

-spec delete_resp_header(binary(), Req) -> Req when Req :: req().
delete_resp_header(Name, Req = #{resp_headers := Headers}) ->
    Req#{resp_headers => maps:remove(Name, Headers)};
delete_resp_header(_Name, Req) ->
    Req.

-spec set_resp_body(resp_body(), Req) -> Req when Req :: req().
set_resp_body(Body, Req) ->
    Req#{resp_body => Body}.

-spec has_resp_body(req()) -> boolean().
has_resp_body(#{resp_body := {sendfile, _Offset, Length, _Path}}) ->
    Length > 0;
has_resp_body(#{resp_body := Body}) ->
    iolist_size(Body) > 0;
has_resp_body(_Req) ->
    false.

-spec inform(http_status(), req()) -> ok.
inform(Status, Req) ->
    inform(Status, #{}, Req).

-doc "Send an informational response, such as `103`, ahead of the response. A server that cannot leaves it out.".
-spec inform(http_status(), http_headers(), req()) -> ok.
inform(Status, Headers, Req) ->
    Server = nova_server:of_req(Req),
    case response_begun(Req) =:= false andalso nova_server:supports(Server, {inform, 3}) of
        true -> Server:inform(status(Status), header_list(Headers), Req);
        false -> ok
    end.

-spec reply(http_status(), Req) -> Req when Req :: req().
reply(Status, Req) ->
    reply(Status, #{}, Req).

-spec reply(http_status(), http_headers(), Req) -> Req when Req :: req().
reply(Status, Headers, Req = #{resp_body := Body}) ->
    reply(Status, Headers, Body, Req);
reply(Status, Headers, Req) ->
    reply(Status, Headers, <<>>, Req).

-doc """
Send the response: the headers and cookies set on the request, `Headers`,
which win over those, and `Body`. A `204` and a `304` have no body, and one
given a body raises.
""".
-spec reply(http_status(), http_headers(), resp_body(), Req) -> Req when Req :: req().
reply(_Status, #{~"set-cookie" := _}, _Body, _Req) ->
    exit(cookie_as_header());
reply(Status0, Headers, Body, Req) ->
    Status = status(Status0),
    case response_begun(Req) of
        false ->
            Length = body_size(Body),
            ok = no_body_where_none_is_allowed(Status, Length),
            Out = response_headers(with_content_length(Status, Length, Headers), Req),
            Server = nova_server:of_req(Req),
            answered(Server:reply(Status, Out, body_to_send(Body, Req), Req), true);
        Sent ->
            dropped(Status, Req, Sent)
    end.

-spec stream_reply(http_status(), Req) -> Req when Req :: req().
stream_reply(Status, Req) ->
    stream_reply(Status, #{}, Req).

-doc "Send the status and the headers of a response whose body follows with `stream_body/3`.".
-spec stream_reply(http_status(), http_headers(), Req) -> Req when Req :: req().
stream_reply(_Status, #{~"set-cookie" := _}, _Req) ->
    exit(cookie_as_header());
stream_reply(Status0, Headers, Req) ->
    case status(Status0) of
        Status when Status =:= 204; Status =:= 304 ->
            reply(Status, Headers, <<>>, Req);
        Status ->
            case response_begun(Req) of
                false ->
                    Server = nova_server:of_req(Req),
                    answered(Server:stream_reply(Status, response_headers(Headers, Req), Req), headers);
                Sent ->
                    dropped(Status, Req, Sent)
            end
    end.

-doc "Send a piece of a streamed body. `fin` ends the response.".
-spec stream_body(iodata(), fin | nofin, req()) -> ok.
stream_body(_Data, _IsFin, #{method := ~"HEAD"}) ->
    ok;
stream_body(Data, IsFin, Req) ->
    case IsFin =:= nofin andalso iolist_size(Data) =:= 0 of
        true -> ok;
        false -> (nova_server:of_req(Req)):stream_body(Data, IsFin, Req)
    end.

-doc "End a streamed response with trailers. A server that cannot send them ends the response without.".
-spec stream_trailers(http_headers(), req()) -> ok.
stream_trailers(Trailers, Req) ->
    Server = nova_server:of_req(Req),
    case nova_server:supports(Server, {stream_trailers, 2}) of
        true -> Server:stream_trailers(header_list(Trailers), Req);
        false -> Server:stream_body(<<>>, fin, Req)
    end.

-doc """
How far the response to the request got. A controller that sends the response
and does not hand back the request it got from that leaves Nova with the
request as it was, so this asks the request process and the server too.
""".
-spec response_begun(req()) -> nova_server:response_progress().
response_begun(#{has_sent_resp := Sent}) ->
    Sent;
response_begun(Req) ->
    case get(nova_response_begun) of
        undefined -> server_response_begun(Req);
        Sent -> Sent
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_response_begun(Req) ->
    Server = nova_server:of_req(Req),
    case nova_server:supports(Server, {response_begun, 1}) of
        true -> Server:response_begun(Req);
        false -> false
    end.

%% The request process remembers that it answered: the request a controller
%% hands back may be the one from before it did.
answered(Req, Sent) ->
    _ = put(nova_response_begun, Sent),
    (maps:without([resp_headers, resp_cookies, resp_body], Req))#{has_sent_resp => Sent}.

%% What is wrong with one request is wrong with every request to its route,
%% so this is one line a second on the node.
dropped(Status, Req, Sent) ->
    case nova_once:per_interval({?MODULE, dropped}, ?DROPPED_INTERVAL) of
        {true, NotLogged} ->
            ?LOG_WARNING(#{msg => ~"The request was answered already, so this response is not sent. A controller, plugin or security callback that answers the request has to hand back the request it got from that",
                           status => Status, method => maps:get(method, Req, undefined),
                           path => maps:get(path, Req, undefined), not_logged => NotLogged});
        false ->
            ok
    end,
    Req#{has_sent_resp => Sent}.

-spec status(http_status()) -> nova_server:status().
status(Status) when is_integer(Status), Status >= 100, Status =< 599 ->
    Status;
status(<<Code:3/binary, _ReasonPhrase/binary>>) ->
    status(binary_to_integer(Code)).

body_size({sendfile, _Offset, Length, _Path}) -> Length;
body_size(Body) -> iolist_size(Body).

no_body_where_none_is_allowed(Status, Length) when Length > 0, (Status =:= 204 orelse Status =:= 304) ->
    exit({response_error, payload_too_large,
          '204 and 304 responses must not include a body. (RFC7230 3.3)'});
no_body_where_none_is_allowed(_Status, _Length) ->
    ok.

%% A 204 has no content-length, and a 304 only the one it was given.
with_content_length(Status, _Length, Headers) when Status =:= 204; Status =:= 304 ->
    Headers;
with_content_length(_Status, Length, Headers) ->
    Headers#{~"content-length" => integer_to_binary(Length)}.

body_to_send(_Body, #{method := ~"HEAD"}) -> <<>>;
body_to_send({sendfile, _Offset, 0, _Path}, _Req) -> <<>>;
body_to_send(Body, _Req) -> Body.

%% The headers set on the request, those of the call, which win, and a
%% set-cookie for each cookie.
-spec response_headers(http_headers(), req()) -> nova_server:headers().
response_headers(Headers, Req) ->
    Merged = maps:merge(maps:get(resp_headers, Req, #{}), Headers),
    header_list(Merged) ++ [{~"set-cookie", Cookie} || _Name := Cookie <- maps:get(resp_cookies, Req, #{})].

header_list(Headers) ->
    maps:to_list(Headers).

cookie_as_header() ->
    {response_error, invalid_header, 'Response cookies must be set using nova_req:set_resp_cookie/3,4.'}.
