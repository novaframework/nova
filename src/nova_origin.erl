-module(nova_origin).
-moduledoc false.

%%% Whether a browser may call from an origin, by what `allowed_origins` of
%%% the nova environment says, or the option of the same name that
%%% nova_ws_origin_plugin and nova_csrf_plugin take.

-export([is_allowed/3]).

%% A request without an Origin header is not from a page, or from a browser
%% that says nothing about where it is, and is allowed. Every clause answers
%% for whatever a client put in `Origin` and `Host`, and for an Allowed that
%% is none of the values, which allows no origin.
-spec is_allowed(Origin :: binary() | undefined, Req :: cowboy_req:req(), Allowed :: term()) -> boolean().
is_allowed(undefined, _Req, _Allowed) ->
    true;
is_allowed(_Origin, _Req, any) ->
    true;
is_allowed(Origin, Req, same_host) when is_binary(Origin) ->
    case origin(Origin) of
        {Scheme, Host, Port} -> is_sent_to(Scheme, Host, Port, Req);
        undefined -> false
    end;
is_allowed(Origin, Req, same_origin) when is_binary(Origin) ->
    case origin(Origin) of
        {Scheme, Host, Port} ->
            Scheme =:= maps:get(scheme, Req, undefined) andalso Host =:= maps:get(host, Req, undefined)
                andalso port(Scheme, Port) =:= maps:get(port, Req, undefined);
        undefined ->
            false
    end;
is_allowed(Origin, _Req, Allowed) when is_binary(Origin) ->
    is_listed(ascii_lowercase(Origin), Allowed);
is_allowed(_Origin, _Req, _Allowed) ->
    false.

%% The scheme, host and port of an origin, the port as it was written.
-spec origin(binary()) -> {Scheme :: binary(), Host :: binary(), Port :: non_neg_integer() | undefined} | undefined.
origin(Origin) ->
    case binary:split(ascii_lowercase(Origin), ~"://") of
        [Scheme, Authority] when Scheme =/= <<>> ->
            case host_and_port(Authority) of
                {Host, Port} -> {Scheme, Host, Port};
                undefined -> undefined
            end;
        _NoAuthority ->
            undefined
    end.

%% The host comes back in lowercase. One that is not a host is refused by
%% the parser, an IPv6 address without its closing bracket included.
-spec host_and_port(binary()) -> {Host :: binary(), Port :: non_neg_integer() | undefined} | undefined.
host_and_port(Authority) ->
    try cow_http_hd:parse_host(Authority) of
        {Host, Port} when Host =/= <<>> -> {Host, Port};
        _NoHost -> undefined
    catch
        _Class:_Reason -> undefined
    end.

%% Whether an origin names the host the request was sent to. A `Host` header
%% that leaves the port out says nothing about it, so the origin must leave
%% it out too or name the port of its scheme. One that names a port, as a
%% proxy sending `$host:$server_port` does, names the port of the origin.
%% HTTP/2 and HTTP/3 send the authority outside the headers, and Cowboy keeps
%% it as host and port, with the port of the scheme where none was sent.
-spec is_sent_to(binary(), binary(), non_neg_integer() | undefined, cowboy_req:req()) -> boolean().
is_sent_to(Scheme, Host, Port, Req) ->
    case requested_host(Req) of
        {Host, undefined} -> Port =:= undefined orelse Port =:= port(Scheme, undefined);
        {Host, RequestedPort} -> RequestedPort =:= port(Scheme, Port);
        _AnotherHost -> false
    end.

-spec requested_host(cowboy_req:req()) -> {Host :: binary(), Port :: non_neg_integer() | undefined} | undefined.
requested_host(#{headers := #{~"host" := Host}}) when is_binary(Host) ->
    host_and_port(Host);
requested_host(#{host := Host, port := Port}) when is_binary(Host), Host =/= <<>>, is_integer(Port) ->
    {Host, Port};
requested_host(_Req) ->
    undefined.

-spec port(Scheme :: binary(), Port :: non_neg_integer() | undefined) -> non_neg_integer() | undefined.
port(~"http", undefined) -> 80;
port(~"https", undefined) -> 443;
port(_Scheme, Port) -> Port.

-spec is_listed(Origin :: binary(), Allowed :: term()) -> boolean().
is_listed(Origin, [Allowed | Rest]) when is_binary(Allowed) ->
    ascii_lowercase(Allowed) =:= Origin orelse is_listed(Origin, Rest);
is_listed(Origin, [_NotAnOrigin | Rest]) ->
    is_listed(Origin, Rest);
is_listed(_Origin, _NoMore) ->
    false.

%% Header values may hold any byte, so this folds ASCII only.
-spec ascii_lowercase(binary()) -> binary().
ascii_lowercase(Value) ->
    << <<(ascii_lowercase_byte(Byte))>> || <<Byte>> <= Value >>.

ascii_lowercase_byte(Byte) when Byte >= $A, Byte =< $Z -> Byte + 32;
ascii_lowercase_byte(Byte) -> Byte.
