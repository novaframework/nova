-module(nova_origin).
-moduledoc false.

-export([is_allowed/3]).

-spec is_allowed(Origin :: binary() | undefined, Req :: cowboy_req:req(), Allowed :: term()) -> boolean().
is_allowed(undefined, _Req, _Allowed) ->
    true;
is_allowed(_Origin, _Req, any) ->
    true;
is_allowed(Origin, Req, same_host) when is_binary(Origin) ->
    case origin(Origin) of
        {Scheme, Host, Port} -> is_as_safe_as(Scheme, Req) andalso is_sent_to(Scheme, Host, Port, Req);
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

-spec host_and_port(binary()) -> {Host :: binary(), Port :: non_neg_integer() | undefined} | undefined.
host_and_port(Authority) ->
    try cow_http_hd:parse_host(Authority) of
        {Host, Port} when Host =/= <<>> -> {Host, Port};
        _NoHost -> undefined
    catch
        _Class:_Reason -> undefined
    end.

-spec is_as_safe_as(Scheme :: binary(), cowboy_req:req()) -> boolean().
is_as_safe_as(Scheme, #{scheme := ~"https"}) ->
    Scheme =:= ~"https";
is_as_safe_as(_Scheme, _Req) ->
    true.

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

-spec ascii_lowercase(binary()) -> binary().
ascii_lowercase(Value) ->
    << <<(ascii_lowercase_byte(Byte))>> || <<Byte>> <= Value >>.

ascii_lowercase_byte(Byte) when Byte >= $A, Byte =< $Z -> Byte + 32;
ascii_lowercase_byte(Byte) -> Byte.
