-module(nova_origin).
-moduledoc false.

%%% The origins a browser may call from, which is what `allowed_origins` of
%%% the nova environment says, and what nova_ws_origin_plugin and
%%% nova_csrf_plugin take as an option of the same name.

-export([configured/0, allowed/1, is_allowed/3]).

-include_lib("kernel/include/logger.hrl").

-type allowed() :: same_host | same_origin | any | [iodata()].
-export_type([allowed/0]).

-spec configured() -> allowed().
configured() ->
    allowed(application:get_env(nova, allowed_origins, same_host)).

%% What allowed_origins may be, with the origins of a list as binaries.
-spec allowed(Value :: term()) -> same_host | same_origin | any | [binary()].
allowed(same_host) ->
    same_host;
allowed(same_origin) ->
    same_origin;
allowed(any) ->
    any;
allowed(Value) ->
    case is_list(Value) andalso origins(Value, []) of
        {ok, Origins} ->
            Origins;
        _NotOrigins ->
            ?LOG_ERROR(#{msg => ~"allowed_origins must be same_host, same_origin, any or a list of origins",
                         value => Value}),
            throw({error, {invalid_allowed_origins, Value}})
    end.

origins([], Origins) ->
    {ok, lists:reverse(Origins)};
origins([Origin | Rest], Origins) when is_binary(Origin) ->
    origins(Rest, [Origin | Origins]);
origins([Origin | Rest], Origins) when is_list(Origin) ->
    Chars = [Char || Char <- Origin, is_integer(Char)],
    case length(Chars) =:= length(Origin) andalso unicode:characters_to_binary(Chars) of
        Binary when is_binary(Binary) -> origins(Rest, [Binary | Origins]);
        _NotAString -> error
    end;
origins(_NotOrigins, _Origins) ->
    error.

%% A request without an Origin header is not from a page, or from a browser
%% that says nothing about where it is, and is allowed.
-spec is_allowed(Origin :: binary() | undefined, Req :: cowboy_req:req(), Allowed :: allowed()) -> boolean().
is_allowed(undefined, _Req, _Allowed) ->
    true;
is_allowed(_Origin, _Req, any) ->
    true;
is_allowed(Origin, Req, same_host) ->
    case binary:split(ascii_lowercase(Origin), ~"://") of
        [Scheme, Authority] ->
            case cowboy_req:header(~"host", Req) of
                undefined -> is_request_authority(Scheme, Authority, Req);
                Host -> Authority =:= ascii_lowercase(Host)
            end;
        _NoAuthority ->
            false
    end;
is_allowed(Origin, Req, same_origin) ->
    case binary:split(ascii_lowercase(Origin), ~"://") of
        [Scheme, Authority] ->
            Scheme =:= cowboy_req:scheme(Req) andalso is_request_authority(Scheme, Authority, Req);
        _NoAuthority ->
            false
    end;
is_allowed(Origin, _Req, Allowed) ->
    lists:member(ascii_lowercase(Origin),
                 [ascii_lowercase(iolist_to_binary(AllowedOrigin)) || AllowedOrigin <- Allowed]).

%% HTTP/2 and HTTP/3 send the authority outside the headers. Cowboy keeps it
%% as host and port, with the port of the scheme where none was sent.
is_request_authority(Scheme, Authority, Req) ->
    try cow_http_hd:parse_host(Authority) of
        {Host, Port} ->
            Host =:= cowboy_req:host(Req) andalso origin_port(Scheme, Port) =:= cowboy_req:port(Req)
    catch
        _Class:_Reason -> false
    end.

origin_port(~"http", undefined) -> 80;
origin_port(~"https", undefined) -> 443;
origin_port(_Scheme, Port) -> Port.

%% Header values may hold any byte, so this folds ASCII only.
ascii_lowercase(Value) ->
    << <<(ascii_lowercase_byte(Byte))>> || <<Byte>> <= Value >>.

ascii_lowercase_byte(Byte) when Byte >= $A, Byte =< $Z -> Byte + 32;
ascii_lowercase_byte(Byte) -> Byte.
