-module(nova_request_plugin).
-moduledoc false.
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-define(DEFAULT_MAX_BODY_LENGTH, 8000000).
-define(CHUNK_LENGTH, 64000).
-define(CHUNK_PERIOD, 15000).

%%--------------------------------------------------------------------
-doc "Pre-request callback".
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                         {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
                         {stop, Req0 :: cowboy_req:req(), NewState :: any()} |
                         {stop, {status, 413}, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, Options, State) ->
    ListOptions = maps:to_list(Options),
    case read_request_body(Req, ListOptions, max_body_length(Options)) of
        {ok, Req0} -> modulate_state(Req0, ListOptions, State);
        {too_large, Req0} -> {stop, {status, 413}, Req0, State}
    end.

%%--------------------------------------------------------------------
-doc "Post-request callback".
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
-doc "nova_plugin callback. Returns information about the plugin.".
-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{title => <<"Nova body plugin">>,
      version => <<"0.0.1">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"This plugin modulates the body of a request.">>,
      options => [
                  {decode_json_body, <<"Decodes the body as JSON and puts it under `json`">>},
                  {read_urlencoded_body, <<"Used to parse body as query-string and put them in state under `qs` key">>},
                  {max_body_length, ~"Reply 413 when the body read for decode_json_body or read_urlencoded_body is longer than this many bytes, or infinity (default 8 000 000)"}
                 ]
     }.


%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

modulate_state(Req, [], State) ->
    {ok, Req, State};

modulate_state( Req = #{method := Method}, [{decode_json_body, true}|Tail], State) when Method =:= <<"GET">>; Method =:= <<"DELETE">> ->
    modulate_state(Req, Tail, State);
modulate_state(Req = #{body := Body}, [{decode_json_body, true}|Tl], State) ->
    case content_type(Req) of
        {~"application", ~"json"} -> decode_json(Req, Body, Tl, State);
        _ -> modulate_state(Req, Tl, State)
    end;
modulate_state(Req = #{body := Body}, [{read_urlencoded_body, true}|Tl], State) ->
    case content_type(Req) of
        {~"application", ~"x-www-form-urlencoded"} ->
            Params = maps:from_list(cow_qs:parse_qs(Body)),
            modulate_state(Req#{params => Params}, Tl, State);
        _ ->
            modulate_state(Req, Tl, State)
    end;
modulate_state(Req, [{parse_qs, Type}|T1], State) ->
    Qs = cowboy_req:parse_qs(Req),
    case Type of
        true -> MapQs = maps:from_list(Qs),
                modulate_state(Req#{parsed_qs => MapQs}, T1, State);
        list -> modulate_state(Req#{parsed_qs => Qs}, T1, State)
    end;
modulate_state(Req, [_|Tl], State) ->
    modulate_state(Req, Tl, State).

decode_json(Req, <<>>, _Tl, State) ->
    Req400 = cowboy_req:reply(400, Req),
    logger:warning(#{status_code => 400,
                     msg => "Failed to decode json.",
                     error => "No body to decode."}),
    {stop, Req400, State};
decode_json(Req, Body, Tl, State) ->
    case nova_json:decode(Body) of
        {ok, JSON} ->
            modulate_state(Req#{json => JSON}, Tl, State);
        Error ->
            Req400 = cowboy_req:reply(400, Req),
            logger:warning(#{status_code => 400,
                             msg => "Failed to decode json.",
                             error => Error}),
            {stop, Req400, State}
    end.

%% cowboy_req:parse_header/2 lowercases the type and subtype, which are
%% case-insensitive (RFC 9110 8.3.1).
content_type(Req) ->
    try cowboy_req:parse_header(~"content-type", Req) of
        {Type, SubType, _Params} -> {Type, SubType};
        _ -> undefined
    catch
        _:_ -> undefined
    end.

%% A multipart body is never buffered here; nova_multipart_plugin or the
%% controller reads the parts from the stream.
read_request_body(Req, Options, MaxLength) ->
    case not is_multipart(Req) andalso should_read_body(Options) andalso cowboy_req:has_body(Req) of
        true -> read_body(Req, MaxLength);
        false -> {ok, Req#{body => <<>>}}
    end.

max_body_length(Options) ->
    case maps:get(max_body_length, Options, ?DEFAULT_MAX_BODY_LENGTH) of
        infinity -> infinity;
        Length when is_integer(Length), Length >= 0 -> Length;
        _ -> ?DEFAULT_MAX_BODY_LENGTH
    end.

is_multipart(Req) ->
    content_type(Req) =:= {~"multipart", ~"form-data"}.

%% A declared length over the limit is refused before anything is read. A
%% chunked body is read until it passes the limit; Cowboy returns once it has
%% the bytes asked for, so no more than one past the limit is waited for.
read_body(Req, MaxLength) ->
    case cowboy_req:body_length(Req) of
        Length when is_integer(Length), MaxLength =/= infinity, Length > MaxLength -> {too_large, Req};
        _ -> read_body(Req, MaxLength, <<>>)
    end.

read_body(Req, MaxLength, Acc) ->
    ReadLength = case MaxLength of
                     infinity -> ?CHUNK_LENGTH;
                     _ -> min(?CHUNK_LENGTH, MaxLength - byte_size(Acc) + 1)
                 end,
    {Status, Data, Req0} = cowboy_req:read_body(Req, #{length => ReadLength, period => ?CHUNK_PERIOD}),
    Body = <<Acc/binary, Data/binary>>,
    case {Status, MaxLength =/= infinity andalso byte_size(Body) > MaxLength} of
        {_, true} -> {too_large, Req0};
        {ok, false} -> {ok, Req0#{body => Body}};
        {more, false} -> read_body(Req0, MaxLength, Body)
    end.

should_read_body([]) -> false;
should_read_body([{decode_json_body, true}|_Tl]) -> true;
should_read_body([{read_urlencoded_body, true}|_Tl]) -> true;
should_read_body([_|Tl]) -> should_read_body(Tl).

-ifdef(TEST).
-compile(export_all).
-endif.
