-module(nova_request_plugin).
-behaviour(nova_plugin).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_MAX_FILE_SIZE, 8000000).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                         {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
                         {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, Options, State) ->
    ListOptions = maps:to_list(Options),
    %% Read the body and put it into the Req object
    case read_request_body(Req, ListOptions) of
        {ok, BodyReq} ->
            modulate_state(BodyReq, ListOptions, State);
        {stop, Req0} ->
            {stop, Req0, State}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
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
                  {read_multipart_body, <<"Reads a `multipart/form-data` body and puts the regular fields under `params` and the uploaded files under `files`. Takes either `true` or a map of options where `max_file_size` caps the size of a single part">>}
                 ]
     }.


%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

modulate_state(Req, [], State) ->
    {ok, Req, State};

modulate_state( Req = #{method := Method}, [{decode_json_body, true}|Tail], State) when Method =:= <<"GET">>; Method =:= <<"DELETE">> ->
    modulate_state(Req, Tail, State);
modulate_state(Req = #{headers := #{<<"content-type">> := <<"application/json", _/binary>>}, body := <<>>}, [{decode_json_body, true}|_Tl], State) ->
    Req400 = cowboy_req:reply(400, Req),
    logger:warning(#{status_code => 400,
                     msg => "Failed to decode json.",
                     error => "No body to decode."}),
    {stop, Req400, State};
modulate_state(Req = #{headers := #{<<"content-type">> := <<"application/json", _/binary>>}, body := Body}, [{decode_json_body, true}|Tl], State) ->
    %% Decode the data
    JsonLib = nova:get_env(json_lib, thoas),
    case JsonLib:decode(Body) of
        {ok, JSON} ->
            modulate_state(Req#{json => JSON}, Tl, State);
        Error ->
            Req400 = cowboy_req:reply(400, Req),
            logger:warning(#{status_code => 400,
                             msg => "Failed to decode json.",
                             error => Error}),
            {stop, Req400, State}
    end;
modulate_state(#{headers := #{<<"content-type">> := <<"application/x-www-form-urlencoded", _/binary>>}, body := Body} = Req,
               [{read_urlencoded_body, true}|Tl], State) ->
    Data = cow_qs:parse_qs(Body),
    %% First read in the body
    Params = maps:from_list(Data),
    modulate_state(Req#{params => Params}, Tl, State);
modulate_state(Req, [{parse_qs, Type}|T1], State) ->
    Qs = cowboy_req:parse_qs(Req),
    case Type of
        true -> MapQs = maps:from_list(Qs),
                modulate_state(Req#{parsed_qs => MapQs}, T1, State);
        list -> modulate_state(Req#{parsed_qs => Qs}, T1, State)
    end;
modulate_state(Req, [_|Tl], State) ->
    modulate_state(Req, Tl, State).

read_request_body(Req, Options) ->
    case is_multipart(Req) of
        true ->
            case multipart_options(Options) of
                false ->
                    %% Leave the body untouched so the controller can stream
                    %% the parts itself through cowboy_req:read_part/1.
                    {ok, Req#{body => <<>>}};
                MultipartOptions ->
                    read_multipart_body(Req#{body => <<>>}, MultipartOptions)
            end;
        false ->
            case should_read_body(Options) andalso cowboy_req:has_body(Req) of
                true -> {ok, read_body(Req, <<>>)};
                false -> {ok, Req#{body => <<>>}}
            end
    end.

%% Case-insensitive per RFC 9110 8.3.1, delegated to Cowboy's own parser
%% rather than a raw prefix match - a raw match misses `Multipart/Form-Data'
%% and friends, and when that happens with decode_json_body set the body
%% falls through to read_body/2 instead, which has no size cap at all
%% (unlike the max_file_size-bounded multipart path), so this is a DoS gap,
%% not just a compatibility one.
is_multipart(Req) ->
    try cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, _Params} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

multipart_options([]) -> false;
multipart_options([{read_multipart_body, true}|_Tl]) -> #{};
multipart_options([{read_multipart_body, Options}|_Tl]) when is_map(Options) -> Options;
multipart_options([_|Tl]) -> multipart_options(Tl).

read_multipart_body(Req, Options) ->
    MaxFileSize = maps:get(max_file_size, Options, ?DEFAULT_MAX_FILE_SIZE),
    read_parts(Req, MaxFileSize, #{}, []).

read_parts(Req, MaxFileSize, Params, Files) ->
    case cowboy_req:read_part(Req) of
        {done, Req0} ->
            {ok, Req0#{params => Params, files => lists:reverse(Files)}};
        {ok, Headers, Req0} ->
            case part_info(Headers) of
                error ->
                    ?LOG_WARNING(#{status_code => 400,
                                   msg => <<"Failed to read multipart body.">>,
                                   error => <<"Malformed content-disposition in part.">>}),
                    {stop, cowboy_req:reply(400, Req0)};
                PartInfo ->
                    case read_part_body(Req0, MaxFileSize, <<>>) of
                        {ok, Body, Req1} ->
                            {Params0, Files0} = add_part(PartInfo, Body, Params, Files),
                            read_parts(Req1, MaxFileSize, Params0, Files0);
                        {too_large, Req1} ->
                            ?LOG_WARNING(#{status_code => 413,
                                           msg => <<"Failed to read multipart body.">>,
                                           error => <<"Part exceeded max_file_size.">>,
                                           max_file_size => MaxFileSize}),
                            {stop, cowboy_req:reply(413, Req1)}
                    end
            end
    end.

part_info(Headers) ->
    try
        cow_multipart:form_data(Headers)
    catch
        _:_ -> error
    end.

add_part({data, FieldName}, Body, Params, Files) ->
    {Params#{FieldName => Body}, Files};
add_part({file, FieldName, Filename, ContentType}, Body, Params, Files) ->
    File = #{name => FieldName,
             filename => Filename,
             content_type => ContentType,
             body => Body},
    {Params, [File|Files]}.

read_part_body(Req, MaxFileSize, Acc) ->
    case cowboy_req:read_part_body(Req) of
        {ok, Data, Req0} ->
            Body = <<Acc/binary, Data/binary>>,
            case byte_size(Body) > MaxFileSize of
                true -> {too_large, Req0};
                false -> {ok, Body, Req0}
            end;
        {more, Data, Req0} ->
            Body = <<Acc/binary, Data/binary>>,
            case byte_size(Body) > MaxFileSize of
                true -> {too_large, Req0};
                false -> read_part_body(Req0, MaxFileSize, Body)
            end
    end.

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> Req0#{body => <<Acc/binary, Data/binary>>};
        {more, Data, Req0} -> read_body(Req0, <<Acc/binary, Data/binary>>)
    end.

should_read_body([]) -> false;
should_read_body([{decode_json_body, true}|_Tl]) -> true;
should_read_body([{read_urlencoded_body, true}|_Tl]) -> true;
should_read_body([_|Tl]) -> should_read_body(Tl).

-ifdef(TEST).
-compile(export_all).
-endif.
