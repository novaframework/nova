-module(nova_request_plugin).
-moduledoc """
Reads and decodes the body and the query string of a request. Use it as a
`pre_request` plugin with the options for what it should do:

- `decode_json_body => true` - decodes the body of a request whose content
  type is `application/json` or ends in `+json`, and puts it in the request
  under `json`. `GET` and `DELETE` requests are left alone. An empty body or
  one that is not JSON is answered with a `400`.
- `read_urlencoded_body => true` - parses an
  `application/x-www-form-urlencoded` body and puts its fields in the request
  under `params`. A malformed body is answered with a `400`, one with too
  many fields with a `413`.
- `parse_qs => true | list` - parses the query string and puts it in the
  request under `parsed_qs`, as a map or as a list of pairs. A malformed
  query string is answered with a `400`.
- `max_body_length` - a body longer than this many bytes is answered with a
  `413`. Default `1000000`. Takes `infinity`.
- `body_read_timeout` - a body that is not read within this many
  milliseconds is answered with a `408`. Default `60000`. Takes `infinity`.

The body is read only when `decode_json_body` or `read_urlencoded_body` is
set, and is then also in the request under `body`. Otherwise `body` is
`<<>>`. A `multipart/form-data` body is never read here, see
`nova_multipart_plugin`.
""".
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-ifdef(TEST).
-export([modulate_state/3, should_read_body/1]).
-endif.

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_MAX_BODY_LENGTH, 1000000).
-define(DEFAULT_BODY_READ_TIMEOUT, 60000).
-define(CHUNK_LENGTH, 64000).
-define(CHUNK_PERIOD, 15000).

%%--------------------------------------------------------------------
-doc """
Reads the body when an option asks for it and applies the options. Returns
`{ok, Req, State}` with the keys described above set in the request, or a
`stop` with the `400`, `408` or `413` that refuses the request.
""".
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                         {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
                         {stop, Req0 :: cowboy_req:req(), NewState :: any()} |
                         {stop, {status, 400 | 413} | {status, 408 | 413, #{binary() => binary()}},
                          Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, Options, State) ->
    ListOptions = maps:to_list(Options),
    MaxLength = limit(max_body_length, Options, ?DEFAULT_MAX_BODY_LENGTH, 0),
    Timeout = limit(body_read_timeout, Options, ?DEFAULT_BODY_READ_TIMEOUT, 1),
    %% The rest of the body is still on the connection, so it is closed.
    case read_request_body(Req, ListOptions, MaxLength, Timeout) of
        {ok, Req0} -> modulate_state(Req0, ListOptions, State);
        {too_large, Req0} -> {stop, {status, 413, #{~"connection" => ~"close"}}, Req0, State};
        {timeout, Req0} -> {stop, {status, 408, #{~"connection" => ~"close"}}, Req0, State}
    end.

%%--------------------------------------------------------------------
-doc "Does nothing: the plugin works in `pre_request/4`.".
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
-doc "The title, version, description and options of the plugin.".
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
                  {decode_json_body, ~"Decodes a body sent as JSON and puts it in the request under `json`"},
                  {read_urlencoded_body, ~"Parses an application/x-www-form-urlencoded body and puts its fields in the request under `params`"},
                  {parse_qs, ~"Parses the query string and puts it in the request under `parsed_qs`: as a map with true, as a list of pairs with list"},
                  {max_body_length, ~"Reply 413 when the body read for decode_json_body or read_urlencoded_body is longer than this many bytes, or infinity (default 1 000 000)"},
                  {body_read_timeout, ~"Reply 408 when reading that body takes longer than this many milliseconds (at least 1), or infinity (default 60 000)"}
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
    case is_json(content_type(Req)) of
        true -> decode_json(Req, Body, Tl, State);
        false -> modulate_state(Req, Tl, State)
    end;
modulate_state(Req = #{body := Body}, [{read_urlencoded_body, true}|Tl], State) ->
    case content_type(Req) of
        {~"application", ~"x-www-form-urlencoded"} ->
            try maps:from_list(cow_qs:parse_qs(Body)) of
                Params -> modulate_state(Req#{params => Params}, Tl, State)
            catch
                error:limit_reached -> refuse(413, ~"Too many fields in the urlencoded body", Req, State);
                error:_ -> refuse(400, ~"Malformed urlencoded body", Req, State)
            end;
        _ ->
            modulate_state(Req, Tl, State)
    end;
modulate_state(Req, [{parse_qs, Type}|Tl], State) when Type =:= true; Type =:= list ->
    try cowboy_req:parse_qs(Req) of
        Qs when Type =:= true -> modulate_state(Req#{parsed_qs => maps:from_list(Qs)}, Tl, State);
        Qs -> modulate_state(Req#{parsed_qs => Qs}, Tl, State)
    catch
        exit:{request_error, _, _} -> refuse(400, ~"Malformed query string", Req, State)
    end;
modulate_state(Req, [_|Tl], State) ->
    modulate_state(Req, Tl, State).

%% What the client sent is left out of the log line.
refuse(Status, Reason, Req, State) ->
    ?LOG_WARNING(#{msg => ~"Request refused", reason => Reason, status_code => Status}),
    {stop, {status, Status}, Req, State}.

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

%% application/json, or any application type with the +json structured
%% syntax suffix (RFC 6839), such as application/vnd.api+json.
is_json({~"application", ~"json"}) ->
    true;
is_json({~"application", SubType}) when byte_size(SubType) > 5 ->
    binary:part(SubType, byte_size(SubType), -5) =:= ~"+json";
is_json(_) ->
    false.

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
read_request_body(Req, Options, MaxLength, Timeout) ->
    case not is_multipart(Req) andalso should_read_body(Options) andalso cowboy_req:has_body(Req) of
        true -> read_body(Req, MaxLength, Timeout);
        false -> {ok, Req#{body => <<>>}}
    end.

-spec limit(atom(), map(), non_neg_integer(), non_neg_integer()) -> non_neg_integer() | infinity.
limit(Key, Options, Default, Min) ->
    case maps:get(Key, Options, Default) of
        infinity -> infinity;
        Value when is_integer(Value), Value >= Min -> Value;
        Value -> invalid_limit(Key, Value, Default)
    end.

%% Once per option value, so a misconfigured route does not log per request.
invalid_limit(Key, Value, Default) ->
    Warned = {?MODULE, invalid_limit, Key, Value},
    case persistent_term:get(Warned, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Warned, true),
            ?LOG_WARNING(#{msg => ~"Invalid nova_request_plugin option, using the default",
                           option => Key, value => Value, default => Default})
    end,
    Default.

is_multipart(Req) ->
    content_type(Req) =:= {~"multipart", ~"form-data"}.

%% A declared length over the limit is refused before anything is read. A
%% chunked body is read until it passes the limit; Cowboy returns once it has
%% the bytes asked for, so no more than one past the limit is waited for. The
%% whole read has a deadline, so a body trickling in cannot hold the request.
read_body(Req, MaxLength, Timeout) ->
    Deadline = case Timeout of
                   infinity -> infinity;
                   _ -> erlang:monotonic_time(millisecond) + Timeout
               end,
    case cowboy_req:body_length(Req) of
        Length when is_integer(Length), MaxLength =/= infinity, Length > MaxLength -> {too_large, Req};
        _ -> read_body(Req, MaxLength, Deadline, <<>>)
    end.

-spec read_body(cowboy_req:req(), non_neg_integer() | infinity, integer() | infinity, binary()) ->
          {ok | too_large | timeout, cowboy_req:req()}.
read_body(Req, MaxLength, Deadline, Acc) ->
    case time_left(Deadline) of
        0 ->
            {timeout, Req};
        TimeLeft ->
            ReadLength = case MaxLength of
                             infinity -> ?CHUNK_LENGTH;
                             _ -> min(?CHUNK_LENGTH, MaxLength - byte_size(Acc) + 1)
                         end,
            Period = case TimeLeft of
                         infinity -> ?CHUNK_PERIOD;
                         _ -> min(?CHUNK_PERIOD, TimeLeft)
                     end,
            {Status, Data, Req0} = cowboy_req:read_body(Req, #{length => ReadLength, period => Period}),
            Body = <<Acc/binary, Data/binary>>,
            case {Status, MaxLength =/= infinity andalso byte_size(Body) > MaxLength} of
                {_, true} -> {too_large, Req0};
                {ok, false} -> {ok, Req0#{body => Body}};
                {more, false} -> read_body(Req0, MaxLength, Deadline, Body)
            end
    end.

-spec time_left(integer() | infinity) -> non_neg_integer() | infinity.
time_left(infinity) ->
    infinity;
time_left(Deadline) ->
    case Deadline - erlang:monotonic_time(millisecond) of
        Left when Left > 0 -> Left;
        _ -> 0
    end.

should_read_body([]) -> false;
should_read_body([{decode_json_body, true}|_Tl]) -> true;
should_read_body([{read_urlencoded_body, true}|_Tl]) -> true;
should_read_body([_|Tl]) -> should_read_body(Tl).
