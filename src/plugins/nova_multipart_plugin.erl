%%%-------------------------------------------------------------------
%%% @doc
%%% Streams a `multipart/form-data' body to a user-supplied
%%% `nova_multipart_handler', part by part and chunk by chunk, instead of
%%% buffering it in memory the way `nova_request_plugin's `read_multipart_body'
%%% option does. Meant for large or many-file uploads where holding the whole
%%% body (or even one whole part) in memory is the thing to avoid.
%%%
%%% Every part is routed by whether Cowboy classifies it as `{file, ...}'
%%% (a `content-disposition' with a `filename=' parameter) or `{data, ...}'.
%%% A client fully controls that choice, so a "regular field" is not
%%% inherently small - it is still bounded, by `max_field_size', to stop an
%%% attacker from omitting `filename=' to route an upload through the
%%% buffered `params' path instead of the streaming handler.
%%%
%%% Configure with exactly one of `nova_request_plugin's `read_multipart_body'
%%% or this plugin in a request's `pre_request' chain, never both - the
%%% request body is a one-shot stream and a second attempt to read it fails
%%% (surfaced by Cowboy, not swallowed here), which nova turns into a 500 via
%%% the normal plugin-error path.
%%%
%%% On a non-multipart request this plugin leaves `Req' untouched (no
%%% `params'/`files' keys added) rather than forcing them to empty, so it
%%% never clobbers a `params' map a preceding plugin (e.g.
%%% `nova_request_plugin's `read_urlencoded_body') already put there.
%%% @end
%%%-------------------------------------------------------------------
-module(nova_multipart_plugin).
-behaviour(nova_plugin).

-include_lib("kernel/include/logger.hrl").

-export([pre_request/4, post_request/4, plugin_info/0]).

-define(DEFAULT_MAX_PARTS, 32).
-define(DEFAULT_MAX_PART_SIZE, 8000000).
-define(DEFAULT_MAX_FIELD_SIZE, 65536).
-define(DEFAULT_MAX_TOTAL_SIZE, 64000000).
-define(DEFAULT_READ_TIMEOUT, 60000).
-define(CHUNK_LENGTH, 64000).
-define(CHUNK_PERIOD, 15000).
-define(LABEL_MAX_LEN, 64).

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback. Options:
%%   handler => {Mod, InitArgs}            (required, nova_multipart_handler)
%%   max_parts => pos_integer()            (default 32)
%%   max_part_size => pos_integer()        (default 8 000 000 bytes)
%%   max_field_size => pos_integer()       (default 65 536 bytes)
%%   max_total_size => pos_integer()       (default 64 000 000 bytes)
%%   read_timeout => pos_integer()         (default 60 000 ms, whole request)
%% @end
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
          {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, #{handler := {Mod, InitArgs}} = Options, State) ->
    case is_multipart(Req) of
        false ->
            {ok, Req, State};
        true ->
            Limits = limits(Options),
            Deadline = erlang:monotonic_time(millisecond) + maps:get(read_timeout, Limits),
            case stream_parts(Req, Mod, InitArgs, Limits, Deadline, 0, 0, #{}, []) of
                {ok, Req0, Params, Files} ->
                    ExistingParams = maps:get(params, Req0, #{}),
                    {ok, Req0#{params => maps:merge(ExistingParams, Params), files => Files}, State};
                {stop, Req0} ->
                    {stop, Req0, State}
            end
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
    #{title => <<"Nova multipart streaming plugin">>,
      version => <<"0.1.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Streams multipart/form-data file parts to a user-supplied handler instead of buffering them in memory.">>,
      options => [
                  {handler, <<"Required. {Mod, InitArgs} implementing nova_multipart_handler">>},
                  {max_parts, <<"Abort with 413 after this many parts (default 32)">>},
                  {max_part_size, <<"Abort a file part with 413 past this many bytes (default 8 000 000)">>},
                  {max_field_size, <<"Abort a non-file field with 413 past this many bytes (default 65 536)">>},
                  {max_total_size, <<"Abort the whole request with 413 past this many bytes across all parts (default 64 000 000)">>},
                  {read_timeout, <<"Abort with 408 if the whole multipart body isn't read within this many ms (default 60 000)">>}
                 ]
     }.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

%% Case-insensitive per RFC 9110 8.3.1 and delegated to Cowboy's own parser
%% rather than a raw prefix match, so this can't be walked around by a
%% client sending `Multipart/Form-Data' or `MULTIPART/FORM-DATA'. A
%% boundary-less `multipart/form-data' is treated as not-multipart (400 via
%% the normal urlencoded/json path) instead of letting Cowboy's own
%% read_part/1 exit with a 500 later.
is_multipart(Req) ->
    try cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, Params} ->
            lists:keymember(<<"boundary">>, 1, Params);
        _ ->
            false
    catch
        _:_ -> false
    end.

limits(Options) ->
    #{max_parts => maps:get(max_parts, Options, ?DEFAULT_MAX_PARTS),
      max_part_size => maps:get(max_part_size, Options, ?DEFAULT_MAX_PART_SIZE),
      max_field_size => maps:get(max_field_size, Options, ?DEFAULT_MAX_FIELD_SIZE),
      max_total_size => maps:get(max_total_size, Options, ?DEFAULT_MAX_TOTAL_SIZE),
      read_timeout => maps:get(read_timeout, Options, ?DEFAULT_READ_TIMEOUT)}.

deadline_expired(Deadline) ->
    erlang:monotonic_time(millisecond) > Deadline.

stream_parts(Req, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files) ->
    case deadline_expired(Deadline) of
        true ->
            warn(408, <<"Multipart read deadline exceeded.">>, #{}),
            {stop, cowboy_req:reply(408, Req)};
        false ->
            case PartCount >= maps:get(max_parts, Limits) of
                true ->
                    warn(413, <<"Too many multipart parts.">>, #{max_parts => maps:get(max_parts, Limits)}),
                    {stop, cowboy_req:reply(413, Req)};
                false ->
                    read_next_part(Req, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files)
            end
    end.

read_next_part(Req, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files) ->
    case safe_read_part(Req) of
        {done, Req0} ->
            {ok, Req0, Params, lists:reverse(Files)};
        {ok, Headers, Req0} ->
            case part_info(Headers) of
                error ->
                    warn(400, <<"Malformed content-disposition in part.">>, #{}),
                    {stop, cowboy_req:reply(400, Req0)};
                {data, FieldName} ->
                    read_field(Req0, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files, FieldName);
                {file, FieldName, Filename, ContentType} ->
                    read_file_part(Req0, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files,
                                   #{name => FieldName, filename => Filename, content_type => ContentType})
            end;
        {error, Reason, Req0} ->
            warn(error_status(Reason), <<"Cowboy raised while reading multipart headers.">>, #{error => safe_label(Reason)}),
            {stop, cowboy_req:reply(error_status(Reason), Req0)}
    end.

read_field(Req, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files, FieldName) ->
    MaxFieldSize = maps:get(max_field_size, Limits),
    case read_bounded_body(Req, MaxFieldSize, Deadline, <<>>) of
        {ok, Body, Req0} ->
            NewTotal = TotalSize + byte_size(Body),
            case NewTotal > maps:get(max_total_size, Limits) of
                true ->
                    warn(413, <<"Multipart request exceeded max_total_size.">>, #{}),
                    {stop, cowboy_req:reply(413, Req0)};
                false ->
                    stream_parts(Req0, Mod, InitArgs, Limits, Deadline, PartCount + 1, NewTotal,
                                 Params#{FieldName => Body}, Files)
            end;
        {too_large, Req0} ->
            warn(413, <<"Multipart field exceeded max_field_size.">>,
                 #{field => safe_label(FieldName), max_field_size => MaxFieldSize}),
            {stop, cowboy_req:reply(413, Req0)};
        {timeout, Req0} ->
            warn(408, <<"Multipart field read deadline exceeded.">>, #{field => safe_label(FieldName)}),
            {stop, cowboy_req:reply(408, Req0)};
        {error, Reason, Req0} ->
            warn(error_status(Reason), <<"Cowboy raised while reading a multipart field.">>,
                 #{field => safe_label(FieldName), error => safe_label(Reason)}),
            {stop, cowboy_req:reply(error_status(Reason), Req0)}
    end.

read_bounded_body(Req, MaxSize, Deadline, Acc) ->
    case deadline_expired(Deadline) of
        true ->
            {timeout, Req};
        false ->
            case safe_read_part_body(Req, #{length => ?CHUNK_LENGTH, period => ?CHUNK_PERIOD}) of
                {more, Data, Req0} ->
                    Body = <<Acc/binary, Data/binary>>,
                    case byte_size(Body) > MaxSize of
                        true -> {too_large, Req0};
                        false -> read_bounded_body(Req0, MaxSize, Deadline, Body)
                    end;
                {ok, Data, Req0} ->
                    Body = <<Acc/binary, Data/binary>>,
                    case byte_size(Body) > MaxSize of
                        true -> {too_large, Req0};
                        false -> {ok, Body, Req0}
                    end;
                {error, _, _} = Error ->
                    Error
            end
    end.

read_file_part(Req, Mod, InitArgs, Limits, Deadline, PartCount, TotalSize, Params, Files, PartInfo) ->
    case safe_call(Mod, init, [PartInfo, InitArgs]) of
        {ok, HandlerState} ->
            case stream_part_body(Req, Mod, HandlerState, Limits, Deadline, 0, TotalSize) of
                {ok, Result, Req0, NewTotal} ->
                    File = PartInfo#{result => Result},
                    stream_parts(Req0, Mod, InitArgs, Limits, Deadline, PartCount + 1, NewTotal, Params, [File|Files]);
                {aborted, Status, Req0} ->
                    {stop, cowboy_req:reply(Status, Req0)}
            end;
        {error, Reason} ->
            warn(400, <<"Handler rejected part.">>, #{error => safe_label(Reason)}),
            {stop, cowboy_req:reply(400, Req)}
    end.

stream_part_body(Req, Mod, HandlerState, Limits, Deadline, PartSize, TotalSize) ->
    case deadline_expired(Deadline) of
        true ->
            safe_abort(Mod, timeout, HandlerState),
            {aborted, 408, Req};
        false ->
            case safe_read_part_body(Req, #{length => ?CHUNK_LENGTH, period => ?CHUNK_PERIOD}) of
                {more, Data, Req0} ->
                    consume_chunk(Data, Req0, Mod, HandlerState, Limits, Deadline, PartSize, TotalSize, more);
                {ok, Data, Req0} ->
                    consume_chunk(Data, Req0, Mod, HandlerState, Limits, Deadline, PartSize, TotalSize, done);
                {error, Reason, Req0} ->
                    warn(error_status(Reason), <<"Cowboy raised while streaming a multipart file.">>,
                         #{error => safe_label(Reason)}),
                    safe_abort(Mod, Reason, HandlerState),
                    {aborted, error_status(Reason), Req0}
            end
    end.

consume_chunk(Data, Req, Mod, HandlerState, Limits, Deadline, PartSize0, TotalSize0, More) ->
    PartSize = PartSize0 + byte_size(Data),
    TotalSize = TotalSize0 + byte_size(Data),
    case PartSize > maps:get(max_part_size, Limits) orelse TotalSize > maps:get(max_total_size, Limits) of
        true ->
            warn(413, <<"Multipart part exceeded a size limit mid-stream.">>, #{}),
            safe_abort(Mod, too_large, HandlerState),
            {aborted, 413, Req};
        false ->
            case safe_call(Mod, handle_data, [Data, HandlerState]) of
                {ok, HandlerState0} ->
                    case More of
                        more ->
                            stream_part_body(Req, Mod, HandlerState0, Limits, Deadline, PartSize, TotalSize);
                        done ->
                            finish_part(Req, Mod, HandlerState0, TotalSize)
                    end;
                {error, Reason} ->
                    warn(500, <<"Multipart handler rejected a chunk.">>, #{error => safe_label(Reason)}),
                    safe_abort(Mod, Reason, HandlerState),
                    {aborted, 500, Req}
            end
    end.

finish_part(Req, Mod, HandlerState, TotalSize) ->
    case safe_call(Mod, handle_end, [HandlerState]) of
        {ok, Result} ->
            {ok, Result, Req, TotalSize};
        {error, Reason} ->
            warn(500, <<"Multipart handler failed to finalize a part.">>, #{error => safe_label(Reason)}),
            safe_abort(Mod, Reason, HandlerState),
            {aborted, 500, Req}
    end.

%% cowboy_req:read_part/1 and read_part_body/2 don't return an error tuple
%% on a stalled/oversize/malformed read - they `exit/1` straight out of the
%% call (see cowboy_req:read_body/2's `after Timeout -> exit(timeout) end`,
%% reached from stream_multipart/3). Cowboy's own `period` (15s here) is
%% shorter than our `read_timeout' (60s default), so that exit is the
%% common way a stalled client actually ends a read - not our own deadline
%% check - and left uncaught it would unwind straight past whichever
%% handler resource (e.g. an open file descriptor) is live at the time,
%% skipping handle_abort/2 entirely. Translate it into the same
%% {error, Reason, Req} shape our own bounds already use, so every caller
%% goes through one cleanup path regardless of who detected the problem.
safe_read_part(Req) ->
    try cowboy_req:read_part(Req) of
        Result -> Result
    catch
        exit:Reason -> {error, Reason, Req}
    end.

safe_read_part_body(Req, Opts) ->
    try cowboy_req:read_part_body(Req, Opts) of
        Result -> Result
    catch
        exit:Reason -> {error, Reason, Req}
    end.

error_status(timeout) -> 408;
error_status({request_error, timeout, _}) -> 408;
error_status({request_error, payload_too_large, _}) -> 413;
error_status({request_error, {multipart, _}, _}) -> 400;
error_status(_) -> 400.

%% Every nova_multipart_handler callback runs through here: a handler that
%% raises instead of returning {error, _} must not crash pre_request/4 (that
%% would skip handle_abort/2 for whichever callback raised) and must not
%% have its stacktrace - which can carry up to a chunk's worth of the
%% client's own uploaded bytes as call arguments - reach the client. The
%% full exception is logged server-side only; callers only ever see the
%% opaque handler_crashed reason.
safe_call(Mod, Fun, Args) ->
    try erlang:apply(Mod, Fun, Args) of
        {ok, _} = Ok -> Ok;
        {error, _} = Error -> Error
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{msg => <<"nova_multipart_handler callback raised.">>,
                        mod => Mod, fun_name => Fun, class => Class,
                        error => Reason, stacktrace => Stacktrace}),
            {error, handler_crashed}
    end.

%% handle_abort/2 runs on an already-failing path - never let a broken
%% handler mask the original error with a crash of its own.
safe_abort(Mod, Reason, HandlerState) ->
    try
        Mod:handle_abort(Reason, HandlerState)
    catch
        Class:AbortReason ->
            ?LOG_WARNING(#{msg => <<"nova_multipart_handler:handle_abort/2 raised.">>,
                           class => Class, error => AbortReason}),
            ok
    end,
    ok.

part_info(Headers) ->
    try
        cow_multipart:form_data(Headers)
    catch
        _:_ -> error
    end.

warn(Status, Msg, Extra) ->
    ?LOG_WARNING(maps:merge(#{status_code => Status, msg => Msg}, Extra)).

%% Client-controlled text (a field name, a handler error reason) heading
%% into a log line: truncate and strip control characters so it can't forge
%% extra report lines or carry invalid UTF-8 into the log encoder.
safe_label(Value) ->
    Bin = to_binary_label(Value),
    Truncated = binary:part(Bin, 0, min(byte_size(Bin), ?LABEL_MAX_LEN)),
    case unicode:characters_to_binary(Truncated) of
        Clean when is_binary(Clean) ->
            re:replace(Clean, <<"[[:cntrl:]]">>, <<".">>, [global, {return, binary}]);
        _ ->
            <<"<invalid-utf8>">>
    end.

to_binary_label(Value) when is_binary(Value) -> Value;
to_binary_label(Value) -> iolist_to_binary(io_lib:format("~0p", [Value])).

-ifdef(TEST).
-compile(export_all).
-endif.
