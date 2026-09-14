%%%-------------------------------------------------------------------
%%% @doc
%%% Reads a `multipart/form-data' body and streams every file part to a
%%% `nova_multipart_handler', chunk by chunk. Regular fields are collected
%%% under `params' and file parts under `files', each as
%%% `#{name, filename, content_type, result}' where `result' is whatever the
%%% handler returned from `handle_end/1'.
%%%
%%% Multipart fields win over `params' set by an earlier plugin, and a
%%% repeated field name keeps the last value. On a non-multipart request the
%%% plugin sets `files' to `[]' and `params' to `#{}' unless an earlier
%%% plugin already set it, so a controller can match both keys on every
%%% request that reaches it.
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

-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
          {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, #{handler := {Mod, InitArgs}} = Options, State) when is_atom(Mod) ->
    case content_type(Req) of
        other ->
            {ok, Req#{params => maps:get(params, Req, #{}), files => []}, State};
        {multipart, no_boundary} ->
            warn(400, <<"multipart/form-data without a boundary parameter.">>, #{}),
            {stop, cowboy_req:reply(400, Req), State};
        multipart ->
            Limits = limits(Options),
            Deadline = erlang:monotonic_time(millisecond) + maps:get(read_timeout, Limits),
            case stream_parts(Req, Mod, InitArgs, Limits, Deadline, 0, 0, #{}, []) of
                {ok, Req0, Params, Files} ->
                    ExistingParams = maps:get(params, Req0, #{}),
                    {ok, Req0#{params => maps:merge(ExistingParams, Params), files => Files}, State};
                {stop, Req0} ->
                    {stop, Req0, State}
            end
    end;
pre_request(Req, _Env, Options, State) ->
    ?LOG_ERROR(#{msg => <<"nova_multipart_plugin needs handler => {Mod, InitArgs}.">>,
                 options => Options}),
    {stop, cowboy_req:reply(500, Req), State}.

-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{title => <<"Nova multipart plugin">>,
      version => <<"0.1.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Reads multipart/form-data bodies. Fields go under `params`, files are streamed to a nova_multipart_handler and listed under `files`.">>,
      options => [
                  {handler, <<"Required. {Mod, InitArgs} implementing nova_multipart_handler. nova_multipart_file_handler spools to disk, nova_multipart_memory_handler buffers in memory">>},
                  {max_parts, <<"Reply 413 after this many parts (default 32)">>},
                  {max_part_size, <<"Reply 413 past this many bytes in one file part (default 8 000 000)">>},
                  {max_field_size, <<"Reply 413 past this many bytes in one non-file field (default 65 536)">>},
                  {max_total_size, <<"Reply 413 past this many bytes across all parts (default 64 000 000)">>},
                  {read_timeout, <<"Reply 408 if the body is not fully read within this many ms (default 60 000). Checked between chunks, so a stalled read can overrun by up to 15 s">>}
                 ]
     }.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

content_type(Req) ->
    try cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, Params} ->
            case has_boundary(Params) of
                true -> multipart;
                false -> {multipart, no_boundary}
            end;
        _ ->
            other
    catch
        _:_ -> other
    end.

has_boundary([{<<"boundary">>, _}|_]) -> true;
has_boundary([_|Tl]) -> has_boundary(Tl);
has_boundary(_) -> false.

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
            Status = error_status(Reason),
            warn(Status, <<"Failed to read multipart headers.">>, #{error => safe_label(Reason)}),
            {stop, cowboy_req:reply(Status, Req0)}
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
            Status = error_status(Reason),
            warn(Status, <<"Failed to read a multipart field.">>,
                 #{field => safe_label(FieldName), error => safe_label(Reason)}),
            {stop, cowboy_req:reply(Status, Req0)}
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
        {reject, Reason} ->
            warn(400, <<"Handler rejected part.">>, #{error => safe_label(Reason)}),
            {stop, cowboy_req:reply(400, Req)};
        {error, Reason} ->
            warn(500, <<"Handler failed to init.">>, #{error => safe_label(Reason)}),
            {stop, cowboy_req:reply(500, Req)}
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
                    Status = error_status(Reason),
                    warn(Status, <<"Failed to read a multipart file part.">>, #{error => safe_label(Reason)}),
                    safe_abort(Mod, Reason, HandlerState),
                    {aborted, Status, Req0}
            end
    end.

consume_chunk(Data, Req, Mod, HandlerState, Limits, Deadline, PartSize0, TotalSize0, More) ->
    PartSize = PartSize0 + byte_size(Data),
    TotalSize = TotalSize0 + byte_size(Data),
    case PartSize > maps:get(max_part_size, Limits) orelse TotalSize > maps:get(max_total_size, Limits) of
        true ->
            warn(413, <<"Multipart part exceeded a size limit.">>, #{}),
            safe_abort(Mod, too_large, HandlerState),
            {aborted, 413, Req};
        false ->
            case safe_call(Mod, handle_data, [Data, HandlerState]) of
                {ok, HandlerState0} when More =:= more ->
                    stream_part_body(Req, Mod, HandlerState0, Limits, Deadline, PartSize, TotalSize);
                {ok, HandlerState0} ->
                    finish_part(Req, Mod, HandlerState0, TotalSize);
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

%% cowboy_req:read_part/1 and read_part_body/2 exit on a stalled or malformed
%% read, and raise error:badmatch on duplicate part headers. Both are turned
%% into {error, Reason, Req} so every caller cleans up through one path.
safe_read_part(Req) ->
    try cowboy_req:read_part(Req)
    catch
        exit:Reason -> {error, Reason, Req};
        error:Reason -> {error, Reason, Req}
    end.

safe_read_part_body(Req, Opts) ->
    try cowboy_req:read_part_body(Req, Opts)
    catch
        exit:Reason -> {error, Reason, Req};
        error:Reason -> {error, Reason, Req}
    end.

error_status(timeout) -> 408;
error_status({request_error, timeout, _}) -> 408;
error_status({request_error, payload_too_large, _}) -> 413;
error_status({request_error, {multipart, _}, _}) -> 400;
error_status({badmatch, _}) -> 400;
error_status(_) -> 500.

%% A handler that raises or returns something other than {ok, _} | {error, _}
%% must not unwind past the abort path. The exception stays in the log; the
%% caller only sees handler_crashed.
safe_call(Mod, Fun, Args) ->
    Result = try erlang:apply(Mod, Fun, Args)
             catch
                 Class:Reason:Stacktrace ->
                     ?LOG_ERROR(#{msg => <<"nova_multipart_handler callback raised.">>,
                                  mod => Mod, fun_name => Fun, class => Class,
                                  error => Reason, stacktrace => Stacktrace}),
                     {error, handler_crashed}
             end,
    case Result of
        {ok, _} -> Result;
        {reject, _} when Fun =:= init -> Result;
        {error, _} -> Result;
        Other ->
            ?LOG_ERROR(#{msg => <<"nova_multipart_handler callback returned an invalid value.">>,
                         mod => Mod, fun_name => Fun, value => Other}),
            {error, handler_crashed}
    end.

safe_abort(Mod, Reason, HandlerState) ->
    try
        Mod:handle_abort(Reason, HandlerState)
    catch
        Class:AbortReason ->
            ?LOG_WARNING(#{msg => <<"nova_multipart_handler:handle_abort/2 raised.">>,
                           class => Class, error => AbortReason})
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

%% Client-controlled text heading into a log line: truncated and stripped of
%% control characters.
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
