-module(nova_multipart_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

plugin_info_test() ->
    Info = nova_multipart_plugin:plugin_info(),
    ?assertEqual(<<"Nova multipart streaming plugin">>, maps:get(title, Info)),
    ?assert(is_list(maps:get(options, Info))).

deadline_expired_test() ->
    Now = erlang:monotonic_time(millisecond),
    ?assert(nova_multipart_plugin:deadline_expired(Now - 1)),
    ?assertNot(nova_multipart_plugin:deadline_expired(Now + 100000)).

non_multipart_req_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/upload">>),
    Req0 = nova_test_helper:with_content_type(<<"application/json">>, Req),
    Options = #{handler => {nova_multipart_test_handler, #{owner => self()}}},
    %% must be a no-op: no params/files added, and nothing already on Req
    %% (e.g. params from an earlier plugin) gets clobbered
    {ok, Req1, state} = nova_multipart_plugin:pre_request(Req0, env, Options, state),
    ?assertEqual(Req0, Req1).

non_multipart_req_preserves_existing_params_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/upload">>),
    Req0 = nova_test_helper:with_content_type(<<"application/json">>, Req),
    Req1 = Req0#{params => #{<<"csrf">> => <<"token">>}},
    Options = #{handler => {nova_multipart_test_handler, #{owner => self()}}},
    {ok, Req2, state} = nova_multipart_plugin:pre_request(Req1, env, Options, state),
    ?assertEqual(#{<<"csrf">> => <<"token">>}, maps:get(params, Req2)).

case_insensitive_content_type_test_() ->
    Parts = [{data_part(<<"title">>), <<"a picture">>}],
    {setup,
     fun() -> mock_cowboy_req(Parts) end,
     fun(_) -> meck:unload(cowboy_req) end,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/upload">>),
         Req0 = nova_test_helper:with_content_type(
                  <<"MULTIPART/FORM-DATA; boundary=----abc">>, Req),
         Options = #{handler => {nova_multipart_test_handler, #{owner => self()}}},
         {ok, Req1, state} = nova_multipart_plugin:pre_request(Req0#{has_body => true}, env, Options, state),
         ?assertEqual(#{<<"title">> => <<"a picture">>}, maps:get(params, Req1))
     end}.

%%====================================================================
%% cowboy_req scripting - same pattern as nova_request_plugin_tests
%%====================================================================

multipart_req() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/upload">>),
    Req0 = nova_test_helper:with_content_type(
             <<"multipart/form-data; boundary=----abc">>, Req),
    Req0#{has_body => true}.

%% Parts is a list of {Headers, Body} where Body is a binary or a list of
%% binaries to be delivered as {more, ...} chunks off cowboy_req:read_part_body/2.
mock_cowboy_req(Parts) ->
    meck:new(cowboy_req, [passthrough]),
    Pid = spawn(fun() -> part_server(Parts) end),
    meck:expect(cowboy_req, read_part,
                fun(Req) ->
                    case call(Pid, next_part) of
                        done -> {done, Req};
                        {part, Headers} -> {ok, Headers, Req}
                    end
                end),
    meck:expect(cowboy_req, read_part_body,
                fun(Req, _Opts) ->
                    %% mirrors cowboy_req:read_part_body/2's own behaviour on
                    %% a stalled/oversize/malformed read: it exits rather
                    %% than returning an {error, _} tuple.
                    case call(Pid, next_chunk) of
                        {last, {raise, Reason}} -> exit(Reason);
                        {more, {raise, Reason}} -> exit(Reason);
                        {last, Data} -> {ok, Data, Req};
                        {more, Data} -> {more, Data, Req}
                    end
                end),
    meck:expect(cowboy_req, reply, fun(Status, Req) -> Req#{replied => Status} end).

call(Pid, Msg) ->
    Pid ! {self(), Msg},
    receive {Pid, Reply} -> Reply end.

part_server(Parts) ->
    part_server(Parts, []).

part_server(Parts, Chunks) ->
    receive
        {From, next_part} ->
            case Parts of
                [] ->
                    From ! {self(), done},
                    part_server([], []);
                [{Headers, Body}|Tl] ->
                    From ! {self(), {part, Headers}},
                    part_server(Tl, chunks(Body))
            end;
        {From, next_chunk} ->
            case Chunks of
                [Last] ->
                    From ! {self(), {last, Last}},
                    part_server(Parts, []);
                [Hd|Tl] ->
                    From ! {self(), {more, Hd}},
                    part_server(Parts, Tl)
            end
    end.

chunks(Body) when is_binary(Body) -> [Body];
chunks(Body) when is_list(Body) -> Body.

data_part(Name) ->
    #{<<"content-disposition">> =>
          <<"form-data; name=\"", Name/binary, "\"">>}.

file_part(Name, Filename, ContentType) ->
    #{<<"content-disposition">> =>
          <<"form-data; name=\"", Name/binary, "\"; filename=\"", Filename/binary, "\"">>,
      <<"content-type">> => ContentType}.

%% OptionsFun receives the *instantiator's* self() - eunit runs a fixture's
%% generator body (this whole test_/0 function) in a different process than
%% the deferred fun() it returns, so capturing self() outside OptionsFun
%% would hand the handler a pid nothing ever receives on.
multipart_test_(Parts, OptionsFun, Assertions) ->
    {setup,
     fun() -> mock_cowboy_req(Parts) end,
     fun(_) -> meck:unload(cowboy_req) end,
     fun() ->
         Options = OptionsFun(self()),
         Result = nova_multipart_plugin:pre_request(multipart_req(), env, Options, state),
         Assertions(Result)
     end}.

handler_opts(Owner) -> #{owner => Owner}.

multipart_reads_fields_and_files_test_() ->
    Parts = [{data_part(<<"title">>), <<"a picture">>},
             {file_part(<<"upload">>, <<"logo.png">>, <<"image/png">>), <<"binarydata">>}],
    multipart_test_(
      Parts, fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}} end,
      fun({ok, Req, state}) ->
          ?assertEqual(#{<<"title">> => <<"a picture">>}, maps:get(params, Req)),
          [File] = maps:get(files, Req),
          ?assertEqual(<<"upload">>, maps:get(name, File)),
          ?assertEqual(<<"logo.png">>, maps:get(filename, File)),
          ?assertEqual(<<"image/png">>, maps:get(content_type, File)),
          ?assertEqual(#{body => <<"binarydata">>}, maps:get(result, File)),
          %% handler saw the part metadata before any chunk arrived, and the
          %% client-supplied filename never touched a filesystem path here.
          ?assertEqual({handler_init, #{name => <<"upload">>,
                                        filename => <<"logo.png">>,
                                        content_type => <<"image/png">>}},
                        receive_one())
      end).

multipart_joins_chunked_part_test_() ->
    Parts = [{file_part(<<"upload">>, <<"big.bin">>, <<"application/octet-stream">>),
              [<<"one">>, <<"two">>, <<"three">>]}],
    multipart_test_(
      Parts, fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}} end,
      fun({ok, Req, state}) ->
          [File] = maps:get(files, Req),
          ?assertEqual(#{body => <<"onetwothree">>}, maps:get(result, File)),
          ?assertMatch({handler_init, _}, receive_one())
      end).

multipart_max_parts_test_() ->
    Parts = [{data_part(<<"a">>), <<"1">>}, {data_part(<<"b">>), <<"2">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}, max_parts => 1} end,
      fun({stop, Req, state}) ->
          ?assertEqual(413, maps:get(replied, Req))
      end).

multipart_max_part_size_test_() ->
    Parts = [{file_part(<<"upload">>, <<"big.bin">>, <<"application/octet-stream">>),
              [<<"aaaa">>, <<"bbbb">>]}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}, max_part_size => 4} end,
      fun({stop, Req, state}) ->
          ?assertEqual(413, maps:get(replied, Req)),
          ?assertMatch({handler_init, _}, receive_one()),
          %% the handler must be told to clean up its partial state
          ?assertEqual({handler_abort, too_large}, receive_one())
      end).

multipart_max_total_size_test_() ->
    Parts = [{data_part(<<"a">>), <<"aaaaa">>}, {data_part(<<"b">>), <<"bbbbb">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}, max_total_size => 8} end,
      fun({stop, Req, state}) ->
          ?assertEqual(413, maps:get(replied, Req))
      end).

multipart_malformed_part_test_() ->
    Parts = [{#{<<"content-type">> => <<"text/plain">>}, <<"no disposition">>}],
    multipart_test_(
      Parts, fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(400, maps:get(replied, Req))
      end).

multipart_handler_rejects_init_test_() ->
    Parts = [{file_part(<<"upload">>, <<"x">>, <<"application/octet-stream">>), <<"data">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, #{owner => Owner, reject_init => quota_exceeded}}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(400, maps:get(replied, Req))
      end).

multipart_handler_rejects_chunk_test_() ->
    Parts = [{file_part(<<"upload">>, <<"x">>, <<"application/octet-stream">>), <<"data">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, #{owner => Owner, reject_data => true}}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(500, maps:get(replied, Req)),
          ?assertMatch({handler_init, _}, receive_one()),
          ?assertEqual({handler_abort, chunk_rejected}, receive_one())
      end).

%% A field with no `filename=' is classified {data, _} and streamed to
%% `params' instead of the handler - it must still be bounded, by
%% max_field_size, or an attacker skips the streaming path entirely just by
%% omitting filename= on an otherwise-identical part.
multipart_field_bounded_by_max_field_size_test_() ->
    Parts = [{data_part(<<"title">>), <<"way too much text">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}, max_field_size => 4} end,
      fun({stop, Req, state}) ->
          ?assertEqual(413, maps:get(replied, Req))
      end).

%% handle_end/1 returning {error, _} must still trigger cleanup via
%% handle_abort/2 - a handler that spools to disk/S3 in handle_data and only
%% commits in handle_end would otherwise leak the partial resource.
multipart_handle_end_error_triggers_abort_test_() ->
    Parts = [{file_part(<<"upload">>, <<"x">>, <<"application/octet-stream">>), <<"data">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, #{owner => Owner, reject_end => true}}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(500, maps:get(replied, Req)),
          ?assertMatch({handler_init, _}, receive_one()),
          ?assertEqual({handler_abort, disk_full}, receive_one())
      end).

%% A handler that raises instead of returning {error, _} must not crash
%% pre_request/4 (which would skip handle_abort/2 entirely) and must not
%% leak its exception/stacktrace to the client.
multipart_handler_raise_is_contained_test_() ->
    Parts = [{file_part(<<"upload">>, <<"x">>, <<"application/octet-stream">>), <<"data">>}],
    multipart_test_(
      Parts,
      fun(Owner) -> #{handler => {nova_multipart_test_handler, #{owner => Owner, raise_data => true}}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(500, maps:get(replied, Req)),
          ?assertMatch({handler_init, _}, receive_one()),
          ?assertEqual({handler_abort, handler_crashed}, receive_one())
      end).

%% cowboy_req:read_part_body/2 doesn't return an {error, _} tuple on a
%% stalled read - it exit/1s straight out of the call. That must still
%% trigger handle_abort/2 for the handler's open resource, not unwind past
%% it uncaught.
multipart_cowboy_exit_mid_file_triggers_abort_test_() ->
    Parts = [{file_part(<<"upload">>, <<"big.bin">>, <<"application/octet-stream">>),
              [<<"partial">>, {raise, timeout}]}],
    multipart_test_(
      Parts, fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(408, maps:get(replied, Req)),
          ?assertMatch({handler_init, _}, receive_one()),
          ?assertEqual({handler_abort, timeout}, receive_one())
      end).

multipart_cowboy_exit_mid_field_test_() ->
    Parts = [{data_part(<<"title">>), [<<"partial">>, {raise, timeout}]}],
    multipart_test_(
      Parts, fun(Owner) -> #{handler => {nova_multipart_test_handler, handler_opts(Owner)}} end,
      fun({stop, Req, state}) ->
          ?assertEqual(408, maps:get(replied, Req))
      end).

receive_one() ->
    receive Msg -> Msg after 1000 -> timeout end.
