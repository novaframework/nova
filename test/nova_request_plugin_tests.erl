-module(nova_request_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> nova_test_helper:setup_nova_env() end).
-define(CLEANUP, fun(Prev) -> nova_test_helper:cleanup_nova_env(Prev) end).

%%====================================================================
%% should_read_body/1
%%====================================================================

should_read_body_empty_test() ->
    ?assertNot(nova_request_plugin:should_read_body([])).

should_read_body_decode_json_test() ->
    ?assert(nova_request_plugin:should_read_body([{decode_json_body, true}])).

should_read_body_urlencoded_test() ->
    ?assert(nova_request_plugin:should_read_body([{read_urlencoded_body, true}])).

should_read_body_unknown_test() ->
    ?assertNot(nova_request_plugin:should_read_body([{parse_qs, true}])).

should_read_body_mixed_test() ->
    ?assert(nova_request_plugin:should_read_body([{parse_qs, true}, {decode_json_body, true}])).

%%====================================================================
%% modulate_state/3 — skip JSON decode for GET/DELETE
%%====================================================================

modulate_state_skip_json_get_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
         {ok, _Req1, state} = nova_request_plugin:modulate_state(
             Req, [{decode_json_body, true}], state)
     end}.

modulate_state_skip_json_delete_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"DELETE">>, <<"/">>),
         {ok, _Req1, state} = nova_request_plugin:modulate_state(
             Req, [{decode_json_body, true}], state)
     end}.

%%====================================================================
%% modulate_state/3 — skip unknown options
%%====================================================================

modulate_state_skip_unknown_test() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
    {ok, Req1, state} = nova_request_plugin:modulate_state(
        Req, [{unknown_option, true}], state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% modulate_state/3 — empty options
%%====================================================================

modulate_state_empty_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1, state} = nova_request_plugin:modulate_state(Req, [], state),
    ?assertEqual(Req, Req1).

%%====================================================================
%% modulate_state/3 — parse_qs
%%====================================================================

modulate_state_parse_qs_true_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Req1 = Req#{qs => <<"foo=bar&baz=qux">>},
    {ok, Req2, state} = nova_request_plugin:modulate_state(
        Req1, [{parse_qs, true}], state),
    Parsed = maps:get(parsed_qs, Req2),
    ?assertEqual(<<"bar">>, maps:get(<<"foo">>, Parsed)),
    ?assertEqual(<<"qux">>, maps:get(<<"baz">>, Parsed)).

modulate_state_parse_qs_list_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    Req1 = Req#{qs => <<"a=1&b=2">>},
    {ok, Req2, state} = nova_request_plugin:modulate_state(
        Req1, [{parse_qs, list}], state),
    Parsed = maps:get(parsed_qs, Req2),
    ?assert(is_list(Parsed)),
    ?assertEqual(2, length(Parsed)).

%%====================================================================
%% modulate_state/3 — decode_json_body on POST with body
%%====================================================================

modulate_state_decode_json_post_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
         Req1 = nova_test_helper:with_json_body(#{<<"name">> => <<"test">>}, Req),
         {ok, Req2, state} = nova_request_plugin:modulate_state(
             Req1, [{decode_json_body, true}], state),
         JSON = maps:get(json, Req2),
         ?assertEqual(<<"test">>, maps:get(<<"name">>, JSON))
     end}.

%%====================================================================
%% modulate_state/3 — decode_json_body on POST with empty body -> stop
%%====================================================================

modulate_state_decode_json_empty_body_test_() ->
    {setup, ?SETUP, ?CLEANUP,
     fun() ->
         Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
         Req1 = nova_test_helper:with_content_type(<<"application/json">>, Req),
         Req2 = Req1#{body => <<>>},
         {stop, _, state} = nova_request_plugin:modulate_state(
             Req2, [{decode_json_body, true}], state)
     end}.

%%====================================================================
%% plugin_info/0
%%====================================================================

plugin_info_test() ->
    Info = nova_request_plugin:plugin_info(),
    ?assertEqual(<<"Nova body plugin">>, maps:get(title, Info)),
    ?assert(is_list(maps:get(options, Info))).

%%====================================================================
%% multipart/form-data
%%====================================================================

multipart_req() ->
    Req = nova_test_helper:mock_req(<<"POST">>, <<"/upload">>),
    Req0 = nova_test_helper:with_content_type(
             <<"multipart/form-data; boundary=----abc">>, Req),
    Req0#{has_body => true}.

%% Script cowboy_req so the part-reading loop can be tested without a socket.
%% Parts is a list of {Headers, Body} where Body is either a binary or a list
%% of binaries to be delivered as {more, ...} chunks.
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
                fun(Req) ->
                    case call(Pid, next_chunk) of
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

multipart_test_(Parts, Options, Assertions) ->
    {setup,
     fun() -> mock_cowboy_req(Parts) end,
     fun(_) -> meck:unload(cowboy_req) end,
     fun() ->
         Result = nova_request_plugin:pre_request(multipart_req(), env, Options, state),
         Assertions(Result)
     end}.

multipart_reads_fields_and_files_test_() ->
    Parts = [{data_part(<<"title">>), <<"a picture">>},
             {file_part(<<"upload">>, <<"logo.png">>, <<"image/png">>), <<"binarydata">>}],
    multipart_test_(
      Parts, #{read_multipart_body => true},
      fun({ok, Req, state}) ->
          ?assertEqual(#{<<"title">> => <<"a picture">>}, maps:get(params, Req)),
          ?assertEqual([#{name => <<"upload">>,
                          filename => <<"logo.png">>,
                          content_type => <<"image/png">>,
                          body => <<"binarydata">>}], maps:get(files, Req)),
          ?assertEqual(<<>>, maps:get(body, Req))
      end).

multipart_joins_chunked_part_test_() ->
    Parts = [{file_part(<<"upload">>, <<"big.bin">>, <<"application/octet-stream">>),
              [<<"one">>, <<"two">>, <<"three">>]}],
    multipart_test_(
      Parts, #{read_multipart_body => true},
      fun({ok, Req, state}) ->
          [File] = maps:get(files, Req),
          ?assertEqual(<<"onetwothree">>, maps:get(body, File))
      end).

multipart_too_large_test_() ->
    Parts = [{file_part(<<"upload">>, <<"big.bin">>, <<"application/octet-stream">>),
              <<"way too much data">>}],
    multipart_test_(
      Parts, #{read_multipart_body => #{max_file_size => 4}},
      fun({stop, Req, state}) ->
          ?assertEqual(413, maps:get(replied, Req))
      end).

multipart_malformed_part_test_() ->
    Parts = [{#{<<"content-type">> => <<"text/plain">>}, <<"no disposition">>}],
    multipart_test_(
      Parts, #{read_multipart_body => true},
      fun({stop, Req, state}) ->
          ?assertEqual(400, maps:get(replied, Req))
      end).

%% Without the option the body must be left for the controller to stream.
multipart_body_not_drained_test_() ->
    {setup,
     fun() ->
         meck:new(cowboy_req, [passthrough]),
         meck:expect(cowboy_req, has_body, fun(_Req) -> true end),
         meck:expect(cowboy_req, read_body,
                     fun(_Req) -> erlang:error(body_should_not_be_read) end)
     end,
     fun(_) -> meck:unload(cowboy_req) end,
     fun() ->
         {ok, Req, state} = nova_request_plugin:pre_request(
                              multipart_req(), env, #{decode_json_body => true}, state),
         ?assertEqual(<<>>, maps:get(body, Req)),
         ?assertNot(maps:is_key(files, Req))
     end}.
