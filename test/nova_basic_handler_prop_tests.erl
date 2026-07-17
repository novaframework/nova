-module(nova_basic_handler_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CALLBACK, fun dummy_mod:dummy_fun/1).

%%====================================================================
%% Generators
%%====================================================================

non_post_method() ->
    oneof([<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"PATCH">>, <<"HEAD">>]).

json_value() ->
    oneof([#{}, #{<<"k">> => <<"v">>}, #{<<"n">> => 1}]).

ws_controller_data() ->
    oneof([#{}, #{data => value}, #{x => 1, y => 2}]).

ws_frame() ->
    oneof([{text, <<"hello">>}, {binary, <<1,2,3>>}, {text, <<>>}]).

%%====================================================================
%% Properties
%%====================================================================

prop_json_post_201() ->
    ?FORALL(JSON, json_value(),
            begin
                Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
                {ok, Req1} = nova_basic_handler:handle_json({json, JSON}, ?CALLBACK, Req),
                maps:get(resp_status_code, Req1) =:= 201
            end).

prop_json_non_post_200() ->
    ?FORALL({Method, JSON}, {non_post_method(), json_value()},
            begin
                Req = nova_test_helper:mock_req(Method, <<"/">>),
                {ok, Req1} = nova_basic_handler:handle_json({json, JSON}, ?CALLBACK, Req),
                maps:get(resp_status_code, Req1) =:= 200
            end).

prop_redirect_always_302() ->
    ?FORALL(Route, binary(),
            begin
                Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
                {ok, Req1} = nova_basic_handler:handle_redirect({redirect, Route}, ?CALLBACK, Req),
                maps:get(resp_status_code, Req1) =:= 302 andalso
                maps:get(<<"location">>, maps:get(resp_headers, Req1)) =:= Route
            end).

prop_ws_ok_preserves_data() ->
    ?FORALL(Data, ws_controller_data(),
            begin
                State = #{controller_data => #{}, commands => []},
                Result = nova_basic_handler:handle_ws({ok, Data}, State),
                maps:get(controller_data, Result) =:= Data
            end).

prop_ws_reply_appends_command() ->
    ?FORALL({Frame, Data}, {ws_frame(), ws_controller_data()},
            begin
                State = #{controller_data => #{}, commands => []},
                Result = nova_basic_handler:handle_ws({reply, Frame, Data}, State),
                maps:get(commands, Result) =:= [Frame] andalso
                maps:get(controller_data, Result) =:= Data
            end).

%%====================================================================
%% EUnit wrappers — setup/cleanup nova env once for the whole suite
%%====================================================================

proper_test_() ->
    Opts = [{numtests, 100}, {to_file, user}],
    {setup,
     fun() -> nova_test_helper:setup_nova_env() end,
     fun(Prev) -> nova_test_helper:cleanup_nova_env(Prev) end,
     [
      {"json POST always 201",
       ?_assert(proper:quickcheck(prop_json_post_201(), Opts))},
      {"json non-POST always 200",
       ?_assert(proper:quickcheck(prop_json_non_post_200(), Opts))},
      {"redirect always 302 with location",
       ?_assert(proper:quickcheck(prop_redirect_always_302(), Opts))},
      {"ws ok preserves controller_data",
       ?_assert(proper:quickcheck(prop_ws_ok_preserves_data(), Opts))},
      {"ws reply appends frame to commands",
       ?_assert(proper:quickcheck(prop_ws_reply_appends_command(), Opts))}
     ]}.
