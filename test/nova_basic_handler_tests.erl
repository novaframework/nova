-module(nova_basic_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> nova_test_helper:setup_nova_env() end).
-define(CLEANUP, fun(Prev) -> nova_test_helper:cleanup_nova_env(Prev) end).
-define(CALLBACK, fun dummy_mod:dummy_fun/1).

%%====================================================================
%% handle_json/3
%%====================================================================

handle_json_get_default_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1} = nova_basic_handler:handle_json({json, #{key => value}}, ?CALLBACK, Req),
        ?assertEqual(200, maps:get(resp_status_code, Req1)),
        ?assert(maps:is_key(<<"content-type">>, maps:get(resp_headers, Req1)))
    end}.

handle_json_post_201_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"POST">>, <<"/">>),
        {ok, Req1} = nova_basic_handler:handle_json({json, #{created => true}}, ?CALLBACK, Req),
        ?assertEqual(201, maps:get(resp_status_code, Req1))
    end}.

handle_json_custom_status_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1} = nova_basic_handler:handle_json(
            {json, 202, #{<<"x-custom">> => <<"yes">>}, #{accepted => true}}, ?CALLBACK, Req),
        ?assertEqual(202, maps:get(resp_status_code, Req1)),
        Headers = maps:get(resp_headers, Req1),
        ?assertEqual(<<"yes">>, maps:get(<<"x-custom">>, Headers))
    end}.

handle_json_req_passthrough_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        CustomReq = Req#{custom_field => true},
        {ok, Req1} = nova_basic_handler:handle_json(
            {json, 200, #{}, CustomReq, #{data => 1}}, ?CALLBACK, Req),
        ?assertEqual(true, maps:get(custom_field, Req1))
    end}.

%%====================================================================
%% handle_status/3
%%====================================================================

handle_status_json_body_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1} = nova_basic_handler:handle_status(
            {status, 400, #{}, #{error => <<"bad">>}}, ?CALLBACK, Req),
        ?assertEqual(400, maps:get(resp_status_code, Req1)),
        Headers = maps:get(resp_headers, Req1),
        ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers))
    end}.

handle_status_binary_body_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        {ok, Req1} = nova_basic_handler:handle_status(
            {status, 400, #{<<"x-err">> => <<"1">>}, <<"bad request">>}, ?CALLBACK, Req),
        ?assertEqual(400, maps:get(resp_status_code, Req1)),
        ?assertEqual(<<"bad request">>, maps:get(resp_body, Req1))
    end}.

handle_status_req_passthrough_test_() ->
    {setup, ?SETUP, ?CLEANUP, fun() ->
        Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
        CustomReq = Req#{my_tag => yes},
        {ok, Req1} = nova_basic_handler:handle_status(
            {status, 500, #{}, #{err => 1}, CustomReq}, ?CALLBACK, Req),
        ?assertEqual(yes, maps:get(my_tag, Req1))
    end}.

%%====================================================================
%% handle_redirect/3
%%====================================================================

handle_redirect_binary_route_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1} = nova_basic_handler:handle_redirect({redirect, <<"/login">>}, ?CALLBACK, Req),
    ?assertEqual(302, maps:get(resp_status_code, Req1)),
    ?assertEqual(<<"/login">>, maps:get(<<"location">>, maps:get(resp_headers, Req1))).

handle_redirect_list_route_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    {ok, Req1} = nova_basic_handler:handle_redirect({redirect, "/home"}, ?CALLBACK, Req),
    ?assertEqual(302, maps:get(resp_status_code, Req1)),
    ?assertEqual(<<"/home">>, maps:get(<<"location">>, maps:get(resp_headers, Req1))).

handle_redirect_req_passthrough_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/">>),
    CustomReq = Req#{tagged => ok},
    {ok, Req1} = nova_basic_handler:handle_redirect({redirect, <<"/x">>, CustomReq}, ?CALLBACK, Req),
    ?assertEqual(ok, maps:get(tagged, Req1)).

%%====================================================================
%% handle_sendfile/3
%%====================================================================

handle_sendfile_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/file">>),
    FileInfo = {0, 100, "/tmp/test.txt"},
    {ok, Req1} = nova_basic_handler:handle_sendfile(
        {sendfile, 200, #{}, FileInfo, <<"text/plain">>}, ?CALLBACK, Req),
    ?assertEqual(200, maps:get(resp_status_code, Req1)),
    Headers = maps:get(resp_headers, Req1),
    ?assertEqual(<<"text/plain">>, maps:get(<<"content-type">>, Headers)),
    ?assertEqual({sendfile, 0, 100, "/tmp/test.txt"}, maps:get(resp_body, Req1)).

handle_sendfile_req_passthrough_test() ->
    Req = nova_test_helper:mock_req(<<"GET">>, <<"/file">>),
    CustomReq = Req#{tag => sf},
    FileInfo = {0, 50, "/tmp/f.txt"},
    {ok, Req1} = nova_basic_handler:handle_sendfile(
        {sendfile, 200, #{}, FileInfo, <<"application/octet-stream">>, CustomReq}, ?CALLBACK, Req),
    ?assertEqual(sf, maps:get(tag, Req1)).

%%====================================================================
%% handle_ws/2
%%====================================================================

handle_ws_reply_test() ->
    State = #{controller_data => #{}, commands => []},
    Result = nova_basic_handler:handle_ws({reply, {text, <<"hi">>}, #{new => data}}, State),
    ?assertEqual(#{new => data}, maps:get(controller_data, Result)),
    ?assertEqual([{text, <<"hi">>}], maps:get(commands, Result)).

handle_ws_reply_hibernate_test() ->
    State = #{controller_data => #{}, commands => [{text, <<"a">>}]},
    Result = nova_basic_handler:handle_ws(
        {reply, {text, <<"b">>}, #{}, hibernate}, State),
    ?assertEqual(true, maps:get(hibernate, Result)),
    ?assertEqual([{text, <<"b">>}, {text, <<"a">>}], maps:get(commands, Result)).

handle_ws_ok_test() ->
    State = #{controller_data => old, commands => []},
    Result = nova_basic_handler:handle_ws({ok, new_data}, State),
    ?assertEqual(new_data, maps:get(controller_data, Result)).

handle_ws_ok_hibernate_test() ->
    State = #{controller_data => old, commands => []},
    Result = nova_basic_handler:handle_ws({ok, new_data, hibernate}, State),
    ?assertEqual(new_data, maps:get(controller_data, Result)),
    ?assertEqual(true, maps:get(hibernate, Result)).

handle_ws_stop_test() ->
    State = #{controller_data => old, commands => []},
    {stop, ResultState} = nova_basic_handler:handle_ws({stop, done}, State),
    ?assertEqual(done, maps:get(controller_data, ResultState)).

handle_ws_ok_atom_test() ->
    State = #{controller_data => x, commands => []},
    ?assertEqual(State, nova_basic_handler:handle_ws(ok, State)).

%%====================================================================
%% get_view_name/1
%%====================================================================

get_view_name_strips_controller_suffix_test() ->
    ?assertEqual(my_app_dtl, nova_basic_handler:get_view_name(my_app_controller)).

get_view_name_no_controller_suffix_test() ->
    ?assertEqual(my_app_dtl, nova_basic_handler:get_view_name(my_app_controller)).

get_view_name_tuple_test() ->
    ?assertEqual(foo_dtl, nova_basic_handler:get_view_name({foo_controller, #{}})).
