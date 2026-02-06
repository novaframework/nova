-module(nova_error_controller_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

error_controller_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"404 not found - JSON response", fun test_not_found_json/0},
      {"404 not found - HTML response", fun test_not_found_html/0},
      {"404 not found - no Accept header", fun test_not_found_no_accept/0},
      {"500 server error - with crash_info status_code", fun test_server_error_with_status/0},
      {"500 server error - with class and reason", fun test_server_error_with_class/0},
      {"500 server error - dev environment JSON", fun test_server_error_dev_json/0},
      {"500 server error - dev environment HTML", fun test_server_error_dev_html/0},
      {"500 server error - prod environment", fun test_server_error_prod/0},
      {"status_code function", fun test_status_code/0}
     ]}.

setup() ->
    %% Set up JSON library
    application:set_env(nova, json_lib, thoas),
    application:set_env(nova, render_error_pages, true),

    %% Mock cowboy_req module
    meck:new(cowboy_req, [passthrough]),

    %% Mock nova module for environment
    meck:new(nova, [passthrough]),
    meck:expect(nova, get_env, fun(json_lib, _Default) -> thoas;
                                   (Other, Default) -> application:get_env(nova, Other, Default)
                                end),
    meck:expect(nova, get_environment, fun() -> dev end),

    %% Mock nova_error_dtl for template rendering
    meck:new(nova_error_dtl, [non_strict]),
    meck:expect(nova_error_dtl, render, fun(_Variables) ->
                                                {ok, <<"<html>Error Page</html>">>}
                                        end),

    ok.

cleanup(_) ->
    catch meck:unload(cowboy_req),
    catch meck:unload(nova),
    catch meck:unload(nova_error_dtl),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_not_found_json() ->
    %% Mock Accept header for JSON
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            <<"application/json">>
                                    end),

    Req = #{method => <<"GET">>, path => <<"/nonexistent">>},

    Result = nova_error_controller:not_found(Req),

    %% Should return JSON status response
    ?assertMatch({status, 404, #{<<"content-type">> := <<"application/json">>}, _}, Result),

    {status, 404, _Headers, Body} = Result,
    {ok, Decoded} = thoas:decode(Body),
    %% thoas returns strings, not binaries for JSON strings
    ?assertEqual(#{<<"message">> => "Resource not found"}, Decoded).

test_not_found_html() ->
    %% Mock Accept header for HTML
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            <<"text/html">>
                                    end),

    Req = #{method => <<"GET">>, path => <<"/nonexistent">>},

    Result = nova_error_controller:not_found(Req),

    %% Should return HTML status response
    ?assertMatch({status, 404, #{<<"content-type">> := <<"text/html">>}, _}, Result),

    {status, 404, _Headers, Body} = Result,
    ?assertEqual(<<"<html>Error Page</html>">>, Body).

test_not_found_no_accept() ->
    %% Mock no Accept header (defaults to JSON)
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            undefined
                                    end),

    Req = #{method => <<"GET">>, path => <<"/nonexistent">>},

    Result = nova_error_controller:not_found(Req),

    %% Should default to JSON
    ?assertMatch({status, 404, #{<<"content-type">> := <<"application/json">>}, _}, Result).

test_server_error_with_status() ->
    %% Test server_error with crash_info containing status_code
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            <<"application/json">>
                                    end),

    CrashInfo = #{status_code => 500,
                  status => <<"Internal Error">>,
                  title => <<"500 Error">>,
                  message => <<"Something went wrong">>,
                  stacktrace => []},

    Req = #{crash_info => CrashInfo},

    Result = nova_error_controller:server_error(Req),

    ?assertMatch({status, 500, _, _}, Result).

test_server_error_with_class() ->
    %% Test server_error with crash_info containing class and reason
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            <<"application/json">>
                                    end),

    CrashInfo = #{class => error,
                  reason => badarg},

    Req = #{crash_info => CrashInfo},

    Result = nova_error_controller:server_error(Req),

    ?assertMatch({status, 500, _, _}, Result).

test_server_error_dev_json() ->
    %% Test server error in dev environment with JSON Accept
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            <<"application/json">>
                                    end),
    meck:expect(nova, get_environment, fun() -> dev end),

    CrashInfo = #{class => error,
                  reason => test_error},

    Req = #{crash_info => CrashInfo},

    Result = nova_error_controller:server_error(Req),

    ?assertMatch({status, 500, #{<<"content-type">> := <<"application/json">>}, _}, Result).

test_server_error_dev_html() ->
    %% Test server error in dev environment with HTML Accept
    meck:expect(cowboy_req, header, fun(<<"accept">>, _Req) ->
                                            <<"text/html">>
                                    end),
    meck:expect(nova, get_environment, fun() -> dev end),

    CrashInfo = #{class => error,
                  reason => test_error},

    Req = #{crash_info => CrashInfo},

    Result = nova_error_controller:server_error(Req),

    ?assertMatch({status, 500, #{<<"content-type">> := <<"text/html">>}, _}, Result).

test_server_error_prod() ->
    %% Test server error in prod environment (should not show details)
    meck:expect(nova, get_environment, fun() -> prod end),

    CrashInfo = #{class => error,
                  reason => sensitive_error},

    Req = #{crash_info => CrashInfo},

    Result = nova_error_controller:server_error(Req),

    %% In production, should just return status code without body
    ?assertEqual({status, 500}, Result).

test_status_code() ->
    %% Test status_code function
    Req1 = #{resp_status_code => 404},
    ?assertEqual({status, 404}, nova_error_controller:status_code(Req1)),

    %% Test with default status
    Req2 = #{},
    ?assertEqual({status, 200}, nova_error_controller:status_code(Req2)).

