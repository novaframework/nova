-module(nova_file_controller_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

file_controller_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Get file - basic", fun test_get_file_basic/0},
      {"Get file - with custom MIME type", fun test_get_file_custom_mime/0},
      {"Get file - with range request", fun test_get_file_range/0},
      {"Get file - invalid range request", fun test_get_file_invalid_range/0},
      {"Get file - not found", fun test_get_file_not_found/0},
      {"Get dir - not found", fun test_get_dir_not_found/0}
     ]}.

setup() ->
    %% Create temporary test files
    TestDir = "/tmp/nova_test_files",
    filelib:ensure_dir(TestDir ++ "/"),

    %% Create a test file
    file:write_file(TestDir ++ "/test.txt", <<"Hello, World!">>),
    file:write_file(TestDir ++ "/index.html", <<"<html>Index</html>">>),
    file:write_file(TestDir ++ "/style.css", <<"body { color: red; }">>),

    %% Mock cow_mimetypes for MIME type detection
    meck:new(cow_mimetypes, [passthrough]),
    meck:expect(cow_mimetypes, web, fun(<<"/test.txt">>) -> {<<"text">>, <<"plain">>, []};
                                        (<<"/tmp/nova_test_files/test.txt">>) -> {<<"text">>, <<"plain">>, []};
                                        (<<"/style.css">>) -> {<<"text">>, <<"css">>, []};
                                        (<<"/tmp/nova_test_files/style.css">>) -> {<<"text">>, <<"css">>, []};
                                        (_) -> {<<"application">>, <<"octet-stream">>, []}
                                    end),

    TestDir.

cleanup(TestDir) ->
    %% Clean up test files
    file:delete(TestDir ++ "/test.txt"),
    file:delete(TestDir ++ "/index.html"),
    file:delete(TestDir ++ "/style.css"),
    file:del_dir(TestDir),

    catch meck:unload(cow_mimetypes),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_get_file_basic() ->
    %% Test basic file serving
    Req = #{extra_state => #{static => {file, "/tmp/nova_test_files/test.txt"},
                             options => #{}},
            headers => #{}},

    Result = nova_file_controller:get_file(Req),

    %% Should return sendfile response
    ?assertMatch({sendfile, 200, #{}, {0, 13, "/tmp/nova_test_files/test.txt"}, <<"text/plain">>}, Result).

test_get_file_custom_mime() ->
    %% Test file serving with custom MIME type
    Req = #{extra_state => #{static => {file, "/tmp/nova_test_files/test.txt"},
                             options => #{mimetype => <<"application/custom">>}},
            headers => #{}},

    Result = nova_file_controller:get_file(Req),

    %% Should use custom MIME type
    ?assertMatch({sendfile, 200, #{}, {0, 13, "/tmp/nova_test_files/test.txt"}, <<"application/custom">>}, Result).

test_get_file_range() ->
    %% Test range request (partial content)
    Req = #{extra_state => #{static => {file, "/tmp/nova_test_files/test.txt"},
                             options => #{}},
            headers => #{<<"range">> => <<"bytes=0-5">>}},

    Result = nova_file_controller:get_file(Req),

    %% Should return 206 Partial Content
    ?assertMatch({sendfile, 206, _, {0, 6, "/tmp/nova_test_files/test.txt"}, <<"text/plain">>}, Result),

    {sendfile, 206, Headers, _, _} = Result,
    ?assertEqual(<<"bytes 0-5/13">>, maps:get(<<"content-range">>, Headers)),
    ?assertEqual(<<"6">>, maps:get(<<"content-length">>, Headers)).

test_get_file_invalid_range() ->
    %% Test invalid range request
    Req = #{extra_state => #{static => {file, "/tmp/nova_test_files/test.txt"},
                             options => #{}},
            headers => #{<<"range">> => <<"bytes=100-200">>}},

    Result = nova_file_controller:get_file(Req),

    %% Should return 416 Range Not Satisfiable
    ?assertMatch({status, 416, _}, Result).

test_get_file_not_found() ->
    %% Test file not found
    Req = #{},

    Result = nova_file_controller:get_file(Req),

    ?assertEqual({status, 404}, Result).

test_get_dir_not_found() ->
    %% Test directory access without proper state
    Req = #{},

    Result = nova_file_controller:get_dir(Req),

    %% Should return 404
    ?assertEqual({status, 404}, Result).
