%%% What is left on disk of a multipart upload once the request is over.
-module(nova_upload_cleanup_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([refused_upload_is_deleted/1,
         upload_stopped_by_a_later_plugin_is_deleted/1,
         upload_of_a_crashing_controller_is_deleted/1,
         upload_nobody_kept_is_deleted_after_the_response/1,
         kept_upload_survives_the_request/1,
         moved_upload_survives_the_request/1,
         abandoned_part_is_deleted/1,
         upload_past_the_file_limit_is_a_503/1,
         upload_past_the_byte_limit_is_a_503/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(BOUNDARY, "nova-upload-boundary").

all() ->
    [refused_upload_is_deleted,
     upload_stopped_by_a_later_plugin_is_deleted,
     upload_of_a_crashing_controller_is_deleted,
     upload_nobody_kept_is_deleted_after_the_response,
     kept_upload_survives_the_request,
     moved_upload_survives_the_request,
     abandoned_part_is_deleted,
     upload_past_the_file_limit_is_a_503,
     upload_past_the_byte_limit_is_a_503].

init_per_suite(Config) ->
    Dir = filename:join(?config(priv_dir, Config), "spool"),
    ok = file:make_dir(Dir),
    Multipart = {pre_request, nova_multipart_plugin, #{handler => {nova_multipart_file_handler, #{dir => Dir}}}},
    Routes = [#{prefix => "/refused",
                security => fun nova_upload_cleanup_pages:refuse/1,
                plugins => [Multipart],
                routes => [{"/upload", fun nova_upload_cleanup_pages:leave/1, #{methods => [post]}}]},
              #{prefix => "/stopped",
                plugins => [Multipart, {pre_request, nova_upload_cleanup_pages, #{}}],
                routes => [{"/upload", fun nova_upload_cleanup_pages:leave/1, #{methods => [post]}}]},
              #{prefix => "/open",
                plugins => [Multipart],
                routes => [{"/leave", fun nova_upload_cleanup_pages:leave/1, #{methods => [post]}},
                           {"/keep", fun nova_upload_cleanup_pages:keep/1, #{methods => [post]}},
                           {"/move", fun nova_upload_cleanup_pages:move/1, #{methods => [post]}},
                           {"/crash", fun nova_upload_cleanup_pages:crash/1, #{methods => [post]}}]}],
    [{port, nova_test_listener:start(#{host => Routes})}, {spool, Dir} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    application:unset_env(nova, multipart_spool),
    Dir = ?config(spool, Config),
    {ok, Files} = file:list_dir(Dir),
    [file:delete(filename:join(Dir, File)) || File <- Files],
    ok.

refused_upload_is_deleted(Config) ->
    {401, _Headers, _Body} = upload(Config, "/refused/upload", [{"a.bin", ~"first"}, {"b.bin", ~"second"}]),
    ?assertEqual([], spooled(Config, 0)).

upload_stopped_by_a_later_plugin_is_deleted(Config) ->
    {403, _Headers, _Body} = upload(Config, "/stopped/upload", [{"a.bin", ~"first"}]),
    ?assertEqual([], spooled(Config, 0)).

upload_of_a_crashing_controller_is_deleted(Config) ->
    {500, _Headers, _Body} = upload(Config, "/open/crash", [{"a.bin", ~"first"}]),
    ?assertEqual([], spooled(Config, 0)).

upload_nobody_kept_is_deleted_after_the_response(Config) ->
    {200, _Headers, Body} = upload(Config, "/open/leave", [{"a.bin", ~"first"}, {"b.bin", ~"second"}]),
    ?assertEqual(#{~"sizes" => [5, 6]}, json:decode(Body)),
    ?assertEqual([], spooled(Config, 0)).

kept_upload_survives_the_request(Config) ->
    {200, _Headers, ~"kept"} = upload(Config, "/open/keep", [{"a.bin", ~"first"}, {"b.bin", ~"second"}]),
    [Kept] = spooled(Config, 1),
    ?assertEqual({ok, ~"first"}, file:read_file(filename:join(?config(spool, Config), Kept))).

moved_upload_survives_the_request(Config) ->
    {200, _Headers, ~"moved"} = upload(Config, "/open/move", [{"a.bin", ~"first"}]),
    [Moved] = spooled(Config, 1),
    ?assertEqual(".moved", filename:extension(Moved)),
    ?assertEqual({ok, ~"first"}, file:read_file(filename:join(?config(spool, Config), Moved))).

%% The part is large enough for the plugin to have opened its file and
%% written to it before the client goes away.
abandoned_part_is_deleted(Config) ->
    {_Headers, Body} = multipart([{"a.bin", binary:copy(~"x", 400000)}]),
    Half = binary:part(Body, 0, byte_size(Body) div 2),
    ok = nova_test_listener:send_and_close(?config(port, Config),
                                           ["POST /open/leave HTTP/1.1\r\n",
                                            "host: localhost\r\n",
                                            "content-type: multipart/form-data; boundary=", ?BOUNDARY, "\r\n",
                                            "content-length: ", integer_to_list(byte_size(Body)), "\r\n\r\n",
                                            Half]),
    ?assertEqual([], spooled(Config, 0)).

upload_past_the_file_limit_is_a_503(Config) ->
    application:set_env(nova, multipart_spool, #{max_files => 1}),
    {503, _Headers, _Body} = upload(Config, "/open/keep", [{"a.bin", ~"first"}, {"b.bin", ~"second"}]),
    ?assertEqual([], spooled(Config, 0)).

upload_past_the_byte_limit_is_a_503(Config) ->
    application:set_env(nova, multipart_spool, #{max_bytes => 8}),
    {503, _Headers, _Body} = upload(Config, "/open/keep", [{"a.bin", ~"first"}, {"b.bin", ~"second"}]),
    ?assertEqual([], spooled(Config, 0)),
    ?assertEqual(#{files => 0, bytes => 0}, nova_multipart_spool:usage()).

upload(Config, Path, Files) ->
    {Headers, Body} = multipart(Files),
    nova_test_listener:response(?config(port, Config), "POST", Path, Headers, Body).

multipart(Files) ->
    Parts = [["--", ?BOUNDARY, "\r\n",
              "content-disposition: form-data; name=\"upload\"; filename=\"", Filename, "\"\r\n",
              "content-type: application/octet-stream\r\n\r\n",
              Data, "\r\n"] || {Filename, Data} <- Files],
    {[{"content-type", ["multipart/form-data; boundary=", ?BOUNDARY]}],
     iolist_to_binary([Parts, "--", ?BOUNDARY, "--\r\n"])}.

%% The files are deleted when the process that served the request is gone,
%% which is shortly after the response, so the directory is given two
%% seconds to get down to the number of files the case expects.
spooled(Config, Expected) ->
    spooled(?config(spool, Config), Expected, 40).

spooled(Dir, Expected, Attempts) ->
    {ok, Files} = file:list_dir(Dir),
    case length(Files) =:= Expected orelse Attempts =:= 0 of
        true ->
            lists:sort(Files);
        false ->
            timer:sleep(50),
            spooled(Dir, Expected, Attempts - 1)
    end.
