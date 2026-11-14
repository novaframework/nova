%%% Requests for static files that try to leave the static root, sent over a
%%% raw socket so nothing normalises the path on the way.
-module(nova_static_path_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         file_inside_root/1,
         dot_segments/1,
         encoded_dot_segments/1,
         encoded_slash/1,
         backslash/1,
         nul/1,
         absolute_segment/1,
         symlink_inside_root/1,
         symlink_escaping_root/1,
         symlink_escaping_root_when_allowed/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(SECRET, ~"outside-the-static-root").

all() ->
    [
     file_inside_root,
     dot_segments,
     encoded_dot_segments,
     encoded_slash,
     backslash,
     nul,
     absolute_segment,
     symlink_inside_root,
     symlink_escaping_root,
     symlink_escaping_root_when_allowed
    ].

init_per_suite(Config) ->
    Base = filename:join(?config(priv_dir, Config), "static_fixture"),
    Root = filename:join(Base, "root"),
    ok = filelib:ensure_path(filename:join(Root, "sub")),
    ok = file:write_file(filename:join(Base, "secret.txt"), ?SECRET),
    ok = file:write_file(filename:join(Root, "inside.txt"), ~"inside"),
    ok = file:write_file(filename:join([Root, "sub", "deep.txt"]), ~"deep"),
    ok = file:make_symlink("inside.txt", filename:join(Root, "link-inside")),
    ok = file:make_symlink("../secret.txt", filename:join(Root, "link-outside")),
    ok = file:make_symlink("..", filename:join(Root, "dir-outside")),
    Host = [#{prefix => "",
              routes => [
                         {"/files/[...]", Root},
                         {"/linked/[...]", Root, #{follow_external_symlinks => true}}
                        ]}],
    [{port, nova_test_listener:start(#{host => Host})}, {secret, filename:join(Base, "secret.txt")} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

file_inside_root(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"inside"}, nova_test_listener:get(Port, "/files/inside.txt")),
    ?assertEqual({200, ~"deep"}, nova_test_listener:get(Port, "/files/sub/deep.txt")).

dot_segments(Config) ->
    refused(Config, "/files/../secret.txt"),
    refused(Config, "/files/sub/../../secret.txt"),
    refused(Config, "/files/./../secret.txt"),
    refused(Config, "/files/.../secret.txt").

encoded_dot_segments(Config) ->
    refused(Config, "/files/%2e%2e/secret.txt"),
    refused(Config, "/files/%2E%2E/secret.txt"),
    refused(Config, "/files/.%2e/secret.txt"),
    refused(Config, "/files/sub/%2e%2e/%2e%2e/secret.txt").

encoded_slash(Config) ->
    refused(Config, "/files/..%2fsecret.txt"),
    refused(Config, "/files/%2e%2e%2fsecret.txt"),
    refused(Config, "/files/sub%2f..%2f..%2fsecret.txt").

backslash(Config) ->
    refused(Config, "/files/..\\secret.txt"),
    refused(Config, "/files/sub\\..\\..\\secret.txt"),
    refused(Config, "/files/..%5csecret.txt").

nul(Config) ->
    refused(Config, "/files/inside.txt%00"),
    refused(Config, "/files/../secret.txt%00.txt"),
    refused(Config, ["/files/inside.txt", 0, ".png"]).

absolute_segment(Config) ->
    Secret = ?config(secret, Config),
    refused(Config, "/files/" ++ Secret),
    refused(Config, "/files" ++ Secret),
    refused(Config, "/files/%2f" ++ lists:flatten(string:replace(tl(Secret), "/", "%2f", all))).

symlink_inside_root(Config) ->
    ?assertEqual({200, ~"inside"}, nova_test_listener:get(?config(port, Config), "/files/link-inside")).

symlink_escaping_root(Config) ->
    refused(Config, "/files/link-outside"),
    refused(Config, "/files/dir-outside/secret.txt").

symlink_escaping_root_when_allowed(Config) ->
    ?assertEqual({200, ?SECRET}, nova_test_listener:get(?config(port, Config), "/linked/link-outside")),
    refused(Config, "/linked/../secret.txt").

refused(Config, Path) ->
    {Status, Body} = nova_test_listener:get(?config(port, Config), Path),
    ?assert(lists:member(Status, [400, 404]), {Path, Status}),
    ?assertEqual(nomatch, binary:match(Body, ?SECRET), Path).
