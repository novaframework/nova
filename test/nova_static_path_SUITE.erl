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
         symlink_escaping_root_when_allowed/1,
         root_that_is_a_symlink/1,
         index_file/1,
         relative_root_after_the_working_directory_changed/1,
         relative_file_after_the_working_directory_changed/1,
         root_link_changed_after_the_boot/1,
         root_removed_after_the_boot/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(SECRET, ~"outside-the-static-root").
-define(RELATIVE_DIR, "nova_static_path_relative").

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
     symlink_escaping_root_when_allowed,
     root_that_is_a_symlink,
     index_file,
     relative_root_after_the_working_directory_changed,
     relative_file_after_the_working_directory_changed,
     root_link_changed_after_the_boot,
     root_removed_after_the_boot
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
    ok = file:make_symlink("root", filename:join(Base, "root-link")),
    ok = filelib:ensure_path(filename:join(Root, "indexed")),
    ok = file:write_file(filename:join([Root, "indexed", "index.html"]), ~"index"),
    ok = filelib:ensure_path(filename:join(Root, "linked-index")),
    ok = file:make_symlink("../../secret.txt", filename:join([Root, "linked-index", "index.html"])),
    ok = filelib:ensure_path(?RELATIVE_DIR),
    ok = file:write_file(filename:join(?RELATIVE_DIR, "file.txt"), ~"inside"),
    ok = file:write_file(filename:join(?RELATIVE_DIR, "index.html"), ~"index"),
    Elsewhere = filename:join([Base, "elsewhere", ?RELATIVE_DIR]),
    ok = filelib:ensure_path(Elsewhere),
    ok = file:make_symlink(filename:join(Base, "secret.txt"), filename:join(Elsewhere, "file.txt")),
    ok = file:make_symlink(filename:join(Base, "secret.txt"), filename:join(Elsewhere, "index.html")),
    ok = filelib:ensure_path(filename:join(Base, "release-1")),
    ok = filelib:ensure_path(filename:join(Base, "release-2")),
    ok = file:write_file(filename:join([Base, "release-1", "file.txt"]), ~"inside"),
    ok = file:write_file(filename:join([Base, "release-1", "index.html"]), ~"index"),
    ok = file:make_symlink("../secret.txt", filename:join([Base, "release-2", "file.txt"])),
    ok = file:make_symlink("../secret.txt", filename:join([Base, "release-2", "index.html"])),
    ok = file:make_symlink("release-1", filename:join(Base, "current")),
    ok = filelib:ensure_path(filename:join(Base, "removed")),
    Host = [#{prefix => "",
              routes => [
                         {"/relative/[...]", ?RELATIVE_DIR},
                         {"/relative-index", ?RELATIVE_DIR},
                         {"/relative-file", filename:join(?RELATIVE_DIR, "file.txt")},
                         {"/removed-index", filename:join(Base, "removed")},
                         {"/removed/[...]", filename:join(Base, "removed")},
                         {"/current/[...]", filename:join(Base, "current")},
                         {"/current-index", filename:join(Base, "current")},
                         {"/files/[...]", Root},
                         {"/linked/[...]", Root, #{follow_external_symlinks => true}},
                         {"/via-link/[...]", filename:join(Base, "root-link")},
                         {"/indexed/[...]", filename:join(Root, "indexed")},
                         {"/linked-index/[...]", filename:join(Root, "linked-index")}
                        ]}],
    [{port, nova_test_listener:start(#{host => Host})}, {secret, filename:join(Base, "secret.txt")},
     {base, Base} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop(),
    file:del_dir_r(?RELATIVE_DIR).

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

root_that_is_a_symlink(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"inside"}, nova_test_listener:get(Port, "/via-link/inside.txt")),
    ?assertEqual({200, ~"deep"}, nova_test_listener:get(Port, "/via-link/sub/deep.txt")),
    ?assertEqual({200, ~"inside"}, nova_test_listener:get(Port, "/via-link/link-inside")),
    refused(Config, "/via-link/link-outside"),
    refused(Config, "/via-link/dir-outside/secret.txt").

index_file(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"index"}, nova_test_listener:get(Port, "/files/indexed")),
    ?assertEqual({200, ~"index"}, nova_test_listener:get(Port, "/indexed")),
    refused(Config, "/files/linked-index"),
    refused(Config, "/linked-index").

%% What was checked when the route was compiled is what is served, also
%% when the same relative path has come to mean another directory.
relative_root_after_the_working_directory_changed(Config) ->
    Port = ?config(port, Config),
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(filename:join(?config(base, Config), "elsewhere")),
        ?assertEqual({200, ~"inside"}, nova_test_listener:get(Port, "/relative/file.txt")),
        ?assertEqual({200, ~"index"}, nova_test_listener:get(Port, "/relative")),
        ?assertEqual({200, ~"index"}, nova_test_listener:get(Port, "/relative-index"))
    after
        ok = file:set_cwd(Cwd)
    end.

relative_file_after_the_working_directory_changed(Config) ->
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(filename:join(?config(base, Config), "elsewhere")),
        ?assertEqual({200, ~"inside"}, nova_test_listener:get(?config(port, Config), "/relative-file"))
    after
        ok = file:set_cwd(Cwd)
    end.

root_removed_after_the_boot(Config) ->
    Port = ?config(port, Config),
    ok = file:del_dir(filename:join(?config(base, Config), "removed")),
    ?assertMatch({404, _Body}, nova_test_listener:get(Port, "/removed-index")),
    ?assertMatch({404, _Body}, nova_test_listener:get(Port, "/removed")),
    ?assertMatch({404, _Body}, nova_test_listener:get(Port, "/removed/file.txt")).

root_link_changed_after_the_boot(Config) ->
    Port = ?config(port, Config),
    Link = filename:join(?config(base, Config), "current"),
    ok = file:delete(Link),
    ok = file:make_symlink("release-2", Link),
    ?assertEqual({200, ~"inside"}, nova_test_listener:get(Port, "/current/file.txt")),
    ?assertEqual({200, ~"index"}, nova_test_listener:get(Port, "/current")),
    ?assertEqual({200, ~"index"}, nova_test_listener:get(Port, "/current-index")).

refused(Config, Path) ->
    {Status, Body} = nova_test_listener:get(?config(port, Config), Path),
    ?assert(lists:member(Status, [400, 404]), {Path, Status}),
    ?assertEqual(nomatch, binary:match(Body, ?SECRET), Path).
