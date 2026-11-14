%%% Drives the controller with pathinfo a router would never produce, so the
%%% check holds on its own when the router in front of it does not
%%% canonicalise the request path.
-module(nova_file_controller_path_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, ~"outside-the-static-root").

setup() ->
    Base = filename:absname(filename:join(["_build", "test", "tmp", atom_to_list(?MODULE)])),
    _ = file:del_dir_r(Base),
    Root = filename:join(Base, "root"),
    ok = filelib:ensure_path(filename:join(Root, "sub")),
    ok = filelib:ensure_path(filename:join(Root, "linked-index")),
    ok = file:write_file(filename:join(Base, "secret.txt"), ?SECRET),
    ok = file:write_file(filename:join(Root, "inside.txt"), ~"inside"),
    ok = file:write_file(filename:join([Root, "sub", "deep.txt"]), ~"deep"),
    ok = file:write_file(filename:join([Root, "sub", "index.html"]), ~"index"),
    ok = file:make_symlink("inside.txt", filename:join(Root, "link-inside")),
    ok = file:make_symlink("sub", filename:join(Root, "dir-inside")),
    ok = file:make_symlink("../secret.txt", filename:join(Root, "link-outside")),
    ok = file:make_symlink(filename:join(Base, "secret.txt"), filename:join(Root, "link-absolute")),
    ok = file:make_symlink("..", filename:join(Root, "dir-outside")),
    ok = file:make_symlink("loop-b", filename:join(Root, "loop-a")),
    ok = file:make_symlink("loop-a", filename:join(Root, "loop-b")),
    ok = file:make_symlink("../../secret.txt", filename:join([Root, "linked-index", "index.html"])),
    ok = file:make_symlink("root", filename:join(Base, "root-link")),
    Base.

cleanup(Base) ->
    ok = file:del_dir_r(Base).

get_dir(Base, Pathinfo) ->
    get_dir(Base, "root", Pathinfo, #{}).

get_dir(Base, RootName, Pathinfo, Options) ->
    nova_file_controller:get_dir(#{path => ~"/files",
                                   headers => #{},
                                   extra_state => #{pathinfo => Pathinfo,
                                                    static => {dir, filename:join(Base, RootName)},
                                                    options => Options}}).

served(Result) ->
    case Result of
        {sendfile, 200, _Headers, {0, _Size, Path}, _Mime} ->
            {ok, Content} = file:read_file(Path),
            Content;
        Other ->
            Other
    end.

static_path_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Base) ->
             Refused = fun(Pathinfo) ->
                               {lists:flatten(io_lib:format("~0p is refused", [Pathinfo])),
                                ?_assertEqual({status, 404}, get_dir(Base, Pathinfo))}
                       end,
             [{"a file inside the root is served",
               ?_assertEqual(~"inside", served(get_dir(Base, [~"inside.txt"])))},
              {"a nested file is served",
               ?_assertEqual(~"deep", served(get_dir(Base, [~"sub", ~"deep.txt"])))},
              {"a directory serves its index file",
               ?_assertEqual(~"index", served(get_dir(Base, [~"sub"])))},
              {"a missing file is a 404",
               ?_assertEqual({status, 404}, get_dir(Base, [~"missing.txt"]))},

              Refused([~"..", ~"secret.txt"]),
              Refused([~"sub", ~"..", ~"..", ~"secret.txt"]),
              Refused([~".", ~"inside.txt"]),
              Refused([~"sub", ~".", ~"deep.txt"]),
              Refused([<<>>, ~"inside.txt"]),
              Refused([~"sub", <<>>]),
              Refused([~"../secret.txt"]),
              Refused([~"sub/../../secret.txt"]),
              Refused([~"sub/deep.txt"]),
              Refused([~"..\\secret.txt"]),
              Refused([~"sub\\deep.txt"]),
              Refused([<<"inside.txt", 0>>]),
              Refused([<<"inside.txt", 0, ".png">>]),
              Refused([unicode:characters_to_binary(filename:join(Base, "secret.txt"))]),
              Refused([~"/etc/passwd"]),
              Refused(["inside.txt"]),
              Refused([inside]),

              {"a link that stays inside the root is served",
               ?_assertEqual(~"inside", served(get_dir(Base, [~"link-inside"])))},
              {"a linked directory inside the root is served",
               ?_assertEqual(~"deep", served(get_dir(Base, [~"dir-inside", ~"deep.txt"])))},
              {"a root that is itself a link is served",
               ?_assertEqual(~"inside", served(get_dir(Base, "root-link", [~"inside.txt"], #{})))},
              Refused([~"link-outside"]),
              Refused([~"link-absolute"]),
              Refused([~"dir-outside", ~"secret.txt"]),
              {"a path that leaves through a link and resolves back inside is served",
               ?_assertEqual(~"inside", served(get_dir(Base, [~"dir-outside", ~"root", ~"inside.txt"])))},
              Refused([~"loop-a"]),
              {"an index file linked out of the root is refused",
               ?_assertEqual({status, 404}, get_dir(Base, [~"linked-index"]))},

              {"follow_external_symlinks serves a link out of the root",
               ?_assertEqual(?SECRET, served(get_dir(Base, "root", [~"link-outside"],
                                                     #{follow_external_symlinks => true})))},
              {"follow_external_symlinks still refuses dot segments",
               ?_assertEqual({status, 404}, get_dir(Base, "root", [~"..", ~"secret.txt"],
                                                    #{follow_external_symlinks => true}))}]
     end}.
