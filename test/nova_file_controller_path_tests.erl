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
    ok = filelib:ensure_path(filename:join([Base, "cwd-compiled", "static"])),
    ok = filelib:ensure_path(filename:join([Base, "cwd-later", "static"])),
    ok = file:write_file(filename:join([Base, "cwd-compiled", "static", "file.txt"]), ~"inside"),
    ok = file:write_file(filename:join([Base, "cwd-compiled", "static", "index.html"]), ~"index"),
    ok = file:make_symlink("../../secret.txt", filename:join([Base, "cwd-later", "static", "file.txt"])),
    ok = file:make_symlink("../../secret.txt", filename:join([Base, "cwd-later", "static", "index.html"])),
    ok = filelib:ensure_path(filename:join(Base, "release-1")),
    ok = filelib:ensure_path(filename:join(Base, "release-2")),
    ok = file:write_file(filename:join([Base, "release-1", "file.txt"]), ~"inside"),
    ok = file:write_file(filename:join([Base, "release-1", "index.html"]), ~"index"),
    ok = file:make_symlink("../secret.txt", filename:join([Base, "release-2", "file.txt"])),
    ok = file:make_symlink("../secret.txt", filename:join([Base, "release-2", "index.html"])),
    ok = file:make_symlink("release-1", filename:join(Base, "current")),
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

%% As the router calls the controller: with the state it compiled for the
%% route, and the trailing segments of the request.
get_compiled_dir(State, Pathinfo) ->
    nova_file_controller:get_dir(#{path => ~"/files", headers => #{}, extra_state => State#{pathinfo => Pathinfo}}).

%% The result of a request, and the paths it asked the file system about a
%% link for. A process cannot trace itself, so the request gets its own.
links_read(Request) ->
    Parent = self(),
    Pid = spawn_link(fun() -> receive go -> Parent ! {result, self(), Request()} end end),
    erlang:trace_pattern({file, read_link_all, 1}, true, [global]),
    erlang:trace(Pid, true, [call]),
    Pid ! go,
    Result = receive {result, Pid, Returned} -> Returned end,
    Delivered = erlang:trace_delivered(Pid),
    receive {trace_delivered, Pid, Delivered} -> ok end,
    erlang:trace_pattern({file, read_link_all, 1}, false, [global]),
    {Result, traced_links()}.

traced_links() ->
    receive
        {trace, _Pid, call, {file, read_link_all, [Path]}} -> [Path | traced_links()]
    after 0 ->
        []
    end.

%% The state of a route compiled in one working directory, and what a
%% request gets for it once the node has moved to another.
compiled_then_moved(Base, Static, Request) ->
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(filename:join(Base, "cwd-compiled")),
        State = nova_file_controller:static_state(Static, #{}),
        ok = file:set_cwd(filename:join(Base, "cwd-later")),
        served(Request(State))
    after
        ok = file:set_cwd(Cwd)
    end.

%% As compiled_then_moved/3, for a root behind a link that is pointed
%% somewhere else after the route was compiled.
compiled_then_relinked(Base, Request) ->
    Link = filename:join(Base, "current"),
    State = nova_file_controller:static_state({dir, Link}, #{}),
    try
        ok = file:delete(Link),
        ok = file:make_symlink("release-2", Link),
        served(Request(State))
    after
        ok = file:delete(Link),
        ok = file:make_symlink("release-1", Link)
    end.

index_of(State) ->
    nova_file_controller:get_dir(#{path => ~"/files", headers => #{}, extra_state => State}).

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

              {"a root the router resolved is not resolved again for a request",
               fun() ->
                       Root = filename:join(Base, "root"),
                       State = nova_file_controller:static_state({dir, Root}, #{}),
                       {Result, Links} = links_read(fun() -> get_compiled_dir(State, [~"sub", ~"deep.txt"]) end),
                       ?assertEqual(~"deep", served(Result)),
                       ?assertEqual([filename:join(Root, "sub"), filename:join([Root, "sub", "deep.txt"])], Links)
               end},
              {"a root the router resolved through a link is not resolved again",
               fun() ->
                       State = nova_file_controller:static_state({dir, filename:join(Base, "root-link")}, #{}),
                       {Result, Links} = links_read(fun() -> get_compiled_dir(State, [~"inside.txt"]) end),
                       ?assertEqual(~"inside", served(Result)),
                       ?assertEqual([filename:join([Base, "root", "inside.txt"])], Links)
               end},
              {"a root the router resolved still keeps links inside it",
               fun() ->
                       State = nova_file_controller:static_state({dir, filename:join(Base, "root-link")}, #{}),
                       ?assertEqual(~"inside", served(get_compiled_dir(State, [~"link-inside"]))),
                       ?assertEqual(~"deep", served(get_compiled_dir(State, [~"dir-inside", ~"deep.txt"]))),
                       ?assertEqual(~"index", served(get_compiled_dir(State, [~"sub"]))),
                       ?assertEqual({status, 404}, get_compiled_dir(State, [~"link-outside"])),
                       ?assertEqual({status, 404}, get_compiled_dir(State, [~"link-absolute"])),
                       ?assertEqual({status, 404}, get_compiled_dir(State, [~"dir-outside", ~"secret.txt"])),
                       ?assertEqual({status, 404}, get_compiled_dir(State, [~"linked-index"])),
                       ?assertEqual({status, 404}, get_compiled_dir(State, [~"loop-a"]))
               end},
              {"the index file of a root the router resolved is kept inside it",
               fun() ->
                       Inside = nova_file_controller:static_state({dir, filename:join([Base, "root", "sub"])}, #{}),
                       ?assertEqual(~"index", served(nova_file_controller:get_dir(#{path => ~"/files", headers => #{},
                                                                                     extra_state => Inside}))),
                       Outside = nova_file_controller:static_state({dir, filename:join([Base, "root", "linked-index"])}, #{}),
                       ?assertEqual({status, 404}, nova_file_controller:get_dir(#{path => ~"/files", headers => #{},
                                                                                  extra_state => Outside}))
               end},
              {"a directory that has moved since the route was compiled is resolved for the request",
               fun() ->
                       Compiled = nova_file_controller:static_state({dir, filename:join([Base, "root", "sub"])}, #{}),
                       Moved = Compiled#{static => {dir, filename:join(Base, "root")}},
                       ?assertEqual(~"inside", served(get_compiled_dir(Moved, [~"inside.txt"]))),
                       ?assertEqual({status, 404}, get_compiled_dir(Moved, [~"link-outside"]))
               end},
              {"a relative root is served from where it was when the route was compiled",
               ?_assertEqual(~"inside",
                             compiled_then_moved(Base, {dir, "static"},
                                                 fun(State) -> get_compiled_dir(State, [~"file.txt"]) end))},
              {"the index file of a relative root is served from where it was when the route was compiled",
               [?_assertEqual(~"index", compiled_then_moved(Base, {dir, "static"}, fun index_of/1)),
                ?_assertEqual(~"index",
                              compiled_then_moved(Base, {dir, "static"}, fun(State) -> get_compiled_dir(State, []) end))]},
              {"a root behind a link is served from where the link pointed when the route was compiled",
               ?_assertEqual(~"inside",
                             compiled_then_relinked(Base, fun(State) -> get_compiled_dir(State, [~"file.txt"]) end))},
              {"the index file of a root behind a link is served from where the link pointed",
               [?_assertEqual(~"index", compiled_then_relinked(Base, fun index_of/1)),
                ?_assertEqual(~"index",
                              compiled_then_relinked(Base, fun(State) -> get_compiled_dir(State, []) end))]},
              {"the path of a static file is absolute once the route is compiled",
               fun() ->
                       {ok, Cwd} = file:get_cwd(),
                       ?assertEqual(#{static => {file, filename:join(Cwd, "a-file")},
                                      options => #{mimetype => ~"text/plain"}},
                                    nova_file_controller:static_state({file, "a-file"}, #{mimetype => ~"text/plain"}))
               end},
              {"a file in a priv directory is looked up for the request, where a release upgrade has moved it",
               ?_assertEqual(#{static => {priv_file, nova, "a-file"}, options => #{}},
                             nova_file_controller:static_state({priv_file, nova, "a-file"}, #{}))},
              {"a relative file is served from where it was when the route was compiled",
               ?_assertEqual(~"inside",
                             compiled_then_moved(Base, {file, filename:join("static", "file.txt")},
                                                 fun(State) ->
                                                         nova_file_controller:get_file(#{headers => #{},
                                                                                         extra_state => State})
                                                 end))},
              {"a root that cannot be resolved is a 404, and is logged once",
               fun() ->
                       State = nova_file_controller:static_state({dir, filename:join([Base, "root", "loop-a"])}, #{}),
                       ok = nova_test_log:start(),
                       Results = [index_of(State), index_of(State), get_compiled_dir(State, [~"inside.txt"])],
                       Logged = [Report || #{level := error, msg := {report, Report}} <- nova_test_log:stop()],
                       ?assertEqual([{status, 404}, {status, 404}, {status, 404}], Results),
                       ?assertMatch([#{directory := _, reason := unresolved}], Logged)
               end},
              {"a root that is gone since the route was compiled is a 404, and is logged once",
               fun() ->
                       Gone = filename:join(Base, "gone"),
                       ok = filelib:ensure_path(Gone),
                       State = nova_file_controller:static_state({dir, Gone}, #{}),
                       ok = file:del_dir(Gone),
                       ok = nova_test_log:start(),
                       Results = [index_of(State), index_of(State)],
                       Logged = [Report || #{level := error, msg := {report, Report}} <- nova_test_log:stop()],
                       ?assertEqual([{status, 404}, {status, 404}], Results),
                       ?assertMatch([#{directory := Gone, reason := enoent}], Logged)
               end},

              {"follow_external_symlinks serves a link out of the root",
               ?_assertEqual(?SECRET, served(get_dir(Base, "root", [~"link-outside"],
                                                     #{follow_external_symlinks => true})))},
              {"follow_external_symlinks still refuses dot segments",
               ?_assertEqual({status, 404}, get_dir(Base, "root", [~"..", ~"secret.txt"],
                                                    #{follow_external_symlinks => true}))}]
     end}.
