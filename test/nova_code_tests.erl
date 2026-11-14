-module(nova_code_tests).
-include_lib("eunit/include/eunit.hrl").

-define(KNOWN, "nova_code_fixture_known").

setup() ->
    Dir = filename:absname(filename:join(["_build", "test", "tmp", atom_to_list(?MODULE)])),
    _ = file:del_dir_r(Dir),
    ok = filelib:ensure_path(Dir),
    true = code:add_patha(Dir),
    Dir.

cleanup(Dir) ->
    true = code:del_path(Dir),
    ok = file:del_dir_r(Dir).

%% A module file for a name this node has no atom for. The compiler needs
%% the name as an atom, so the file is compiled under another name of the
%% same length and the name is replaced in it.
write_module(Dir, Name) ->
    {ok, nova_code_fixture_known, Beam} =
        compile:forms([{attribute, 1, module, nova_code_fixture_known},
                       {attribute, 2, export, [{name, 0}]},
                       {function, 3, name, 0, [{clause, 3, [], [], [{atom, 3, ok}]}]}]),
    true = length(Name) =:= length(?KNOWN),
    Renamed = binary:replace(Beam, list_to_binary(?KNOWN), list_to_binary(Name), [global]),
    ok = file:write_file(filename:join(Dir, Name ++ ".beam"), Renamed).

%% Put together when the test runs, so the name is not an atom of this module.
name(Suffix) ->
    "nova_code_fixture_" ++ Suffix.

is_atom_already(Name) ->
    try list_to_existing_atom(Name) of
        _Atom -> true
    catch
        error:badarg -> false
    end.

module_named_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Dir) ->
             [{"a loaded module is found by its name",
               ?_assertEqual({ok, lists}, nova_code:module_named("lists"))},
              {"a name that is an atom already is that atom, with or without a module",
               ?_assertEqual({ok, nova_code_tests_names_no_module}, nova_code:module_named("nova_code_tests_names_no_module"))},
              {"a module in the code path is loaded, and only then is its name an atom",
               fun() ->
                       Name = name("fresh"),
                       ok = write_module(Dir, Name),
                       ?assertNot(is_atom_already(Name)),
                       {ok, Module} = nova_code:module_named(Name),
                       ?assertEqual(Name, atom_to_list(Module)),
                       ?assertEqual(ok, Module:name())
               end},
              {"a name no module in the code path has is not made an atom",
               fun() ->
                       Name = name("never"),
                       ?assertEqual(error, nova_code:module_named(Name)),
                       ?assertNot(is_atom_already(Name))
               end},
              {"a name that points out of the code path is not looked for",
               fun() ->
                       Name = name("apart"),
                       ok = filelib:ensure_path(filename:join(Dir, "sub")),
                       ok = write_module(filename:join(Dir, "sub"), Name),
                       ?assertEqual(error, nova_code:module_named(filename:join("sub", Name))),
                       ?assertNot(is_atom_already(Name))
               end}]
     end}.
