%%% Nova runs on the server of a server package, so nothing in the nova
%%% application may call a server itself.
-module(nova_server_free_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVERS, ["cowboy", "ranch", "roadrunner"]).

nova_calls_no_server_test() ->
    _ = application:load(nova),
    {ok, Modules} = application:get_key(nova, modules),
    ?assertNotEqual([], Modules),
    ?assertEqual([], [{Module, Called} || Module <- Modules, Called <- servers_called(Module)]).

nova_depends_on_no_server_test() ->
    _ = application:load(nova),
    {ok, Applications} = application:get_key(nova, applications),
    ?assertEqual([], [Application || Application <- Applications, is_server(Application)]).

servers_called(Module) ->
    {ok, {Module, [{imports, Imports}]}} = beam_lib:chunks(code:which(Module), [imports]),
    lists:usort([Called || {Called, _Function, _Arity} <- Imports, is_server(Called)]).

is_server(Module) ->
    Name = atom_to_list(Module),
    lists:any(fun(Server) -> lists:prefix(Server, Name) end, ?SERVERS).
