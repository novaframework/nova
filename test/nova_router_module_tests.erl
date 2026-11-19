%%% The router module of an application, when its environment names none.
%%% The Elixir modules are made here, so no Elixir is needed: the presence
%%% of 'Elixir.System' is what nova:detect_language/0 goes by.
-module(nova_router_module_tests).
-include_lib("eunit/include/eunit.hrl").

elixir_router_test_() ->
    {setup,
     fun() ->
             [loaded(Module) || Module <- ['Elixir.System', 'Elixir.NovaRouterApp.Router', 'Elixir.Aliased.Router']]
     end,
     fun unloaded/1,
     [{"an application named in snake case has its router under the camelised name",
       ?_assertEqual('Elixir.NovaRouterApp.Router', nova_router:router_module(nova_router_app))},
      {"an application named by its Elixir module has its router under that module",
       ?_assertEqual('Elixir.Aliased.Router', nova_router:router_module('Elixir.Aliased'))},
      {"the router_module of the environment comes first",
       fun() ->
               application:set_env(nova_router_app, router_module, some_other_router),
               try
                   ?assertEqual(some_other_router, nova_router:router_module(nova_router_app))
               after
                   application:unset_env(nova_router_app, router_module)
               end
       end},
      {"an application without a router is refused",
       ?_assertThrow({error, {router_module_not_found, nova_router_no_such_app}},
                     nova_router:router_module(nova_router_no_such_app))}]}.

erlang_router_test_() ->
    {setup,
     fun() -> [loaded(nova_router_app_router)] end,
     fun unloaded/1,
     [{"the router of an Erlang application is <app>_router",
       ?_assertEqual(nova_router_app_router, nova_router:router_module(nova_router_app))},
      {"a name that is an atom and no module is not a router",
       fun() ->
               ?assertEqual(non_existing, code:which(nova_router_only_an_atom_router)),
               ?assertThrow({error, {router_module_not_found, nova_router_only_an_atom}},
                            nova_router:router_module(nova_router_only_an_atom))
       end}]}.

loaded(Module) ->
    {ok, Module, Binary} = compile:forms([{attribute, 1, module, Module},
                                          {attribute, 2, export, [{routes, 1}]},
                                          {function, 3, routes, 1, [{clause, 3, [{var, 3, '_'}], [], [{nil, 3}]}]}]),
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".beam", Binary),
    Module.

unloaded(Modules) ->
    [begin
         code:purge(Module),
         code:delete(Module),
         code:purge(Module)
     end || Module <- Modules].
