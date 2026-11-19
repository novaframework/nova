%%% Functions that are exported for another Nova module, not for applications,
%%% stay out of the generated documentation.
-module(nova_internal_exports_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/eep48.hrl").

doc(Module, Function, Arity) ->
    {ok, #docs_v1{docs = Docs}} = code:get_doc(Module),
    [Doc] = [Doc || {{function, Name, DocArity}, _Anno, _Signature, Doc, _Metadata} <- Docs,
                    Name =:= Function, DocArity =:= Arity],
    Doc.

internal_exports_are_hidden_test_() ->
    [{lists:flatten(io_lib:format("~s:~s/~b", [Module, Function, Arity])),
      ?_assertEqual(hidden, doc(Module, Function, Arity))}
     || {Module, Function, Arity} <- [{nova_router, compile_fresh, 2},
                                      {nova_router, overrides_in_force, 1},
                                      {nova_router, secures_in_force, 1},
                                      {nova_routing_trie, canonical_segments, 1},
                                      {nova_routing_trie, is_canonical, 1},
                                      {nova_plugin_manager, ensure_plugin, 1},
                                      {nova_security_handler, deny, 1},
                                      {nova_session, client_id, 1}]].

public_exports_stay_documented_test_() ->
    [?_assertMatch(#{}, doc(nova_router, canonical_path, 1)),
     ?_assertMatch(#{}, doc(nova_router, dispatch_key, 1)),
     ?_assertMatch(#{}, doc(nova_plugin_manager, add_plugin, 1))].
