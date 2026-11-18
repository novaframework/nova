-module(nova_router_security_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/nova_router.hrl").

-define(KEY, {nova_dispatch, ?MODULE}).

-export([group_callback/1, host_callback/1, page/1]).

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova_test_sub_app, router_module, nova_test_mounted_router),
    Prev.

cleanup(Prev) ->
    nova_router:delete_dispatch(?KEY),
    application:unset_env(nova_test_sub_app, router_module),
    nova_test_mounted_router:set_routes([]),
    nova_test_helper:cleanup_nova_env(Prev).

%% A router does not store into the table it is compiled into, so what is
%% stored as it runs is stored by another process.
elsewhere(Store) ->
    {Storing, Monitor} = spawn_monitor(fun() ->
                                               exit({stored, try
                                                                 Store()
                                                             catch
                                                                 Class:Reason -> {Class, Reason}
                                                             end})
                                       end),
    receive
        {'DOWN', Monitor, process, Storing, {stored, Stored}} -> Stored
    after 5000 -> error(not_stored)
    end.

group_callback(_Req) -> true.
host_callback(_Req) -> true.
page(_Req) -> {status, 200}.

group(Security) ->
    Group = #{prefix => "/admin", routes => [{"/", fun ?MODULE:page/1, #{methods => [get]}}]},
    case Security of
        unset -> Group;
        _ -> Group#{security => Security}
    end.

secure_of(GroupSecurity, AppOptions) ->
    nova_test_mounted_router:set_routes([group(GroupSecurity)]),
    Dispatch = nova_router:compile([{nova_test_sub_app, AppOptions}], ?KEY),
    {ok, _Bindings, #nova_handler_value{secure = Secure}} =
        nova_routing_trie:find('_', ~"/admin/", ~"GET", Dispatch),
    nova_router:delete_dispatch(?KEY),
    Secure.

%% A router that takes its time can find the table of its listener
%% replaced, by a listener that was started again. What it read before is
%% not written over the new one.
replaced_table_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             Bound = Open#{prefix => "/bound", listener => web},
             nova_test_mounted_router:set_routes([Open]),
             _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          _ = nova_router:compile_fresh([], ?KEY),
                                          [Open, Bound]
                                  end),
                 ?assertError({routing_table_replaced, ?KEY}, nova_router:compile([nova_test_sub_app], ?KEY))
             after
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertEqual(error, element(1, nova_routing_trie:find('_', ~"/admin/", ~"GET",
                                                                   persistent_term:get(?KEY)))),
             ?assertEqual([], nova_router:compiled_apps(?KEY)),
             ?assertEqual([], nova_router:bound_groups(?KEY))
     end}.

%% Nothing is merged into the table of a listener while the one that takes
%% its place is built. What is under the key then is the table before it,
%% and a merge into that would be stored over the one that was built.
rebuilt_table_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             Test = self(),
             nova_test_mounted_router:set_routes([Open]),
             _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) when self() =:= Test ->
                                          Test ! {merged, elsewhere(fun() ->
                                                                            nova_router:compile([nova_test_sub_app], ?KEY)
                                                                    end)},
                                          Test ! {added, elsewhere(fun() ->
                                                                           nova_router:add_routes(
                                                                             nova_test_app, Open#{prefix => "/added"}, ?KEY)
                                                                   end)},
                                          Test ! {removed, elsewhere(fun() ->
                                                                             nova_router:remove_application(
                                                                               nova_test_sub_app, ?KEY)
                                                                     end)},
                                          Test ! {built, elsewhere(fun() ->
                                                                           nova_router:compile_fresh([], ?KEY)
                                                                   end)},
                                          [Open#{prefix => "/fresh"}];
                                     (_Env) ->
                                          [Open]
                                  end),
                 _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY)
             after
                 meck:unload(nova_test_mounted_router)
             end,
             Refused = {error, {routing_table_replaced, ?KEY}},
             ?assertEqual(Refused, receive {merged, Merged} -> Merged after 0 -> none end),
             ?assertEqual(Refused, receive {added, Added} -> Added after 0 -> none end),
             ?assertEqual(Refused, receive {removed, Removed} -> Removed after 0 -> none end),
             ?assertEqual(Refused, receive {built, Built} -> Built after 0 -> none end),
             Found = fun(Path) -> element(1, nova_routing_trie:find('_', Path, ~"GET", persistent_term:get(?KEY))) end,
             ?assertEqual([error, error, ok], [Found(Path) || Path <- [~"/admin/", ~"/added/", ~"/fresh/"]]),
             ?assertEqual([{nova_test_sub_app, "/"}], nova_router:compiled_apps(?KEY)),
             ok = nova_router:add_routes(nova_test_app, Open#{prefix => "/after"}, ?KEY),
             ?assertEqual(ok, Found(~"/after/"))
     end}.

%% A router that stores into the table it is compiled into would leave
%% what it stored there while its own routes are refused. It is thrown at,
%% as for its other mistakes, as a listener starts and as routes are
%% merged alike, and nothing is stored.
own_table_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             Side = Open#{prefix => "/side"},
             InUse = {error, {routing_table_in_use, ?KEY}},
             Found = fun(Path) -> element(1, nova_routing_trie:find('_', Path, ~"GET", persistent_term:get(?KEY))) end,
             Stores = [fun() -> nova_router:add_routes(nova_test_app, Side, ?KEY) end,
                       fun() -> nova_router:compile([nova_test_app], ?KEY) end,
                       fun() -> nova_router:remove_application(nova_test_sub_app, ?KEY) end],
             Group = #{prefix => "/g", apps => [nova_test_sub_app], routes => []},
             Compiles = [fun() -> nova_router:compile_fresh([nova_test_sub_app], ?KEY) end,
                         fun() -> nova_router:compile([nova_test_sub_app], ?KEY) end,
                         fun() -> nova_router:add_routes(nova_test_app, Group, ?KEY) end],
             nova_test_mounted_router:set_routes([Open]),
             _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 _ = [begin
                          ok = meck:expect(nova_test_mounted_router, routes,
                                           fun(_Env) ->
                                                   _ = Store(),
                                                   [Open#{prefix => "/own"}]
                                           end),
                          ?assertThrow(InUse, Compile()),
                          ?assertEqual([ok, error, error], [Found(Path) || Path <- [~"/admin/", ~"/side/", ~"/own/"]]),
                          ?assertEqual([{nova_test_sub_app, "/"}], nova_router:compiled_apps(?KEY))
                      end || Store <- Stores, Compile <- Compiles],
                 ok = meck:expect(nova_test_mounted_router, routes, fun(_Env) -> [Open#{prefix => "/own"}] end),
                 _ = nova_router:compile([nova_test_sub_app], ?KEY),
                 ?assertEqual(ok, Found(~"/own/")),
                 ?assertEqual(ok, nova_router:add_routes(nova_test_app, Side, ?KEY))
             after
                 meck:unload(nova_test_mounted_router)
             end
     end}.

%% A table that does not compile for a listener that starts is not left as
%% one that is being built: the one before it takes routes again.
failed_build_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             _ = nova_router:compile_fresh([], ?KEY),
             ?assertThrow({error, _}, nova_router:compile_fresh([nova_test_no_such_app], ?KEY)),
             nova_test_mounted_router:set_routes([group(unset)]),
             _ = nova_router:compile([nova_test_sub_app], ?KEY),
             ?assertEqual([{nova_test_sub_app, "/"}], nova_router:compiled_apps(?KEY))
     end}.

%% Routes are added to the table that was read. One that was replaced
%% while the routers of the group ran takes none of them.
added_replaced_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             _ = nova_router:compile_fresh([], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          _ = nova_router:compile_fresh([], ?KEY),
                                          [group(unset)]
                                  end),
                 ?assertError({routing_table_replaced, ?KEY},
                              nova_router:add_routes(nova_test_app,
                                                     #{prefix => "/g", apps => [nova_test_sub_app], routes => []},
                                                     ?KEY))
             after
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertEqual([], nova_router:compiled_apps(?KEY))
     end}.

%% The applications a group of added routes mounts are listed with the
%% table, as those of a compile are.
added_sub_apps_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             _ = nova_router:compile_fresh([], ?KEY),
             nova_test_mounted_router:set_routes([group(unset)]),
             ok = nova_router:add_routes(nova_test_app, #{prefix => "/g", apps => [nova_test_sub_app], routes => []},
                                         ?KEY),
             ?assertEqual([{nova_test_sub_app, "/g"}, {nova_test_app, "/"}], nova_router:compiled_apps(?KEY))
     end}.

%% A build that fails while another process asks to build the table is
%% the one whose mark is taken away: the other was refused, and left none.
failed_beside_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             _ = nova_router:compile_fresh([], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          case get(?FUNCTION_NAME) of
                                              undefined ->
                                                  put(?FUNCTION_NAME,
                                                      elsewhere(fun() -> nova_router:compile_fresh([], ?KEY) end)),
                                                  error(fails);
                                              _Failed ->
                                                  [Open]
                                          end
                                  end),
                 ?assertError(fails, nova_router:compile_fresh([nova_test_sub_app], ?KEY)),
                 ?assertEqual({error, {routing_table_replaced, ?KEY}}, get(?FUNCTION_NAME)),
                 _ = nova_router:compile([nova_test_sub_app], ?KEY)
             after
                 erase(?FUNCTION_NAME),
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertEqual([{nova_test_sub_app, "/"}], nova_router:compiled_apps(?KEY))
     end}.

%% A process that is stopped as it builds a table leaves it marked. The
%% table before it takes routes again, as no table is being built.
stopped_build_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Test = self(),
             _ = nova_router:compile_fresh([], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) when self() =:= Test ->
                                          [group(unset)];
                                     (_Env) ->
                                          Test ! building,
                                          receive after infinity -> [] end
                                  end),
                 {Builder, Monitor} = spawn_monitor(fun() -> nova_router:compile_fresh([nova_test_sub_app], ?KEY) end),
                 receive building -> ok after 5000 -> error(not_building) end,
                 ?assertError({routing_table_replaced, ?KEY}, nova_router:compile([nova_test_sub_app], ?KEY)),
                 exit(Builder, kill),
                 receive {'DOWN', Monitor, process, Builder, killed} -> ok after 5000 -> error(not_stopped) end,
                 _ = nova_router:compile([nova_test_sub_app], ?KEY)
             after
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertEqual([{nova_test_sub_app, "/"}], nova_router:compiled_apps(?KEY))
     end}.

%% The mark a stopped build left is the build of the table until routes
%% are stored into it. Two merges that read it are two merges into one
%% table: the one that stores last is told that the table changed, and is
%% compiled again where routes are added.
dead_build_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Test = self(),
             Open = group(unset),
             Group = #{prefix => "/g", apps => [nova_test_sub_app], routes => []},
             Found = fun(Path) -> element(1, nova_routing_trie:find('_', Path, ~"GET", persistent_term:get(?KEY))) end,
             _ = nova_router:compile_fresh([], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) when self() =:= Test ->
                                          case put(?FUNCTION_NAME, stored) of
                                              undefined ->
                                                  ok = elsewhere(fun() ->
                                                                         nova_router:add_routes(
                                                                           nova_test_app, Open#{prefix => "/other"}, ?KEY)
                                                                 end);
                                              stored ->
                                                  ok
                                          end,
                                          [Open];
                                     (_Env) ->
                                          Test ! building,
                                          receive after infinity -> [] end
                                  end),
                 {Builder, Monitor} = spawn_monitor(fun() -> nova_router:compile_fresh([nova_test_sub_app], ?KEY) end),
                 receive building -> ok after 5000 -> error(not_building) end,
                 exit(Builder, kill),
                 receive {'DOWN', Monitor, process, Builder, killed} -> ok after 5000 -> error(not_stopped) end,
                 ?assertEqual(ok, nova_router:add_routes(nova_test_app, Group, ?KEY)),
                 ?assertEqual([ok, ok], [Found(Path) || Path <- [~"/other/", ~"/g/admin/"]])
             after
                 erase(?FUNCTION_NAME),
                 meck:unload(nova_test_mounted_router)
             end
     end}.

%% An application that is removed while the routers of a compile run is
%% not put back by what that compile read before.
removed_meanwhile_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             nova_test_mounted_router:set_routes([Open]),
             _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          ok = elsewhere(fun() ->
                                                                 nova_router:remove_application(nova_test_sub_app, ?KEY)
                                                         end),
                                          [Open#{prefix => "/again"}]
                                  end),
                 ?assertError({routing_table_changed, ?KEY}, nova_router:compile([nova_test_sub_app], ?KEY))
             after
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertEqual(error, element(1, nova_routing_trie:find('_', ~"/admin/", ~"GET", persistent_term:get(?KEY)))),
             ?assertEqual([], nova_router:compiled_apps(?KEY))
     end}.

%% Of two merges that read the same table, the one that stores last has
%% none of what the other one stored. A route that was given a security
%% callback meanwhile is not left open by it.
secured_meanwhile_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             Callback = fun ?MODULE:group_callback/1,
             _ = nova_router:compile_fresh([], ?KEY),
             ok = nova_router:add_routes(nova_test_app, Open, ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          ok = elsewhere(fun() ->
                                                                 nova_router:add_routes(nova_test_app, group(Callback), ?KEY)
                                                         end),
                                          [Open#{prefix => "/sub"}]
                                  end),
                 ?assertError({routing_table_changed, ?KEY}, nova_router:compile([nova_test_sub_app], ?KEY))
             after
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertMatch({ok, _Bindings, #nova_handler_value{secure = Callback}},
                          nova_routing_trie:find('_', ~"/admin/", ~"GET", persistent_term:get(?KEY))),
             nova_test_mounted_router:set_routes([Open#{prefix => "/sub"}]),
             _ = nova_router:compile([nova_test_sub_app], ?KEY),
             ?assertMatch({ok, _Bindings, #nova_handler_value{secure = Callback}},
                          nova_routing_trie:find('_', ~"/admin/", ~"GET", persistent_term:get(?KEY))),
             ?assertEqual(ok, element(1, nova_routing_trie:find('_', ~"/sub/", ~"GET", persistent_term:get(?KEY))))
     end}.

%% Routes that are stored as others are added to the table are no reason
%% not to add those: they are compiled again into the table as it is by
%% then. Not for ever: a table that changes each time is given up on.
added_again_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             Group = #{prefix => "/g", apps => [nova_test_sub_app], routes => []},
             Other = fun() ->
                             elsewhere(fun() -> nova_router:add_routes(nova_test_app, Open#{prefix => "/other"}, ?KEY) end)
                     end,
             Found = fun(Path) -> element(1, nova_routing_trie:find('_', Path, ~"GET", persistent_term:get(?KEY))) end,
             _ = nova_router:compile_fresh([], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          case put(?FUNCTION_NAME, stored) of
                                              undefined -> ok = Other();
                                              stored -> ok
                                          end,
                                          [Open]
                                  end),
                 ?assertEqual(ok, nova_router:add_routes(nova_test_app, Group, ?KEY)),
                 ?assertEqual(2, meck:num_calls(nova_test_mounted_router, routes, '_')),
                 ?assertEqual([ok, ok], [Found(Path) || Path <- [~"/other/", ~"/g/admin/"]]),
                 ok = meck:reset(nova_test_mounted_router),
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          ok = Other(),
                                          [Open#{prefix => "/never"}]
                                  end),
                 ?assertError({routing_table_changed, ?KEY}, nova_router:add_routes(nova_test_app, Group, ?KEY)),
                 ?assertEqual(3, meck:num_calls(nova_test_mounted_router, routes, '_')),
                 ?assertEqual(error, Found(~"/g/never/"))
             after
                 erase(?FUNCTION_NAME),
                 meck:unload(nova_test_mounted_router)
             end
     end}.

%% Of merges that store at once, none is stored over another: every one
%% that is answered is in the table.
racing_merges_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     {timeout, 60,
      fun() ->
              _ = nova_router:compile_fresh([], ?KEY),
              Test = self(),
              Open = group(unset),
              Add = fun(Prefix) ->
                            try nova_router:add_routes(nova_test_app, Open#{prefix => Prefix}, ?KEY) of
                                ok -> {added, Prefix}
                            catch
                                error:{routing_table_changed, ?KEY} -> refused
                            end
                    end,
              Racers = [spawn(fun() -> receive go -> Test ! {raced, self(), Add("/r" ++ integer_to_list(I))} end end)
                        || I <- lists:seq(1, 16)],
              _ = [Racer ! go || Racer <- Racers],
              Results = [receive {raced, Racer, Result} -> Result after 30000 -> error(not_raced) end
                         || Racer <- Racers],
              Found = fun(Prefix) ->
                              element(1, nova_routing_trie:find('_', list_to_binary(Prefix ++ "/"), ~"GET",
                                                                persistent_term:get(?KEY)))
                      end,
              ?assertNotEqual([], [Prefix || {added, Prefix} <- Results]),
              ?assertEqual([], [Prefix || {added, Prefix} <- Results, Found(Prefix) =/= ok])
      end}}.

%% Routes that are stored as an application is taken out of the table are
%% kept: the application is taken out of the table as it is by then.
removed_again_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             nova_test_mounted_router:set_routes([Open]),
             _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY),
             ok = meck:new(nova_routing_trie, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_routing_trie, foldl,
                                  fun(Trie, Each) ->
                                          case put(?FUNCTION_NAME, stored) of
                                              undefined ->
                                                  ok = nova_router:add_routes(nova_test_app, Open#{prefix => "/added"},
                                                                              ?KEY);
                                              stored ->
                                                  ok
                                          end,
                                          meck:passthrough([Trie, Each])
                                  end),
                 ?assertEqual(ok, nova_router:remove_application(nova_test_sub_app, ?KEY))
             after
                 erase(?FUNCTION_NAME),
                 meck:unload(nova_routing_trie)
             end,
             Found = fun(Path) -> element(1, nova_routing_trie:find('_', Path, ~"GET", persistent_term:get(?KEY))) end,
             ?assertEqual([error, ok], [Found(Path) || Path <- [~"/admin/", ~"/added/"]]),
             ?assertEqual([{nova_test_app, "/"}], nova_router:compiled_apps(?KEY))
     end}.

failed_compile_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
             Open = group(unset),
             _ = nova_router:compile_fresh([], ?KEY),
             nova_test_mounted_router:set_routes([Open, Open#{prefix => "/bound", listener => web}]),
             ?assertThrow({error, _}, nova_router:compile([nova_test_sub_app, nova_test_no_such_app], ?KEY)),
             ?assertEqual([], nova_router:compiled_apps(?KEY)),
             ?assertEqual([], nova_router:bound_groups(?KEY)),
             ?assertEqual(error, element(1, nova_routing_trie:find('_', ~"/admin/", ~"GET",
                                                                   persistent_term:get(?KEY)))),
             _ = nova_router:compile([nova_test_sub_app], ?KEY),
             ?assertEqual([{nova_test_sub_app, "/"}], nova_router:compiled_apps(?KEY)),
             ?assertEqual([{skipped, nova_test_sub_app, "/bound", [web]}], nova_router:bound_groups(?KEY))
     end}.

precedence_test_() ->
    Group = fun ?MODULE:group_callback/1,
    Host = fun ?MODULE:host_callback/1,
    {setup, fun setup/0, fun cleanup/1,
     [{"a group callback survives secure => false on the mounting application",
       ?_assertEqual(Group, secure_of(Group, #{secure => false}))},
      {"a callback on the mounting application runs in front of a group callback",
       ?_assertEqual([Host, Group], secure_of(Group, #{secure => Host}))},
      {"a callback that is on both runs once",
       ?_assertEqual(Host, secure_of(Host, #{secure => Host}))},
      {"the deprecated {Mod, Fun} form is accepted on both",
       ?_assertEqual([Host, Group], secure_of({?MODULE, group_callback}, #{secure => {?MODULE, host_callback}}))},
      {"the application callback is the default for a group that sets none",
       ?_assertEqual(Host, secure_of(unset, #{secure => Host}))},
      {"the application callback still protects a group that declares false",
       ?_assertEqual(Host, secure_of(false, #{secure => Host}))},
      {"override_secure replaces a group callback",
       ?_assertEqual(Host, secure_of(Group, #{override_secure => Host}))},
      {"override_secure => false is no override",
       ?_assertEqual(Group, secure_of(Group, #{override_secure => false}))},
      {"override_secure wins over secure",
       ?_assertEqual(Host, secure_of(unset, #{secure => Group, override_secure => Host}))},
      {"no callback anywhere leaves the group open",
       ?_assertEqual(false, secure_of(unset, #{}))},
      {"the deprecated {Mod, Fun} form is still accepted on a group",
       ?_assertEqual(Group, secure_of({?MODULE, group_callback}, #{secure => false}))}]}.

inherited_override_test_() ->
    Group = fun ?MODULE:group_callback/1,
    Host = fun ?MODULE:host_callback/1,
    Other = fun ?MODULE:page/1,
    {setup, fun setup/0, fun cleanup/1,
     [{"an inherited override_secure replaces a group callback",
       ?_assertEqual(Host, secure_of(Group, #{inherited_override_secure => [Host]}))},
      {"an inherited override_secure stays in force next to the application's own",
       ?_assertEqual([Host, Other], secure_of(Group, #{inherited_override_secure => [Host], override_secure => Other}))},
      {"inherited values are kept in their order, in front of the application's own",
       ?_assertEqual([Other, Host, Group],
                     secure_of(unset, #{inherited_override_secure => [Other, Host], override_secure => Group}))},
      {"override_secure => false leaves an inherited one in force",
       ?_assertEqual(Host, secure_of(Group, #{inherited_override_secure => [Host], override_secure => false}))},
      {"a callback that is in force twice runs once",
       [?_assertEqual(Host, secure_of(Group, #{inherited_override_secure => [Host], override_secure => Host})),
        ?_assertEqual([Host, Other],
                      secure_of(Group, #{inherited_override_secure => [Host, Other, Host], override_secure => Other}))]},
      {"the deprecated {Mod, Fun} form is accepted for an inherited value",
       ?_assertEqual([Host, Other],
                     secure_of(unset, #{inherited_override_secure => [{?MODULE, host_callback}],
                                        override_secure => Other}))},
      {"an invalid inherited value stops the compile",
       ?_assertThrow({error, {invalid_security, "my_auth:check"}},
                     secure_of(unset, #{inherited_override_secure => ["my_auth:check"], override_secure => Host}))},
      {"an invalid override_secure stops the compile next to an inherited one",
       ?_assertThrow({error, {invalid_security, true}},
                     secure_of(unset, #{inherited_override_secure => [Host], override_secure => true}))}]}.

inherited_secure_test_() ->
    Group = fun ?MODULE:group_callback/1,
    Host = fun ?MODULE:host_callback/1,
    Other = fun ?MODULE:page/1,
    {setup, fun setup/0, fun cleanup/1,
     [{"an inherited secure is the default for a group that sets none",
       ?_assertEqual(Host, secure_of(unset, #{inherited_secure => [Host]}))},
      {"an inherited secure runs in front of the application's own and of a group callback",
       ?_assertEqual([Other, Host, Group], secure_of(Group, #{inherited_secure => [Other], secure => Host}))},
      {"secure => false leaves an inherited one in force",
       ?_assertEqual([Host, Group], secure_of(Group, #{inherited_secure => [Host], secure => false}))},
      {"a callback that is in force twice runs once",
       ?_assertEqual([Host, Other], secure_of(Host, #{inherited_secure => [Host, Other], secure => Other}))},
      {"override_secure replaces an inherited secure",
       [?_assertEqual(Other, secure_of(Group, #{inherited_secure => [Host], override_secure => Other})),
        ?_assertEqual(Other, secure_of(Group, #{inherited_secure => [Host], inherited_override_secure => [Other]}))]},
      {"the deprecated {Mod, Fun} form is accepted for an inherited value",
       ?_assertEqual([Host, Group], secure_of(Group, #{inherited_secure => [{?MODULE, host_callback}]}))},
      {"an invalid inherited value stops the compile, also where an override_secure replaces it",
       [?_assertThrow({error, {invalid_security, "my_auth:check"}},
                      secure_of(unset, #{inherited_secure => ["my_auth:check"]})),
        ?_assertThrow({error, {invalid_security, "my_auth:check"}},
                      secure_of(unset, #{inherited_secure => ["my_auth:check"], override_secure => Host}))]}]}.

secures_in_force_test_() ->
    Host = fun ?MODULE:host_callback/1,
    Other = fun ?MODULE:page/1,
    [?_assertEqual([], nova_router:secures_in_force(#{})),
     ?_assertEqual([], nova_router:secures_in_force(#{prefix => "/admin", secure => false, override_secure => Host})),
     ?_assertEqual([Host], nova_router:secures_in_force(#{secure => Host})),
     ?_assertEqual([Other, Host], nova_router:secures_in_force(#{inherited_secure => [Other], secure => Host})),
     ?_assertEqual([Other], nova_router:secures_in_force(#{inherited_secure => [Other], secure => false}))].

%% The applications a route group lists under `apps` are behind the security
%% of that group.
apps_of_a_group_test_() ->
    Group = fun ?MODULE:group_callback/1,
    Host = fun ?MODULE:host_callback/1,
    Other = fun ?MODULE:page/1,
    SecureOf = fun(GroupSecurity, Apps) ->
                       nova_test_mounted_router:set_routes([group(Group)]),
                       Outer = (group(GroupSecurity))#{prefix => "/outer", routes => [], apps => Apps},
                       try
                           _ = nova_router:compile_fresh([], ?KEY),
                           ok = nova_router:add_routes(security_tests_app, [Outer], ?KEY),
                           {ok, _Bindings, #nova_handler_value{secure = Secure}} =
                               nova_router:lookup_url('_', ~"/outer/admin/", ~"GET", ?KEY),
                           Secure
                       after
                           nova_router:delete_dispatch(?KEY)
                       end
               end,
    {setup, fun setup/0, fun cleanup/1,
     [{"the security of the group runs in front of that of the application's own group",
       ?_assertEqual([Host, Group], SecureOf(Host, [nova_test_sub_app]))},
      {"and in front of the secure the application is listed with",
       ?_assertEqual([Host, Other, Group], SecureOf(Host, [{nova_test_sub_app, #{secure => Other}}]))},
      {"a group without security adds nothing",
       ?_assertEqual(Group, SecureOf(unset, [nova_test_sub_app]))},
      {"override_secure on the entry of the application replaces it",
       ?_assertEqual(Other, SecureOf(Host, [{nova_test_sub_app, #{override_secure => Other}}]))}]}.

misplaced_security_on_an_entry_test_() ->
    Host = fun ?MODULE:host_callback/1,
    {setup, fun setup/0, fun cleanup/1,
     [{"a callback written as security on the entry of an application stops the compile",
       [?_assertThrow({error, {misplaced_security, nova_test_sub_app, security, "/"}},
                      secure_of(unset, #{security => Host})),
        ?_assertThrow({error, {misplaced_security, nova_test_sub_app, security, "/admin"}},
                      secure_of(unset, #{prefix => "/admin", security => Host, secure => Host}))]},
      {"security => false there is only warned about",
       fun() ->
               {Secure, Events} = nova_test_log:collect(fun() -> secure_of(unset, #{security => false, secure => Host}) end),
               ?assertEqual(Host, Secure),
               ?assertMatch([#{app := nova_test_sub_app, key := security, path := "/"}],
                            [Report || {warning, {report, Report}} <- Events])
       end}]}.

overrides_in_force_test_() ->
    Host = fun ?MODULE:host_callback/1,
    Other = fun ?MODULE:page/1,
    [?_assertEqual([], nova_router:overrides_in_force(#{})),
     ?_assertEqual([], nova_router:overrides_in_force(#{prefix => "/admin", secure => Host, override_secure => false})),
     ?_assertEqual([Host], nova_router:overrides_in_force(#{override_secure => Host})),
     ?_assertEqual([Other, Host],
                   nova_router:overrides_in_force(#{inherited_override_secure => [Other], override_secure => Host})),
     ?_assertEqual([Other],
                   nova_router:overrides_in_force(#{inherited_override_secure => [Other], override_secure => false})),
     ?_assertEqual([true], nova_router:overrides_in_force(#{override_secure => true}))].

invalid_value_stops_the_compile_test_() ->
    Host = fun ?MODULE:host_callback/1,
    {setup, fun setup/0, fun cleanup/1,
     [{"on a route group",
       ?_assertThrow({error, {invalid_security, true}}, secure_of(true, #{}))},
      {"on a route group, with a callback on the mounting application",
       ?_assertThrow({error, {invalid_security, enabled}}, secure_of(enabled, #{secure => Host}))},
      {"as secure on the mounting application",
       ?_assertThrow({error, {invalid_security, true}}, secure_of(unset, #{secure => true}))},
      {"as secure on the mounting application, with a callback on the route group",
       ?_assertThrow({error, {invalid_security, true}},
                     secure_of(fun ?MODULE:group_callback/1, #{secure => true}))},
      {"as override_secure on the mounting application",
       ?_assertThrow({error, {invalid_security, "my_auth:check"}},
                     secure_of(unset, #{override_secure => "my_auth:check"}))},
      {"on a route group whose callback an override_secure replaces",
       [?_assertThrow({error, {invalid_security, enabled}}, secure_of(enabled, #{override_secure => Host})),
        ?_assertThrow({error, {invalid_security, enabled}},
                      secure_of(enabled, #{inherited_override_secure => [Host]}))]},
      {"as secure on the mounting application, which an override_secure replaces",
       ?_assertThrow({error, {invalid_security, true}},
                     secure_of(unset, #{secure => true, override_secure => Host}))},
      {"as override_secure first, where the values it replaces are invalid too",
       ?_assertThrow({error, {invalid_security, "my_auth:check"}},
                     secure_of(enabled, #{secure => true, override_secure => "my_auth:check"}))},
      {"and leaves the routing table as it was",
       fun() ->
               ?assertThrow({error, {invalid_security, true}}, secure_of(true, #{})),
               ?assertEqual([], nova_router:compiled_apps(?KEY))
       end}]}.
