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
             nova_test_mounted_router:set_routes([Open]),
             _ = nova_router:compile_fresh([nova_test_sub_app], ?KEY),
             ok = meck:new(nova_test_mounted_router, [passthrough, no_link]),
             try
                 ok = meck:expect(nova_test_mounted_router, routes,
                                  fun(_Env) ->
                                          _ = nova_router:compile_fresh([], ?KEY),
                                          [Open]
                                  end),
                 ?assertError({routing_table_replaced, ?KEY}, nova_router:compile([nova_test_sub_app], ?KEY))
             after
                 meck:unload(nova_test_mounted_router)
             end,
             ?assertEqual(error, element(1, nova_routing_trie:find('_', ~"/admin/", ~"GET",
                                                                   persistent_term:get(?KEY))))
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
