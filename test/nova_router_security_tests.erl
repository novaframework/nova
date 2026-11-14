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

precedence_test_() ->
    Group = fun ?MODULE:group_callback/1,
    Host = fun ?MODULE:host_callback/1,
    {setup, fun setup/0, fun cleanup/1,
     [{"a group callback survives secure => false on the mounting application",
       ?_assertEqual(Group, secure_of(Group, #{secure => false}))},
      {"a group callback wins over a callback on the mounting application",
       ?_assertEqual(Group, secure_of(Group, #{secure => Host}))},
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
