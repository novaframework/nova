-module(nova_erlydtl_inventory_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    Prev = nova_test_helper:setup_nova_env(),
    application:set_env(nova, dispatch_backend, persistent_term),
    persistent_term:put(nova_apps, [{my_app, "/prefix"}, {root_app, "/"}]),
    Prev.

cleanup(Prev) ->
    persistent_term:erase(nova_apps),
    nova_test_helper:cleanup_nova_env(Prev).

url_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"the prefix of the application is put in front of the url",
       ?_assertEqual(~"/prefix/users",
                     nova_erlydtl_inventory:url([~"/users", {application, ~"my_app"}], []))},
      {"a prefix that ends where the url starts gives one slash",
       ?_assertEqual(~"/users",
                     nova_erlydtl_inventory:url([~"/users", {application, ~"root_app"}], []))},
      {"an application that is not known gives # and its name does not become an atom",
       fun() ->
               Name = ~"nova_inventory_unknown_app_4f1c",
               ?assertEqual(~"#", nova_erlydtl_inventory:url([~"/x", {application, Name}], [])),
               ?assertError(badarg, binary_to_existing_atom(Name))
       end}]}.
