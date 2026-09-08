%%% Router for the end-to-end test application.
%%%
%%% Between this module and nova_test_sub_app_router, every routing feature
%%% Nova documents should appear at least once. If you add a feature to the
%%% router, add a route for it here and a case to nova_full_app_SUITE.
-module(nova_test_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Environment) ->
    [
     %% Plain routes, bindings, methods, plugins and a catch-all static dir.
     #{prefix => "",
       security => false,
       plugins => [{pre_request, nova_test_app_plugin, #{}},
                   {post_request, nova_test_app_plugin, #{}}],
       routes => [
                  {"/", fun nova_test_app_controller:index/1, #{methods => [get]}},
                  {"/json", fun nova_test_app_controller:json/1, #{methods => [get]}},
                  {"/echo/:id", fun nova_test_app_controller:echo/1, #{methods => [get]}},
                  {"/echo/:id/comments/:comment_id",
                   fun nova_test_app_controller:echo/1, #{methods => [get]}},
                  {"/users/new", fun nova_test_app_controller:literal/1, #{methods => [get]}},
                  {"/users/:id", fun nova_test_app_controller:echo/1, #{methods => [get]}},
                  {"/methods", fun nova_test_app_controller:method/1,
                   #{methods => [get, post, put, delete, patch]}},
                  {"/any-method", fun nova_test_app_controller:method/1, #{}},
                  {"/get-only", fun nova_test_app_controller:method/1, #{methods => [get]}},
                  {"/redirect", fun nova_test_app_controller:redirect/1, #{methods => [get]}},
                  {"/teapot", fun nova_test_app_controller:teapot/1, #{methods => [get]}},
                  {"/crash", fun nova_test_app_controller:crash/1, #{methods => [get]}},
                  {"/extra", fun nova_test_app_controller:extra/1,
                   #{methods => [get], extra_state => #{answer => 42}}},
                  {"/assets/[...]", "assets"},
                  {"/ws", nova_test_app_ws_controller, #{protocol => ws}},

                  %% Status-code routes. Nova registers its own 404 and 500,
                  %% and an application may override them.
                  {404, fun nova_test_app_controller:not_found/1, #{}}
                 ]},

     %% A prefixed group behind a security callback.
     #{prefix => "/secure",
       security => fun nova_test_app_security:check/1,
       routes => [
                  {"/", fun nova_test_app_controller:secret/1, #{methods => [get]}},
                  {"/data", fun nova_test_app_controller:auth_data/1, #{methods => [get]}}
                 ]},

     %% A host-scoped route. Only served when the Host header matches.
     #{prefix => "",
       host => <<"api.localhost">>,
       routes => [
                  {"/host", fun nova_test_app_controller:host/1, #{methods => [get]}}
                 ]}
    ].
