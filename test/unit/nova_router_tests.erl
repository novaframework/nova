-module(nova_router_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("routing_tree/include/routing_tree.hrl").
-include("../../include/nova_router.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

router_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Simple route compilation", fun test_simple_route_compilation/0},
      {"Route with bindings compilation", fun test_route_with_bindings/0},
      {"Multiple methods route", fun test_multiple_methods/0},
      {"Route with prefix", fun test_route_with_prefix/0},
      {"Status code handler compilation", fun test_status_code_handlers/0},
      {"URL lookup - exact match", fun test_url_lookup_exact/0},
      {"URL lookup - with bindings", fun test_url_lookup_bindings/0},
      {"URL lookup - not found", fun test_url_lookup_not_found/0},
      {"URL lookup - method matching", fun test_url_lookup_method/0},
      {"Empty route list", fun test_empty_routes/0},
      {"Method to binary conversion", fun test_method_to_binary/0},
      {"String concatenation", fun test_concat_strings/0}
     ]}.

setup() ->
    %% Ensure application environment is set
    application:set_env(nova, dispatch_backend, persistent_term),
    application:set_env(nova, use_strict_routing, false),
    ok.

cleanup(_) ->
    %% Clean up persistent_term
    catch persistent_term:erase(nova_dispatch),
    catch persistent_term:erase(nova_apps),
    catch persistent_term:erase(nova_plugins),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_simple_route_compilation() ->
    %% Create a simple route
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_) -> {ok, #{}} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/users", <<"GET">>, Value, Tree),

    %% Lookup the route
    Result = routing_tree:lookup('_', <<"/users">>, <<"GET">>, Tree1),

    ?assertMatch({ok, #{}, #nova_handler_value{}}, Result).

test_route_with_bindings() ->
    %% Create a route with bindings
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_) -> {ok, #{}} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/users/:id", <<"GET">>, Value, Tree),

    %% Lookup with a specific ID
    Result = routing_tree:lookup('_', <<"/users/123">>, <<"GET">>, Tree1),

    ?assertMatch({ok, #{<<"id">> := <<"123">>}, #nova_handler_value{}}, Result).

test_multiple_methods() ->
    %% Create a route that supports multiple methods
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_) -> {ok, #{}} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/users", <<"GET">>, Value, Tree),
    Tree2 = nova_router:insert('_', "/users", <<"POST">>, Value, Tree1),

    %% Both methods should work
    GetResult = routing_tree:lookup('_', <<"/users">>, <<"GET">>, Tree2),
    PostResult = routing_tree:lookup('_', <<"/users">>, <<"POST">>, Tree2),

    ?assertMatch({ok, #{}, #nova_handler_value{}}, GetResult),
    ?assertMatch({ok, #{}, #nova_handler_value{}}, PostResult).

test_route_with_prefix() ->
    %% Test string concatenation used in prefix handling
    Prefix = "/api/v1",
    Path = "/users",
    Combined = nova_router:concat_strings(Prefix, Path),

    ?assertEqual("/api/v1/users", Combined).

test_status_code_handlers() ->
    %% Create status code handlers
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    NotFoundCallback = fun(_) -> {status, 404} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = NotFoundCallback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', 404, '_', Value, Tree),

    %% Lookup the status code handler
    Result = routing_tree:lookup('_', 404, '_', Tree1),

    ?assertMatch({ok, #{}, #nova_handler_value{}}, Result).

test_url_lookup_exact() ->
    %% Setup dispatch table
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_) -> {ok, #{}} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/exact/path", <<"GET">>, Value, Tree),
    persistent_term:put(nova_dispatch, Tree1),

    %% Lookup should find exact match
    Result = nova_router:lookup_url('_', <<"/exact/path">>, <<"GET">>),

    ?assertMatch({ok, #{}, #nova_handler_value{app = test_app}}, Result).

test_url_lookup_bindings() ->
    %% Setup dispatch table with binding
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_) -> {ok, #{}} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/users/:id/posts/:post_id", <<"GET">>, Value, Tree),
    persistent_term:put(nova_dispatch, Tree1),

    %% Lookup with specific values
    Result = nova_router:lookup_url('_', <<"/users/42/posts/99">>, <<"GET">>),

    {ok, Bindings, _Value} = Result,
    ?assertEqual(<<"42">>, maps:get(<<"id">>, Bindings)),
    ?assertEqual(<<"99">>, maps:get(<<"post_id">>, Bindings)).

test_url_lookup_not_found() ->
    %% Setup empty dispatch table
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    persistent_term:put(nova_dispatch, Tree),

    %% Lookup should return not_found
    Result = nova_router:lookup_url('_', <<"/nonexistent">>, <<"GET">>),

    ?assertEqual({error, not_found}, Result).

test_url_lookup_method() ->
    %% Setup route with specific method
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    Callback = fun(_) -> {ok, #{}} end,
    Value = #nova_handler_value{
               app = test_app,
               callback = Callback,
               plugins = [],
               secure = false,
               extra_state = #{}
              },

    Tree1 = nova_router:insert('_', "/users", <<"POST">>, Value, Tree),
    persistent_term:put(nova_dispatch, Tree1),

    %% POST should work
    PostResult = nova_router:lookup_url('_', <<"/users">>, <<"POST">>),
    ?assertMatch({ok, #{}, #nova_handler_value{}}, PostResult),

    %% GET should fail with method not allowed
    GetResult = nova_router:lookup_url('_', <<"/users">>, <<"GET">>),
    ?assertMatch({error, comparator_not_found, _}, GetResult).

test_empty_routes() ->
    %% Compiling empty route list should return empty tree
    Tree = routing_tree:new(#{use_strict => false, convert_to_binary => true}),
    {ok, Tree1, _Options} = nova_router:compile_paths([], Tree, #{app => test_app}),

    ?assertEqual(Tree, Tree1).

test_method_to_binary() ->
    %% Test all HTTP method conversions
    ?assertEqual(<<"GET">>, nova_router:method_to_binary(get)),
    ?assertEqual(<<"POST">>, nova_router:method_to_binary(post)),
    ?assertEqual(<<"PUT">>, nova_router:method_to_binary(put)),
    ?assertEqual(<<"DELETE">>, nova_router:method_to_binary(delete)),
    ?assertEqual(<<"OPTIONS">>, nova_router:method_to_binary(options)),
    ?assertEqual(<<"HEAD">>, nova_router:method_to_binary(head)),
    ?assertEqual(<<"CONNECT">>, nova_router:method_to_binary(connect)),
    ?assertEqual(<<"TRACE">>, nova_router:method_to_binary(trace)),
    ?assertEqual(<<"PATCH">>, nova_router:method_to_binary(patch)),
    ?assertEqual('_', nova_router:method_to_binary(unknown_method)).

test_concat_strings() ->
    %% Test various string concatenation scenarios
    ?assertEqual("/api/users", nova_router:concat_strings("/api", "/users")),
    ?assertEqual("/users", nova_router:concat_strings("", "/users")),
    ?assertEqual("/api/", nova_router:concat_strings("/api", "/")),

    %% Test with binaries
    ?assertEqual("/api/users", nova_router:concat_strings(<<"/api">>, "/users")),
    ?assertEqual("/api/users", nova_router:concat_strings("/api", <<"/users">>)),
    ?assertEqual("/api/users", nova_router:concat_strings(<<"/api">>, <<"/users">>)),

    %% Test with integer (status code)
    ?assertEqual(404, nova_router:concat_strings("/api", 404)).
