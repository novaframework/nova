-module(nova_controller_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

controller_test_() ->
    [
     {"Routes with module only", fun test_routes_module_only/0},
     {"Routes with module and options", fun test_routes_module_with_opts/0},
     {"Routes from non-existent callback", fun test_routes_no_callback/0},
     {"Routes with module tuple", fun test_routes_module_tuple/0}
    ].

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_routes_module_only() ->
    %% Create a mock controller module
    meck:new(test_routes_controller, [non_strict]),
    meck:expect(test_routes_controller, routes, fun(_Env, _Opts) ->
                                                        [#{path => "/test"}]
                                                end),

    %% Call routes/2
    Routes = nova_controller:routes(test_routes_controller, dev),

    %% Should return the routes
    ?assertEqual([#{path => "/test"}], Routes),

    meck:unload(test_routes_controller).

test_routes_module_with_opts() ->
    %% Create a mock controller module
    meck:new(test_opts_controller, [non_strict]),
    meck:expect(test_opts_controller, routes, fun(dev, #{prefix := "/api"}) ->
                                                      [#{path => "/users"}];
                                                 (_Env, _Opts) ->
                                                      []
                                              end),

    %% Call routes/2 with module tuple
    Routes = nova_controller:routes({test_opts_controller, #{prefix => "/api"}}, dev),

    %% Should pass options to routes callback
    ?assertEqual([#{path => "/users"}], Routes),

    meck:unload(test_opts_controller).

test_routes_no_callback() ->
    %% Create a module without routes/2 callback
    meck:new(no_routes_controller, [non_strict]),
    %% Don't expect the routes function - it should handle this gracefully

    %% Call routes/2
    Routes = nova_controller:routes(no_routes_controller, dev),

    %% Should return empty list
    ?assertEqual([], Routes),

    meck:unload(no_routes_controller).

test_routes_module_tuple() ->
    %% Test with {Module, Opts} tuple
    meck:new(tuple_controller, [non_strict]),
    meck:expect(tuple_controller, routes, fun(_Env, Opts) ->
                                                  [#{opts => Opts}]
                                          end),

    %% Call with tuple
    Routes = nova_controller:routes({tuple_controller, #{key => value}}, prod),

    %% Should pass opts correctly
    ?assertEqual([#{opts => #{key => value}}], Routes),

    meck:unload(tuple_controller).
