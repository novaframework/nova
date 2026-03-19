-module(nova_controller_tests).
-include_lib("eunit/include/eunit.hrl").

routes_with_module_that_exports_routes_test() ->
    meck:new(test_ctrl, [non_strict, no_link]),
    meck:expect(test_ctrl, routes, fun(dev, _Opts) ->
        [#{routes => [{"/test", fun(_) -> ok end, #{}}]}]
    end),
    Result = nova_controller:routes(test_ctrl, dev),
    ?assertMatch([#{routes := _}], Result),
    meck:unload(test_ctrl).

routes_with_module_no_routes_callback_test() ->
    meck:new(test_ctrl2, [non_strict, no_link]),
    %% Don't define routes/2 — should return []
    Result = nova_controller:routes(test_ctrl2, dev),
    ?assertEqual([], Result),
    meck:unload(test_ctrl2).

routes_with_tuple_test() ->
    meck:new(test_ctrl3, [non_strict, no_link]),
    meck:expect(test_ctrl3, routes, fun(dev, Opts) ->
        val = maps:get(opt, Opts),
        [#{routes => [{"/x", fun(_) -> ok end, #{}}]}]
    end),
    Result = nova_controller:routes({test_ctrl3, #{opt => val}}, dev),
    ?assertMatch([#{routes := _}], Result),
    meck:unload(test_ctrl3).

routes_with_nonexistent_module_test() ->
    Result = nova_controller:routes(totally_fake_module_xyz_123, dev),
    ?assertEqual([], Result).
