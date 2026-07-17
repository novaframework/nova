-module(nova_basic_handler_view_name_tests).
-include_lib("eunit/include/eunit.hrl").

%% Regression: get_view_name crashed with function_clause
%% when module name did not end with _controller
get_view_name_no_controller_suffix_test() ->
    ?assertEqual(my_module_dtl, nova_basic_handler:get_view_name(my_module)).

get_view_name_with_controller_suffix_test() ->
    ?assertEqual(my_app_dtl, nova_basic_handler:get_view_name(my_app_controller)).

get_view_name_tuple_test() ->
    ?assertEqual(foo_dtl, nova_basic_handler:get_view_name({foo_controller, #{}})).

get_view_name_tuple_no_suffix_test() ->
    ?assertEqual(foo_dtl, nova_basic_handler:get_view_name({foo, #{}})).
