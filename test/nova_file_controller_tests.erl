-module(nova_file_controller_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ASSETS, filename:join(filename:dirname(?FILE), "nova_test_app/priv/assets")).

wildcard(Pathinfo) ->
    #{path => ~"/assets",
      headers => #{},
      extra_state => #{pathinfo => Pathinfo, static => {dir, ?ASSETS}, options => #{}}}.

serves_a_file_inside_the_root_test() ->
    ?assertMatch({sendfile, 200, _, {0, _, _}, ~"text/plain"},
                 nova_file_controller:get_dir(wildcard([~"hello.txt"]))).

serves_a_nested_file_test() ->
    ?assertMatch({sendfile, 200, _, _, _},
                 nova_file_controller:get_dir(wildcard([~"nested", ~"deep.txt"]))).

dotdot_segment_is_refused_test() ->
    ?assertEqual({status, 404},
                 nova_file_controller:get_dir(wildcard([~"..", ~"outside_assets.txt"]))).

dotdot_after_a_directory_is_refused_test() ->
    ?assertEqual({status, 404},
                 nova_file_controller:get_dir(wildcard([~"nested", ~"..", ~"..", ~"outside_assets.txt"]))).

segment_with_a_slash_is_refused_test() ->
    ?assertEqual({status, 404},
                 nova_file_controller:get_dir(wildcard([~"../outside_assets.txt"]))),
    ?assertEqual({status, 404},
                 nova_file_controller:get_dir(wildcard([~"/etc/passwd"]))).

segment_with_a_nul_is_refused_test() ->
    ?assertEqual({status, 404},
                 nova_file_controller:get_dir(wildcard([<<"hello.txt", 0>>]))).
