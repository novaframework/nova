%%% The router resolves dot segments and empty segments before it matches a
%%% route. What decides by path in front of the controller has to decide on
%%% that same path.
-module(nova_canonical_path_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         excluded_path_is_not_checked/1,
         excluded_path_ends_at_a_segment/1,
         dot_segments_do_not_reach_an_exclusion/1,
         canonical_spelling_of_an_excluded_path/1,
         plugin_sees_the_routed_path/1
        ]).

-export([page/1, canonical/1, pre_request/4, plugin_info/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     excluded_path_is_not_checked,
     excluded_path_ends_at_a_segment,
     dot_segments_do_not_reach_an_exclusion,
     canonical_spelling_of_an_excluded_path,
     plugin_sees_the_routed_path
    ].

init_per_suite(Config) ->
    Host = [
            #{plugins => [{pre_request, nova_csrf_plugin, #{excluded_paths => [~"/api"]}}],
              routes => [{"/api/hook", fun ?MODULE:page/1, #{methods => [post]}},
                         {"/apikeys", fun ?MODULE:page/1, #{methods => [post]}},
                         {"/admin/delete", fun ?MODULE:page/1, #{methods => [post]}}]},
            #{prefix => "/seen",
              plugins => [{pre_request, ?MODULE, #{}}],
              routes => [{"/by/plugin", fun ?MODULE:canonical/1, #{methods => [get]}}]}
           ],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

page(_Req) ->
    {status, 200, #{}, ~"page"}.

canonical(#{seen_by_plugin := Path}) ->
    {status, 200, #{}, Path}.

pre_request(Req, _Env, _Options, State) ->
    {ok, Req#{seen_by_plugin => nova_router:canonical_path(Req)}, State}.

plugin_info() ->
    #{title => ~"canonical path plugin",
      version => ~"1.0.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team <info@novaframework.org>"],
      description => ~"Records the canonical path",
      options => []}.

excluded_path_is_not_checked(Config) ->
    ?assertEqual({200, ~"page"}, post(Config, "/api/hook")).

excluded_path_ends_at_a_segment(Config) ->
    ?assertMatch({403, _}, post(Config, "/apikeys")).

dot_segments_do_not_reach_an_exclusion(Config) ->
    ?assertMatch({403, _}, post(Config, "/api/../admin/delete")),
    ?assertMatch({403, _}, post(Config, "/api/hook/../../admin/delete")),
    ?assertMatch({403, _}, post(Config, "/api//../admin/delete")).

canonical_spelling_of_an_excluded_path(Config) ->
    ?assertEqual({200, ~"page"}, post(Config, "//api/./hook")),
    ?assertEqual({200, ~"page"}, post(Config, "/admin/../api/hook")).

plugin_sees_the_routed_path(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"/seen/by/plugin"}, nova_test_listener:get(Port, "/seen/by/plugin")),
    ?assertEqual({200, ~"/seen/by/plugin"}, nova_test_listener:get(Port, "/x/../seen//by/./plugin/?a=b")).

post(Config, Path) ->
    nova_test_listener:request(?config(port, Config), "POST", Path, []).
