%%% A request path that is not in its canonical form is answered 400 before
%%% a route is looked up. With `non_canonical_paths` set to `resolve` the
%%% router resolves dot segments and empty segments before it matches a
%%% route, and what decides by path in front of the controller has to decide
%%% on that same path.
-module(nova_canonical_path_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).

-export([
         excluded_path_is_not_checked/1,
         excluded_path_ends_at_a_segment/1,
         dot_segments_are_refused/1,
         empty_segments_are_refused/1,
         encoded_dot_segments_are_refused/1,
         one_trailing_slash_is_routed/1,
         encoded_slash_stays_in_its_segment/1,
         refused_before_plugins_and_security/1,
         refusal_is_a_status_page/1,
         dot_segments_do_not_reach_an_exclusion/1,
         canonical_spelling_of_an_excluded_path/1,
         plugin_sees_the_routed_path/1
        ]).

-export([pre_security/4, pre_request/4, plugin_info/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [{group, refused}, {group, resolved}].

groups() ->
    [
     {refused, [], [
                    excluded_path_is_not_checked,
                    excluded_path_ends_at_a_segment,
                    dot_segments_are_refused,
                    empty_segments_are_refused,
                    encoded_dot_segments_are_refused,
                    one_trailing_slash_is_routed,
                    encoded_slash_stays_in_its_segment,
                    refused_before_plugins_and_security,
                    refusal_is_a_status_page
                   ]},
     {resolved, [], [
                     excluded_path_is_not_checked,
                     excluded_path_ends_at_a_segment,
                     dot_segments_do_not_reach_an_exclusion,
                     canonical_spelling_of_an_excluded_path,
                     plugin_sees_the_routed_path
                    ]}
    ].

init_per_suite(Config) ->
    Host = [
            #{plugins => [{pre_request, nova_csrf_plugin, #{excluded_paths => [~"/api"]}}],
              routes => [{"/api/hook", fun nova_canonical_path_pages:page/1, #{methods => [post]}},
                         {"/apikeys", fun nova_canonical_path_pages:page/1, #{methods => [post]}},
                         {"/admin/delete", fun nova_canonical_path_pages:page/1, #{methods => [post]}}]},
            #{prefix => "/seen",
              plugins => [{pre_request, ?MODULE, #{}}],
              routes => [{"/by/plugin", fun nova_canonical_path_pages:canonical/1, #{methods => [get]}},
                         {"/item/:id", fun nova_canonical_path_pages:binding/1, #{methods => [get]}}]},
            #{prefix => "/guarded",
              security => fun nova_canonical_path_pages:secure/1,
              plugins => [{pre_request, ?MODULE, #{}}],
              routes => [{"/page", fun nova_canonical_path_pages:page/1, #{methods => [get]}}]},
            #{routes => [{400, fun nova_canonical_path_pages:bad_path/1, #{}}]}
           ],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_group(resolved, Config) ->
    application:set_env(nova, non_canonical_paths, resolve),
    Config;
init_per_group(refused, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    application:unset_env(nova, non_canonical_paths).

pre_security(Req, _Env, _Options, State) ->
    nova_canonical_path_pages:seen(plugin),
    {ok, Req, State}.

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

dot_segments_are_refused(Config) ->
    [?assertMatch({400, _}, post(Config, Path))
     || Path <- ["/api/../admin/delete", "/api/hook/../../admin/delete", "/admin/../api/hook",
                 "/api/./hook", "/./api/hook", "/api/hook/.", "/api/hook/..", "/.."]].

empty_segments_are_refused(Config) ->
    [?assertMatch({400, _}, post(Config, Path))
     || Path <- ["//api/hook", "/api//hook", "/api/hook//", "//"]].

encoded_dot_segments_are_refused(Config) ->
    [?assertMatch({400, _}, post(Config, Path))
     || Path <- ["/api/%2e%2e/admin/delete", "/api/%2E%2E/admin/delete", "/api/.%2e/admin/delete",
                 "/api/%2e./admin/delete", "/api/%2e/hook"]].

one_trailing_slash_is_routed(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"page"}, post(Config, "/api/hook/")),
    ?assertEqual({200, ~"/seen/by/plugin"}, nova_test_listener:get(Port, "/seen/by/plugin/?a=b")).

encoded_slash_stays_in_its_segment(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"a%2Fb"}, nova_test_listener:get(Port, "/seen/item/a%2Fb")),
    ?assertEqual({200, ~"..."}, nova_test_listener:get(Port, "/seen/item/...")),
    ?assertEqual({200, ~".well-known"}, nova_test_listener:get(Port, "/seen/item/.well-known")).

refused_before_plugins_and_security(Config) ->
    Port = ?config(port, Config),
    true = register(nova_canonical_path_pages, self()),
    try
        ?assertMatch({400, _}, nova_test_listener:get(Port, "/guarded/../guarded/page")),
        ?assertMatch({400, _}, nova_test_listener:get(Port, "/guarded//page")),
        ?assertEqual([], seen()),
        ?assertEqual({200, ~"page"}, nova_test_listener:get(Port, "/guarded/page")),
        ?assertEqual([plugin, security], seen())
    after
        unregister(nova_canonical_path_pages)
    end.

refusal_is_a_status_page(Config) ->
    ?assertEqual({400, ~"bad path"}, post(Config, "/api/../admin/delete")).

seen() ->
    receive
        {seen, What} -> [What | seen()]
    after 100 ->
        []
    end.

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
