%%% A request path that is not in its canonical form is answered 400 before
%%% a route is looked up. With `non_canonical_paths` set to `resolve` the
%%% router resolves dot segments and empty segments before it matches a
%%% route, and what decides by path in front of the controller has to decide
%%% on that same path.
-module(nova_canonical_path_SUITE).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).

-export([
         excluded_path_is_not_checked/1,
         dot_segments_are_refused/1,
         empty_segments_are_refused/1,
         encoded_dot_segments_are_refused/1,
         one_trailing_slash_is_routed/1,
         encoded_slash_stays_in_its_segment/1,
         refused_before_plugins_and_security/1,
         refusal_is_a_status_page/1,
         path_without_a_leading_slash_is_refused/1,
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
                    dot_segments_are_refused,
                    empty_segments_are_refused,
                    encoded_dot_segments_are_refused,
                    one_trailing_slash_is_routed,
                    encoded_slash_stays_in_its_segment,
                    refused_before_plugins_and_security,
                    refusal_is_a_status_page,
                    path_without_a_leading_slash_is_refused
                   ]},
     {resolved, [], [
                     excluded_path_is_not_checked,
                     plugin_sees_the_routed_path,
                     path_without_a_leading_slash_is_refused
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
    nova_test_settings:set(non_canonical_paths, resolve),
    Config;
init_per_group(refused, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    nova_test_settings:unset(non_canonical_paths).

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

%% HTTP/2 lets a client send one. It is no path to resolve: a plugin that
%% looks at how the path starts would be told nothing by it.
path_without_a_leading_slash_is_refused(Config) ->
    Port = ?config(port, Config),
    true = register(nova_canonical_path_pages, self()),
    try
        [?assertEqual({Path, 400}, {Path, nova_test_listener:h2_get(Port, Path)})
         || Path <- [~"guarded/page", ~"seen/by/plugin", ~"x/../guarded/page", ~"*/guarded/page", ~"**"]],
        ?assertEqual([], seen()),
        ?assertEqual(200, nova_test_listener:h2_get(Port, ~"/guarded/page")),
        ?assertEqual([plugin, security], seen())
    after
        unregister(nova_canonical_path_pages)
    end.

seen() ->
    receive
        {seen, What} -> [What | seen()]
    after 100 ->
        []
    end.

plugin_sees_the_routed_path(Config) ->
    Port = ?config(port, Config),
    ?assertEqual({200, ~"/seen/by/plugin"}, nova_test_listener:get(Port, "/seen/by/plugin")),
    ?assertEqual({200, ~"/seen/by/plugin"}, nova_test_listener:get(Port, "/x/../seen//by/./plugin/?a=b")).

post(Config, Path) ->
    nova_test_listener:request(?config(port, Config), "POST", Path, []).
