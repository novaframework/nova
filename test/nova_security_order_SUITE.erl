%%% What runs ahead of the security callback of a route and what waits for it.
-module(nova_security_order_SUITE).

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).

-export([body_is_not_read_for_a_refused_request/1,
         body_is_decoded_for_an_allowed_request/1,
         upload_is_not_read_for_a_refused_request/1,
         refused_request_has_its_correlation_id_and_cors_headers/1,
         preflight_is_answered_ahead_of_security/1,
         preflight_nothing_answers_runs_pre_security_alone/1,
         options_that_is_no_preflight_runs_no_plugin/1,
         preflight_to_a_route_that_takes_options_meets_its_security/1,
         query_string_is_parsed_ahead_of_security/1,
         pre_request_of_a_plugin_waits_for_security/1,
         pre_security_of_a_plugin_runs_ahead_of_security/1,
         plugin_listed_before_security_runs_ahead_of_it/1,
         every_plugin_runs_ahead_of_security_with_all/1,
         plugin_listed_after_security_runs_nothing_ahead_of_it/1,
         plugin_listed_both_ways_runs_after_security/1,
         plugin_after_security_can_stop_the_request/1,
         plugins_after_security_are_skipped_once_an_earlier_one_failed/1,
         plugin_that_fails_after_security_ends_the_request/1,
         list_from_the_environment_applies/1,
         route_group_list_wins_over_the_environment/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(JSON, {"content-type", "application/json"}).
-define(MULTIPART, {"content-type", "multipart/form-data; boundary=nova"}).
-define(ALLOWED, {"authorization", "letmein"}).

all() ->
    [{group, route_group_option}, {group, environment_option}].

groups() ->
    [{route_group_option, [], [body_is_not_read_for_a_refused_request,
                               body_is_decoded_for_an_allowed_request,
                               upload_is_not_read_for_a_refused_request,
                               refused_request_has_its_correlation_id_and_cors_headers,
                               preflight_is_answered_ahead_of_security,
                               preflight_nothing_answers_runs_pre_security_alone,
                               options_that_is_no_preflight_runs_no_plugin,
                               preflight_to_a_route_that_takes_options_meets_its_security,
                               query_string_is_parsed_ahead_of_security,
                               pre_request_of_a_plugin_waits_for_security,
                               pre_security_of_a_plugin_runs_ahead_of_security,
                               plugin_listed_before_security_runs_ahead_of_it,
                               every_plugin_runs_ahead_of_security_with_all,
                               plugin_listed_after_security_runs_nothing_ahead_of_it,
                               plugin_listed_both_ways_runs_after_security,
                               plugin_after_security_can_stop_the_request,
                               plugins_after_security_are_skipped_once_an_earlier_one_failed,
                               plugin_that_fails_after_security_ends_the_request]},
     {environment_option, [], [list_from_the_environment_applies,
                               route_group_list_wins_over_the_environment]}].

init_per_group(route_group_option, Config) ->
    Multipart = {pre_request, nova_multipart_plugin, #{handler => {nova_multipart_memory_handler, #{}}}},
    Own = fun(Options) -> {pre_request, nova_security_order_pages, Options} end,
    Routes = [group("/default", #{plugins => plugins() ++ [Own(#{early => mark, action => mark})]}),
              group("/upload", #{plugins => [Multipart]}),
              group("/before", #{plugins => plugins() ++ [Own(#{action => mark})],
                                 plugins_before_security => [nova_request_plugin]}),
              group("/all", #{plugins => plugins() ++ [Own(#{action => mark})],
                              plugins_before_security => all}),
              group("/all-without-cors", #{plugins => [request(), Own(#{early => mark, action => mark})],
                                           plugins_before_security => all}),
              (group("/takes-options", #{plugins => [Own(#{early => mark})]}))
              #{routes => [{"/echo", fun nova_security_order_pages:echo/1, #{methods => [post, options]}}]},
              group("/after", #{plugins => plugins() ++ [Own(#{early => mark})],
                                plugins_after_security => [nova_correlation_plugin, nova_request_plugin,
                                                           nova_security_order_pages]}),
              group("/both", #{plugins => [request()],
                               plugins_before_security => [nova_request_plugin],
                               plugins_after_security => [nova_request_plugin]}),
              group("/stopping", #{plugins => [Own(#{action => stop})]}),
              group("/failing", #{plugins => [Own(#{early => crash}), request()]}),
              group("/failing-late", #{plugins => [Own(#{action => crash}), request()]})],
    [{port, nova_test_listener:start(#{host => Routes})} | Config];
init_per_group(environment_option, Config) ->
    Routes = [group("/inherits", #{plugins => [request()]}),
              group("/overrides", #{plugins => [request()], plugins_before_security => []})],
    application:load(nova),
    application:set_env(nova, plugins_before_security, [nova_request_plugin]),
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_group(_Group, _Config) ->
    application:unset_env(nova, plugins_before_security),
    nova_test_listener:stop().

%% The plugins a typical application lists, in the order it lists them.
plugins() ->
    [request(),
     {pre_request, nova_correlation_plugin, #{}},
     {pre_request, nova_cors_plugin, #{allow_origins => ~"https://app.example"}}].

request() ->
    {pre_request, nova_request_plugin, #{decode_json_body => true, parse_qs => true}}.

group(Prefix, Options) ->
    Options#{prefix => Prefix,
             security => fun nova_security_order_pages:by_header/1,
             routes => [{"/echo", fun nova_security_order_pages:echo/1, #{methods => [post]}}]}.

body_is_not_read_for_a_refused_request(Config) ->
    ?assertMatch({401, #{~"body" := false}}, refused(Config, "/default/echo", [?JSON], ~"{\"a\":1}")),
    %% The malformed body would be a 400 if it was read.
    ?assertMatch({401, _}, refused(Config, "/default/echo", [?JSON], ~"{")).

body_is_decoded_for_an_allowed_request(Config) ->
    {200, _Headers, Body} = post(Config, "/default/echo?b=2", [?JSON, ?ALLOWED], ~"{\"a\":1}"),
    ?assertMatch(#{~"security_saw" := #{~"body" := false},
                   ~"controller_json" := #{~"a" := 1},
                   ~"controller_qs" := #{~"b" := ~"2"}}, json:decode(Body)),
    ?assertMatch({400, _, _}, post(Config, "/default/echo", [?JSON, ?ALLOWED], ~"{")).

%% A body that is not multipart at all is a 400 from the plugin that reads it.
upload_is_not_read_for_a_refused_request(Config) ->
    ?assertMatch({401, _, _}, post(Config, "/upload/echo", [?MULTIPART], ~"not a multipart body")),
    ?assertMatch({400, _, _}, post(Config, "/upload/echo", [?MULTIPART, ?ALLOWED], ~"not a multipart body")).

refused_request_has_its_correlation_id_and_cors_headers(Config) ->
    {401, Headers, _Body} = post(Config, "/default/echo", [{"origin", "https://app.example"}], <<>>),
    ?assert(is_map_key(~"x-correlation-id", Headers)),
    ?assertEqual(~"https://app.example", maps:get(~"access-control-allow-origin", Headers)).

%% A preflight carries no credentials, so the security callback would refuse
%% it and the browser would never send the request it asks about.
preflight_is_answered_ahead_of_security(Config) ->
    {Status, Headers, _Body} =
        nova_test_listener:response(?config(port, Config), "OPTIONS", "/default/echo",
                                    [{"origin", "https://app.example"},
                                     {"access-control-request-method", "POST"},
                                     {"access-control-request-headers", "authorization"}], <<>>),
    ?assertEqual(204, Status),
    ?assertEqual(~"https://app.example", maps:get(~"access-control-allow-origin", Headers)),
    ?assertEqual(~"authorization", maps:get(~"access-control-allow-headers", Headers)).

%% The route does not take OPTIONS, so there is no security callback to wait
%% for. What a route group lists to run ahead of its callback is not
%% something to run where there is none: the body would be read for anybody.
preflight_nothing_answers_runs_pre_security_alone(Config) ->
    {Status, Headers, _Body} = preflight(Config, "/all-without-cors/echo", ~"{"),
    ?assertEqual(405, Status),
    ?assertEqual(~"ran", maps:get(~"x-early", Headers)),
    ?assertNot(is_map_key(~"x-late", Headers)),
    ?assertMatch({401, #{~"body" := true, ~"late" := true}},
                 refused(Config, "/all-without-cors/echo", [?JSON], ~"{\"a\":1}")).

options_that_is_no_preflight_runs_no_plugin(Config) ->
    [begin
         {Status, Headers, _Body} =
             nova_test_listener:response(?config(port, Config), "OPTIONS", "/all-without-cors/echo", Sent, <<>>),
         ?assertEqual({Sent, 405}, {Sent, Status}),
         ?assertNot(is_map_key(~"x-early", Headers)),
         ?assertNot(is_map_key(~"x-late", Headers))
     end || Sent <- [[], [{"origin", "https://app.example"}], [{"access-control-request-method", "POST"}]]].

preflight_to_a_route_that_takes_options_meets_its_security(Config) ->
    {Status, Headers, _Body} = preflight(Config, "/takes-options/echo", <<>>),
    ?assertEqual(401, Status),
    ?assert(is_map_key(~"x-security-saw", Headers)).

preflight(Config, Path, Body) ->
    nova_test_listener:response(?config(port, Config), "OPTIONS", Path,
                                [{"origin", "https://app.example"}, {"access-control-request-method", "POST"}, ?JSON],
                                Body).

%% Parsing it reads nothing from the client, and a websocket or a link can
%% only carry its token there.
query_string_is_parsed_ahead_of_security(Config) ->
    ?assertMatch({401, #{~"qs" := true, ~"body" := false}}, refused(Config, "/default/echo?token=t", [?JSON], ~"{}")),
    ?assertMatch({400, _, _}, post(Config, "/default/echo?a=%zz", [], <<>>)).

pre_request_of_a_plugin_waits_for_security(Config) ->
    {401, Refused, _} = post(Config, "/default/echo", [], <<>>),
    ?assertNot(is_map_key(~"x-late", Refused)),
    {200, Allowed, Body} = post(Config, "/default/echo", [?ALLOWED], <<>>),
    ?assertEqual(~"ran", maps:get(~"x-late", Allowed)),
    ?assertMatch(#{~"security_saw" := #{~"late" := false}}, json:decode(Body)).

pre_security_of_a_plugin_runs_ahead_of_security(Config) ->
    {401, Headers, _} = post(Config, "/default/echo", [], <<>>),
    ?assertEqual(~"ran", maps:get(~"x-early", Headers)),
    ?assertMatch({401, #{~"early" := true}}, refused(Config, "/default/echo", [], <<>>)).

%% The order of earlier releases, for the plugins a route group names.
plugin_listed_before_security_runs_ahead_of_it(Config) ->
    ?assertMatch({401, #{~"body" := true, ~"late" := false}}, refused(Config, "/before/echo", [?JSON], ~"{\"a\":1}")),
    ?assertMatch({400, _, _}, post(Config, "/before/echo", [?JSON], ~"{")).

every_plugin_runs_ahead_of_security_with_all(Config) ->
    ?assertMatch({401, #{~"body" := true, ~"late" := true}}, refused(Config, "/all/echo", [?JSON], ~"{\"a\":1}")).

plugin_listed_after_security_runs_nothing_ahead_of_it(Config) ->
    {401, Headers, _} = post(Config, "/after/echo?token=t", [{"origin", "https://app.example"}], <<>>),
    ?assertNot(is_map_key(~"x-correlation-id", Headers)),
    ?assertNot(is_map_key(~"x-early", Headers)),
    ?assertEqual(~"https://app.example", maps:get(~"access-control-allow-origin", Headers)),
    ?assertMatch({401, #{~"qs" := false, ~"early" := false}}, refused(Config, "/after/echo?token=t", [], <<>>)),
    {200, Allowed, Body} = post(Config, "/after/echo?b=2", [?ALLOWED], <<>>),
    ?assert(is_map_key(~"x-correlation-id", Allowed)),
    ?assertEqual(~"ran", maps:get(~"x-early", Allowed)),
    ?assertMatch(#{~"controller_qs" := #{~"b" := ~"2"}}, json:decode(Body)).

plugin_listed_both_ways_runs_after_security(Config) ->
    ?assertMatch({401, #{~"body" := false, ~"qs" := false}}, refused(Config, "/both/echo?a=1", [?JSON], ~"{\"a\":1}")).

plugin_after_security_can_stop_the_request(Config) ->
    ?assertMatch({401, _, _}, post(Config, "/stopping/echo", [], <<>>)),
    ?assertMatch({403, _, _}, post(Config, "/stopping/echo", [?ALLOWED], <<>>)).

%% The malformed body would be a 400 if the plugin reading it still ran.
plugins_after_security_are_skipped_once_an_earlier_one_failed(Config) ->
    ?assertMatch({500, _, _}, post(Config, "/failing/echo", [?JSON, ?ALLOWED], ~"{")).

plugin_that_fails_after_security_ends_the_request(Config) ->
    ?assertMatch({401, _, _}, post(Config, "/failing-late/echo", [?JSON], ~"{")),
    {500, _Headers, Body} = post(Config, "/failing-late/echo", [?JSON, ?ALLOWED], ~"{}"),
    ?assertEqual(nomatch, binary:match(Body, ~"controller_json")).

list_from_the_environment_applies(Config) ->
    ?assertMatch({401, #{~"body" := true}}, refused(Config, "/inherits/echo", [?JSON], ~"{\"a\":1}")).

route_group_list_wins_over_the_environment(Config) ->
    ?assertMatch({401, #{~"body" := false}}, refused(Config, "/overrides/echo", [?JSON], ~"{\"a\":1}")).

%% The status of a request without credentials and what the security
%% callback found in it.
refused(Config, Path, Headers, Body) ->
    case post(Config, Path, Headers, Body) of
        {Status, #{~"x-security-saw" := Saw}, _Body} -> {Status, json:decode(Saw)};
        {Status, _Headers, _Body} -> {Status, undefined}
    end.

post(Config, Path, Headers, Body) ->
    nova_test_listener:response(?config(port, Config), "POST", Path, Headers, Body).
