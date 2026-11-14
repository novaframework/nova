%%% Whether a body-reading plugin runs before or after the security callback.
-module(nova_security_order_SUITE).

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).

-export([body_is_read_before_security_by_default/1,
         listed_plugin_runs_after_security/1,
         listed_plugin_runs_for_an_allowed_request/1,
         unlisted_plugin_still_runs_before_security/1,
         refused_upload_is_not_read/1,
         upload_is_read_before_security_by_default/1,
         listed_plugin_can_stop_the_request/1,
         listed_plugin_is_skipped_after_an_earlier_plugin_failed/1,
         list_from_the_environment_applies/1,
         route_group_list_wins_over_the_environment/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(JSON, {"content-type", "application/json"}).
-define(MULTIPART, {"content-type", "multipart/form-data; boundary=nova"}).

all() ->
    [{group, route_group_option}, {group, environment_option}].

groups() ->
    [{route_group_option, [], [body_is_read_before_security_by_default,
                               listed_plugin_runs_after_security,
                               listed_plugin_runs_for_an_allowed_request,
                               unlisted_plugin_still_runs_before_security,
                               refused_upload_is_not_read,
                               upload_is_read_before_security_by_default,
                               listed_plugin_can_stop_the_request,
                               listed_plugin_is_skipped_after_an_earlier_plugin_failed]},
     {environment_option, [], [list_from_the_environment_applies,
                               route_group_list_wins_over_the_environment]}].

init_per_group(route_group_option, Config) ->
    Json = {pre_request, nova_request_plugin, #{decode_json_body => true}},
    Multipart = {pre_request, nova_multipart_plugin, #{handler => {nova_multipart_memory_handler, #{}}}},
    Routes = [group("/default", #{plugins => [Json]}),
              group("/ordered", #{plugins => [{pre_request, nova_correlation_plugin, #{}}, Json],
                                  plugins_after_security => [nova_request_plugin]}),
              group("/default-upload", #{plugins => [Multipart]}),
              group("/ordered-upload", #{plugins => [Multipart],
                                         plugins_after_security => [nova_multipart_plugin]}),
              group("/stopping", #{plugins => [{pre_request, nova_security_order_pages, #{action => stop}}],
                                   plugins_after_security => [nova_security_order_pages]}),
              group("/failing", #{plugins => [{pre_request, nova_security_order_pages, #{action => crash}}, Json],
                                  plugins_after_security => [nova_request_plugin]})],
    [{port, nova_test_listener:start(#{host => Routes})} | Config];
init_per_group(environment_option, Config) ->
    Json = {pre_request, nova_request_plugin, #{decode_json_body => true}},
    Routes = [group("/inherits", #{plugins => [Json]}),
              group("/overrides", #{plugins => [Json], plugins_after_security => []})],
    application:load(nova),
    application:set_env(nova, plugins_after_security, [nova_request_plugin]),
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_group(_Group, _Config) ->
    application:unset_env(nova, plugins_after_security),
    nova_test_listener:stop().

group(Prefix, Options) ->
    Options#{prefix => Prefix,
             security => fun nova_security_order_pages:by_header/1,
             routes => [{"/echo", fun nova_security_order_pages:echo/1, #{methods => [post]}}]}.

body_is_read_before_security_by_default(Config) ->
    {401, Headers, _Body} = post(Config, "/default/echo", [?JSON], ~"{\"a\":1}"),
    ?assertEqual(~"true", maps:get(~"x-security-saw-body", Headers)).

listed_plugin_runs_after_security(Config) ->
    {401, Headers, _Body} = post(Config, "/ordered/echo", [?JSON], ~"{\"a\":1}"),
    ?assertEqual(~"false", maps:get(~"x-security-saw-body", Headers)).

listed_plugin_runs_for_an_allowed_request(Config) ->
    {200, _Headers, Body} = post(Config, "/ordered/echo", [?JSON, {"authorization", "letmein"}], ~"{\"a\":1}"),
    ?assertEqual(#{~"security_saw_body" => false, ~"controller_json" => #{~"a" => 1}}, json:decode(Body)).

unlisted_plugin_still_runs_before_security(Config) ->
    {401, Headers, _Body} = post(Config, "/ordered/echo", [?JSON], ~"{\"a\":1}"),
    ?assert(is_map_key(~"x-correlation-id", Headers)).

%% A body that is not multipart at all is a 400 from the plugin that reads it.
refused_upload_is_not_read(Config) ->
    ?assertMatch({401, _, _}, post(Config, "/ordered-upload/echo", [?MULTIPART], ~"not a multipart body")).

upload_is_read_before_security_by_default(Config) ->
    ?assertMatch({400, _, _}, post(Config, "/default-upload/echo", [?MULTIPART], ~"not a multipart body")).

listed_plugin_can_stop_the_request(Config) ->
    ?assertMatch({401, _, _}, post(Config, "/stopping/echo", [], <<>>)),
    ?assertMatch({403, _, _}, post(Config, "/stopping/echo", [{"authorization", "letmein"}], <<>>)).

%% The malformed body would be a 400 if the plugin reading it still ran.
listed_plugin_is_skipped_after_an_earlier_plugin_failed(Config) ->
    ?assertMatch({500, _, _}, post(Config, "/failing/echo", [?JSON, {"authorization", "letmein"}], ~"{")).

list_from_the_environment_applies(Config) ->
    {401, Headers, _Body} = post(Config, "/inherits/echo", [?JSON], ~"{\"a\":1}"),
    ?assertEqual(~"false", maps:get(~"x-security-saw-body", Headers)).

route_group_list_wins_over_the_environment(Config) ->
    {401, Headers, _Body} = post(Config, "/overrides/echo", [?JSON], ~"{\"a\":1}"),
    ?assertEqual(~"true", maps:get(~"x-security-saw-body", Headers)).

post(Config, Path, Headers, Body) ->
    nova_test_listener:response(?config(port, Config), "POST", Path, Headers, Body).
