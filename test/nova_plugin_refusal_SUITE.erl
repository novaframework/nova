%%% A plugin that refuses a request, in a supported way or not, ends it: the
%%% controller does not run, and neither the log nor the response carries the
%%% credentials of the request.
-module(nova_plugin_refusal_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([passing_plugin_reaches_the_controller/1,
         error_return_is_a_500/1,
         unsupported_return_is_a_500/1,
         bare_request_return_is_a_500/1,
         unsupported_reply_is_a_500/1,
         crash_is_a_500/1,
         crash_with_the_request_in_the_stacktrace_is_a_500/1,
         break_sends_the_reply_of_the_plugin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(OBSERVER, nova_plugin_refusal_observer).
-define(PLUGIN, nova_plugin_refusal_pages).
-define(BEARER, ~"canary-bearer-6f1d0c").
-define(COOKIE, ~"canary-cookie-9a2e47").

all() ->
    [passing_plugin_reaches_the_controller,
     error_return_is_a_500,
     unsupported_return_is_a_500,
     bare_request_return_is_a_500,
     unsupported_reply_is_a_500,
     crash_is_a_500,
     crash_with_the_request_in_the_stacktrace_is_a_500,
     break_sends_the_reply_of_the_plugin].

init_per_suite(Config) ->
    Routes = [#{prefix => "/" ++ atom_to_list(Action),
                plugins => [{pre_request, ?PLUGIN, #{action => Action}}],
                routes => [{"/page", fun ?PLUGIN:index/1, #{methods => [get]}}]}
              || Action <- [pass, error, unsupported, bare_req, unsupported_reply, crash, no_clause, break]],
    [{port, nova_test_listener:start(#{host => Routes})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

init_per_testcase(_Case, Config) ->
    register(?OBSERVER, self()),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

passing_plugin_reaches_the_controller(Config) ->
    ?assertMatch({{200, ~"index"}, [controller], _Log}, refused(Config, "/pass/page")).

error_return_is_a_500(Config) ->
    ends_without_credentials(500, refused(Config, "/error/page")).

unsupported_return_is_a_500(Config) ->
    ends_without_credentials(500, refused(Config, "/unsupported/page")).

bare_request_return_is_a_500(Config) ->
    ends_without_credentials(500, refused(Config, "/bare_req/page")).

unsupported_reply_is_a_500(Config) ->
    ends_without_credentials(500, refused(Config, "/unsupported_reply/page")).

crash_is_a_500(Config) ->
    ends_without_credentials(500, refused(Config, "/crash/page")).

crash_with_the_request_in_the_stacktrace_is_a_500(Config) ->
    ends_without_credentials(500, refused(Config, "/no_clause/page")).

break_sends_the_reply_of_the_plugin(Config) ->
    ends_without_credentials(403, refused(Config, "/break/page")).

%% The answer to a request carrying credentials, what ran and what was logged.
refused(Config, Path) ->
    Headers = [{~"authorization", [~"Bearer ", ?BEARER]},
               {~"cookie", [~"__Host-session_id=", ?COOKIE]}],
    {Answer, Events} = nova_test_log:collect(
                         fun() -> nova_test_listener:get(?config(port, Config), Path, Headers) end),
    {Answer, observations(), unicode:characters_to_binary(io_lib:format("~0tp", [Events]))}.

%% A 500 is logged, so an empty log would mean nothing was captured.
ends_without_credentials(Status, {{AnswerStatus, Body}, Observed, Log}) ->
    ?assertEqual(Status, AnswerStatus),
    ?assertEqual([], Observed),
    [?assertEqual(nomatch, binary:match(Text, Canary)) || Text <- [Log, Body], Canary <- [?BEARER, ?COOKIE]],
    ?assert(Status =/= 500 orelse binary:match(Log, ~"Plugin") =/= nomatch).

observations() ->
    receive
        {observed, What} -> [What | observations()]
    after 300 ->
        []
    end.
