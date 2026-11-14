%%% A controller that answers the request with Cowboy and then returns as if
%%% it had not is answered once: what it returned is not sent, the connection
%%% goes on to serve the next request, and one warning says which controller
%%% returned what.
-module(nova_own_reply_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         ok_return_after_an_own_reply/1,
         view_return_after_an_own_reply/1,
         json_return_after_an_own_reply/1,
         status_return_after_an_own_reply/1,
         redirect_return_after_an_own_reply/1,
         fallback_controller_after_an_own_reply/1,
         fallback_controller_that_replies_itself/1,
         crash_after_an_own_reply/1,
         thrown_status_after_an_own_reply/1,
         post_request_reply_after_an_own_reply/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PAGES, nova_own_reply_pages).

all() ->
    [
     ok_return_after_an_own_reply,
     view_return_after_an_own_reply,
     json_return_after_an_own_reply,
     status_return_after_an_own_reply,
     redirect_return_after_an_own_reply,
     fallback_controller_after_an_own_reply,
     fallback_controller_that_replies_itself,
     crash_after_an_own_reply,
     thrown_status_after_an_own_reply,
     post_request_reply_after_an_own_reply
    ].

init_per_suite(Config) ->
    Host = [#{prefix => "",
              plugins => [],
              routes => [
                         {"/ok", fun ?PAGES:ok/1, #{methods => [get]}},
                         {"/view", fun ?PAGES:view/1, #{methods => [get]}},
                         {"/json", fun ?PAGES:json/1, #{methods => [get]}},
                         {"/status", fun ?PAGES:status/1, #{methods => [get]}},
                         {"/redirect", fun ?PAGES:redirect/1, #{methods => [get]}},
                         {"/unsupported", fun ?PAGES:unsupported/1, #{methods => [get]}},
                         {"/to-fallback", fun ?PAGES:to_fallback/1, #{methods => [get]}},
                         {"/crash", fun ?PAGES:crash/1, #{methods => [get]}},
                         {"/thrown", fun ?PAGES:thrown/1, #{methods => [get]}},
                         {"/plain", fun ?PAGES:plain/1, #{methods => [get]}},
                         {404, fun ?PAGES:not_found/1, #{}}
                        ]},
            #{prefix => "/plugin",
              plugins => [{post_request, ?PAGES, #{}}],
              routes => [{"/page", fun ?PAGES:before_plugin/1, #{methods => [get]}}]}],
    [{port, nova_test_listener:start(#{host => Host})} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

ok_return_after_an_own_reply(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := ok, return := ok}], answered_once(Config, "/ok")).

view_return_after_an_own_reply(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := view, return := view}], answered_once(Config, "/view")).

json_return_after_an_own_reply(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := json, return := json}], answered_once(Config, "/json")).

status_return_after_an_own_reply(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := status, return := status}],
                 answered_once(Config, "/status")).

redirect_return_after_an_own_reply(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := redirect, return := redirect}],
                 answered_once(Config, "/redirect")).

%% The controller answered and returned something only its fallback
%% controller understands.
fallback_controller_after_an_own_reply(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := resolve, return := status}],
                 answered_once(Config, "/unsupported")).

fallback_controller_that_replies_itself(Config) ->
    ?assertMatch([#{controller := ?PAGES, function := resolve, return := status}],
                 answered_once(Config, "/to-fallback")).

%% The 500 page is not sent after the answer of the controller.
crash_after_an_own_reply(Config) ->
    ?assertMatch([#{msg := ~"The response had begun when the controller failed, so no error page is sent"}],
                 answered_once(Config, "/crash")).

%% Nor is the 404 page the controller asks for by throwing.
thrown_status_after_an_own_reply(Config) ->
    ?assertMatch([#{msg := ~"The response had begun when the controller failed, so no error page is sent"}],
                 answered_once(Config, "/thrown")).

%% The plugin hands back a request that does not say it was answered.
post_request_reply_after_an_own_reply(Config) ->
    [ControllerReturn, PluginReply] = answered_once(Config, "/plugin/page"),
    ?assertMatch(#{controller := ?PAGES, function := before_plugin, return := status}, ControllerReturn),
    ?assertMatch(#{plugin := ?PAGES}, PluginReply).

%% Asks for Path and then for /plain on one connection, which is only served
%% when the first request left the connection process alive. Returns the
%% reports logged at warning level.
answered_once(Config, Path) ->
    {Received, Events} = nova_test_log:collect(fun() -> two_requests(?config(port, Config), Path) end),
    ?assertEqual(2, length(binary:matches(Received, ~"HTTP/1.1 "))),
    ?assertEqual(2, length(binary:matches(Received, ~"HTTP/1.1 200"))),
    ?assertEqual(nomatch, binary:match(Received, ~"from the")),
    [Report || {warning, {report, Report}} <- Events].

two_requests(Port, Path) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}]),
    try
        ok = gen_tcp:send(Socket, ["GET ", Path, " HTTP/1.1\r\nhost: localhost\r\n\r\n"]),
        First = receive_until(Socket, ~"own-reply", <<>>),
        ok = gen_tcp:send(Socket, ~"GET /plain HTTP/1.1\r\nhost: localhost\r\n\r\n"),
        receive_until(Socket, ~"still-here", First)
    after
        gen_tcp:close(Socket)
    end.

receive_until(Socket, Expected, Acc) ->
    case binary:match(Acc, Expected) of
        nomatch ->
            {ok, <<Data/binary>>} = gen_tcp:recv(Socket, 0, 5000),
            receive_until(Socket, Expected, <<Acc/binary, Data/binary>>);
        _Found ->
            Acc
    end.
