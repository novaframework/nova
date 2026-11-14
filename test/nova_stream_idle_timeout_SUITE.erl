%%% Cowboy resets its idle timer on data received, not on data sent, so a
%%% response that only streams out is cut once the timeout passes. This pins
%%% that, and the per-stream option the handlers guide tells you to set.
-module(nova_stream_idle_timeout_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([
         stream_is_cut_at_the_idle_timeout/1,
         stream_with_its_own_idle_timeout_survives/1
        ]).

-export([handle_stream/3]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(IDLE_TIMEOUT, 300).

all() ->
    [
     stream_is_cut_at_the_idle_timeout,
     stream_with_its_own_idle_timeout_survives
    ].

init_per_suite(Config) ->
    Host = [#{prefix => "",
              routes => [
                         {"/stream", fun nova_stream_idle_timeout_pages:stream/1, #{methods => [get]}},
                         {"/stream-with-idle-timeout", fun nova_stream_idle_timeout_pages:stream_with_idle_timeout/1,
                          #{methods => [get]}}
                        ]}],
    Port = nova_test_listener:start(#{host => Host,
                                      cowboy_options => #{idle_timeout => ?IDLE_TIMEOUT}}),
    ok = nova_handlers:register_handler(test_stream, fun ?MODULE:handle_stream/3),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    nova_test_listener:stop().

handle_stream({test_stream, IdleTimeout}, _Callback, Req0) ->
    case IdleTimeout of
        listener_default -> ok;
        _ -> cowboy_req:cast({set_options, #{idle_timeout => IdleTimeout}}, Req0)
    end,
    Req = cowboy_req:stream_reply(200, #{~"content-type" => ~"text/event-stream"}, Req0),
    [begin
         timer:sleep(?IDLE_TIMEOUT div 2),
         cowboy_req:stream_body([~"data: chunk-", integer_to_binary(N), ~"\n\n"], nofin, Req)
     end || N <- lists:seq(1, 5)],
    cowboy_req:stream_body(<<>>, fin, Req),
    exit(normal).

stream_is_cut_at_the_idle_timeout(Config) ->
    {200, Body} = nova_test_listener:get(?config(port, Config), "/stream"),
    ?assertEqual(nomatch, binary:match(Body, ~"chunk-5")).

stream_with_its_own_idle_timeout_survives(Config) ->
    {200, Body} = nova_test_listener:get(?config(port, Config), "/stream-with-idle-timeout"),
    ?assertNotEqual(nomatch, binary:match(Body, ~"chunk-1")),
    ?assertNotEqual(nomatch, binary:match(Body, ~"chunk-5")).
