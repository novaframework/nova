%%% The controllers, the fallback controller and the plugin of
%%% nova_own_reply_SUITE. Each controller answers the request with Cowboy and
%%% then returns as if it had not.
-module(nova_own_reply_pages).
-behaviour(nova_plugin).

-fallback_controller(nova_own_reply_pages).

-export([ok/1, view/1, json/1, status/1, redirect/1, unsupported/1, to_fallback/1, crash/1, thrown/1,
         before_plugin/1, plain/1, not_found/1, resolve/2]).
-export([denies/1, denies_with_a_body/1, redirects/1]).
-export([pre_request/4, post_request/4, plugin_info/0]).

ok(Req) ->
    answer(Req),
    {ok, #{}, #{view => nova_error}}.

view(Req) ->
    answer(Req),
    {view, #{}, #{view => nova_error}}.

json(Req) ->
    answer(Req),
    {json, #{from => ~"the return"}}.

status(Req) ->
    answer(Req),
    {status, 201, #{}, ~"from the return"}.

redirect(Req) ->
    answer(Req),
    {redirect, "/plain"}.

unsupported(Req) ->
    answer(Req),
    answered.

to_fallback(_Req) ->
    answer_in_the_fallback.

crash(Req) ->
    answer(Req),
    error(crashed_after_the_answer).

thrown(Req) ->
    answer(Req),
    throw({404, gone}).

before_plugin(Req) ->
    answer(Req),
    {status, 200, #{}, ~"from the return"}.

plain(_Req) ->
    {status, 200, #{}, ~"still-here"}.

not_found(_Req) ->
    {status, 404, #{}, ~"page 404"}.

resolve(_Req, answered) ->
    {status, 409, #{}, ~"from the fallback"};
resolve(Req, answer_in_the_fallback) ->
    answer(Req),
    {status, 409, #{}, ~"from the fallback"}.

denies(Req) ->
    answer(Req),
    false.

denies_with_a_body(Req) ->
    answer(Req),
    {false, 403, #{}, ~"from the callback"}.

redirects(Req) ->
    answer(Req),
    {redirect, "/plain"}.

pre_request(Req, _Env, #{then := stop}, State) ->
    answer(Req),
    {stop, {reply, 503, #{}, ~"from the plugin"}, Req, State};
pre_request(Req, _Env, #{then := crash}, _State) ->
    answer(Req),
    error(crashed_after_the_answer);
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {stop, {reply, 503, #{}, ~"from the plugin"}, maps:remove(has_sent_resp, Req), State}.

plugin_info() ->
    #{title => ~"nova_own_reply_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Replies after the controller, with a request that does not say it was answered"}.

answer(Req) ->
    _ = nova_req:reply(200, #{~"content-type" => ~"text/plain"}, ~"own-reply", Req),
    ok.
