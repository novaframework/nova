%%% The controllers and the plugin of nova_reply_SUITE.
-module(nova_reply_pages).
-behaviour(nova_plugin).

-export([no_content_with_a_body/1, not_modified_with_a_body/1, json_no_content/1, page/1]).
-export([post_request/4, plugin_info/0]).

no_content_with_a_body(_Req) ->
    {status, 204, #{}, ~"not for a 204"}.

not_modified_with_a_body(_Req) ->
    {status, 304, #{~"etag" => ~"\"v1\""}, ~"not for a 304"}.

json_no_content(_Req) ->
    {json, 204, #{}, #{deleted => true}}.

page(_Req) ->
    {status, 200, #{}, ~"page"}.

post_request(Req, _Env, _Options, State) ->
    {stop, {reply, 503, ~"too late"}, Req, State}.

plugin_info() ->
    #{title => ~"nova_reply_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Replies after the controller has"}.
