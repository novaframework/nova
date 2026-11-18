%%% The controllers and the plugin of nova_request_error_SUITE.
-module(nova_request_error_pages).
-behaviour(nova_plugin).

-export([echo/1, parse_qs/1, short_body/1, slow_body/1]).
-export([pre_request/4, plugin_info/0]).

echo(Req) ->
    {json, 200, #{}, #{~"parsed_qs" => maps:get(parsed_qs, Req, null),
                       ~"params" => maps:get(params, Req, null)}}.

parse_qs(Req) ->
    {json, 200, #{}, maps:from_list(nova_req:parse_qs(Req))}.

short_body(Req) ->
    {ok, Fields, _Req} = nova_req:read_urlencoded_body(Req, #{length => 8, period => 200}),
    {json, 200, #{}, maps:from_list(Fields)}.

slow_body(Req) ->
    {ok, Fields, _Req} = nova_req:read_urlencoded_body(Req, #{period => 200}),
    {json, 200, #{}, maps:from_list(Fields)}.

pre_request(Req, _Env, _Options, State) ->
    #{id := _Id} = cowboy_req:match_qs([{id, int}], Req),
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_request_error_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Wants an integer id in the query string"}.
