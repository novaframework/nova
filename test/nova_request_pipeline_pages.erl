%%% The controllers of nova_request_pipeline_SUITE.
-module(nova_request_pipeline_pages).

-export([page/1, deny/1, method_page/1, marked/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

deny(_Req) ->
    false.

method_page(_Req) ->
    {status, 405, #{}, ~"method page"}.

marked(#{headers := Headers}) ->
    is_map_key(~"x-marked", Headers).
