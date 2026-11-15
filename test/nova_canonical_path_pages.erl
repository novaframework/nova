%%% The controllers of nova_canonical_path_SUITE.
-module(nova_canonical_path_pages).

-export([page/1, canonical/1, binding/1, bad_path/1, secure/1, seen/1]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

canonical(#{seen_by_plugin := Path}) ->
    {status, 200, #{}, Path}.

binding(#{bindings := #{~"id" := Id}}) ->
    {status, 200, #{}, Id}.

bad_path(_Req) ->
    {status, 400, #{}, ~"bad path"}.

secure(_Req) ->
    seen(security),
    true.

seen(What) ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> Pid ! {seen, What}, ok
    end.
