%%% The controllers of nova_boot_SUITE.
-module(nova_boot_pages).

-export([routes/1, page/1, allow/1]).

%% As a router: one route, named after the environment it was asked for.
routes(Environment) ->
    [#{prefix => "/environment",
       routes => [{"/" ++ atom_to_list(Environment), fun ?MODULE:page/1, #{methods => [get]}}]}].

page(_Req) ->
    {status, 200, #{}, ~"page"}.

allow(_Req) ->
    true.
