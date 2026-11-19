%%% The controller, security callback and plugin of nova_redirect_SUITE.
-module(nova_redirect_pages).
-behaviour(nova_plugin).

-export([back/1, guard/1]).
-export([pre_request/4, plugin_info/0]).

back(Req) ->
    {redirect_local, next(Req)}.

guard(Req) ->
    {redirect_local, next(Req)}.

pre_request(Req, _Env, _Options, State) ->
    {stop, {redirect_local, next(Req)}, Req, State}.

plugin_info() ->
    #{title => ~"nova_redirect_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Stops every request with a redirect to its next parameter"}.

next(Req) ->
    #{next := Next} = cowboy_req:match_qs([next], Req),
    Next.
