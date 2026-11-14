%%% The controller, the plugin and the security callback of
%%% nova_default_pages_SUITE.
-module(nova_default_pages_pages).
-behaviour(nova_plugin).

-export([page/1, refuse/1]).
-export([pre_request/4, post_request/4, plugin_info/0]).

page(_Req) ->
    {status, 200, #{}, ~"page"}.

refuse(_Req) ->
    error(security_failed_secretly).

pre_request(_Req, _Env, _Options, _State) ->
    error(plugin_failed_secretly).

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_default_pages_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Crashes before the controller"}.
