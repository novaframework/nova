%%% The plugin, the security callback and the controllers of
%%% nova_plugin_once_SUITE.
-module(nova_plugin_once_pages).
-behaviour(nova_plugin).

-export([index/1, allow/1]).
-export([init/1, websocket_handle/2, websocket_info/2]).
-export([pre_request/4, post_request/4, plugin_info/0]).

index(_Req) ->
    {status, 200, #{}, ~"index"}.

allow(_Req) ->
    nova_plugin_once_SUITE:observed(security),
    true.

init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle(_Frame, ControllerData) ->
    {reply, {text, ~"ok"}, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.

pre_request(Req, _Env, #{name := Name} = Options, State) ->
    nova_plugin_once_SUITE:observed({pre_request, Name}),
    case maps:get(action, Options, pass) of
        pass -> {ok, Req, State};
        crash -> error(plugin_failed);
        error -> {error, plugin_refused}
    end.

post_request(Req, _Env, #{name := Name}, State) ->
    nova_plugin_once_SUITE:observed({post_request, Name}),
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_plugin_once_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Tells the suite each time it runs, then passes, crashes or returns an error"}.
