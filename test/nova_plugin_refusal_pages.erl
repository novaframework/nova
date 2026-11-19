%%% The plugin and the controller of nova_plugin_refusal_SUITE.
-module(nova_plugin_refusal_pages).
-behaviour(nova_plugin).

-export([index/1]).
-export([pre_request/4, post_request/4, plugin_info/0]).

index(_Req) ->
    nova_test_observer:observed(nova_plugin_refusal_observer, controller),
    {status, 200, #{}, ~"index"}.

%% Every way of refusing hands the request back, the way a careless plugin
%% would, so the suite sees whether its headers reach the log.
pre_request(Req, _Env, #{action := Action}, State) ->
    case Action of
        pass -> {ok, Req, State};
        error -> {error, {refused, Req}};
        unsupported -> {carry_on, Req};
        bare_req -> Req;
        unsupported_reply -> {stop, {teapot, Req}, Req, State};
        crash -> error({refused, Req});
        no_clause -> refuse(Req, Action);
        break -> {break, {reply, 403, #{}, ~"refused"}, Req, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_plugin_refusal_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Refuses a request in every way a plugin can"}.

refuse(Req, never) ->
    Req.
