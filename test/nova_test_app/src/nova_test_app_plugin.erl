%%% Records that it ran, so the test suite can assert both plugin phases fire.
%%%
%%% pre_request can still set response headers, but post_request runs after
%%% nova_handler has already replied, so it counts through a shared counter
%%% instead. The suite installs the counter under nova_test_app_plugin in
%%% persistent_term; without it the plugin is a no-op.
-module(nova_test_app_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

-define(COUNTERS, nova_test_app_plugin).
-define(PRE_REQUEST, 1).
-define(POST_REQUEST, 2).

pre_request(Req, _Env, _Options, State) ->
    bump(?PRE_REQUEST),
    {ok, cowboy_req:set_resp_header(<<"x-nova-pre-request">>, <<"1">>, Req), State}.

post_request(Req, _Env, _Options, State) ->
    bump(?POST_REQUEST),
    {ok, Req, State}.

bump(Index) ->
    case persistent_term:get(?COUNTERS, undefined) of
        undefined -> ok;
        Ref       -> counters:add(Ref, Index, 1)
    end.

plugin_info() ->
    #{
      title => <<"nova_test_app_plugin">>,
      version => <<"1.0.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org>">>],
      description => <<"Test plugin that marks the request in both phases">>,
      options => []
     }.
