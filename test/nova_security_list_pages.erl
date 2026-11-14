%%% The plugin of nova_security_list_SUITE: tells the test case that it ran,
%%% through the process the callbacks of nova_secure_layers_pages report to.
-module(nova_security_list_pages).
-behaviour(nova_plugin).

-export([pre_request/4, plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    nova_secure_layers_pages ! {nova_secure_layers_pages, plugin},
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_security_list_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Reports that it ran"}.
