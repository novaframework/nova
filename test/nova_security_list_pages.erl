%%% The plugin of nova_security_list_SUITE: tells the test case that it ran,
%%% through the process the callbacks of nova_secure_layers_pages report to.
-module(nova_security_list_pages).
-behaviour(nova_plugin).

-export([pre_security/4, pre_request/4, plugin_info/0]).

%% With `early` or `late` in its options, the plugin puts that under
%% `auth_data` in the request, ahead of the security callback or behind it.
pre_security(Req, _Env, #{early := AuthData}, State) ->
    {ok, Req#{auth_data => AuthData}, State};
pre_security(Req, _Env, _Options, State) ->
    {ok, Req, State}.

pre_request(Req, _Env, Options, State) ->
    nova_secure_layers_pages ! {nova_secure_layers_pages, plugin},
    case Options of
        #{late := AuthData} -> {ok, Req#{auth_data => AuthData}, State};
        _ -> {ok, Req, State}
    end.

plugin_info() ->
    #{title => ~"nova_security_list_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Reports that it ran"}.
