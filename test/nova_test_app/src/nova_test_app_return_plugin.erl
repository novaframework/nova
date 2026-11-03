%%% Answers with whatever plugin return the route asks for in its options,
%%% so nova_full_app_SUITE can drive each return through a real listener.
-module(nova_test_app_return_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

pre_request(Req, _Env, #{return := break_redirect}, State) ->
    {break, {redirect, ~"/login"}, Req, State};
pre_request(Req, _Env, #{return := break_status}, State) ->
    {break, {status, 401}, Req, State};
pre_request(Req, _Env, #{return := break_unknown}, State) ->
    {break, {not_a_reply, 1}, Req, State};
pre_request(Req, _Env, #{return := stop_two_tuple}, _State) ->
    {stop, cowboy_req:reply(429, #{}, ~"slow down", Req)};
pre_request(_Req, _Env, #{return := error}, _State) ->
    {error, deliberate};
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_test_app_return_plugin",
      version => ~"0.1.0",
      url => ~"https://github.com/novaframework/nova",
      authors => [~"Nova team"],
      description => ~"Returns what the route options ask for"}.
