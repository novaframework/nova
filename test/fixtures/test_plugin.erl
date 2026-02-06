-module(test_plugin).

%% Test plugin for nova_plugin_handler tests
-export([
         pre_request/4,
         post_request/4,
         init/1,
         stop/1
        ]).

%% Plugin callbacks

%% Returns {ok, Req, State} - continue to next plugin
pre_request(Req, _Env, #{action := ok}, State) ->
    {ok, Req#{plugin_ok => true}, State};

%% Returns {ok, Reply, Req, State} - continue with reply data
pre_request(Req, _Env, #{action := ok_with_reply}, State) ->
    Reply = {reply, 200, #{<<"x-plugin">> => <<"active">>}, <<"Plugin data">>},
    {ok, Reply, Req, State};

%% Returns {break, Req, State} - stop plugin chain, continue to handler
pre_request(Req, _Env, #{action := break}, State) ->
    {break, Req#{plugin_break => true}, State};

%% Returns {break, Reply, Req, State} - stop with reply
pre_request(Req, _Env, #{action := break_with_reply}, State) ->
    Reply = {reply, 403, #{}, <<"Forbidden">>},
    {break, Reply, Req, State};

%% Returns {stop, Req, State} - stop entire request
pre_request(Req, _Env, #{action := stop}, State) ->
    {stop, Req#{plugin_stop => true}, State};

%% Returns {stop, Reply, Req, State} - stop and send response immediately
pre_request(Req, _Env, #{action := stop_with_reply}, State) ->
    Reply = {reply, 401, #{}, <<"Unauthorized">>},
    {stop, Reply, Req, State};

%% Crash scenario
pre_request(_Req, _Env, #{action := crash}, _State) ->
    error(plugin_crash);

%% Default - just continue
pre_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

%% Post-request plugin
post_request(Req, _Env, _Options, State) ->
    {ok, Req#{post_plugin_executed => true}, State}.

%% Lifecycle callbacks
init(_Options) ->
    {ok, #{}}.

stop(_State) ->
    ok.
