-module(nova_plugin_handler).
-behaviour(cowboy_middleware).

-export([
         execute/2
        ]).

-include_lib("kernel/include/logger.hrl").

execute(Req = #{plugins := Plugins}, Env = #{plugin_state := pre_request}) ->
    %% This is a post plugin
    PostPlugins = proplists:get_value(post_request, Plugins, []),
    run_plugins(PostPlugins, post_request, Req, Env);
execute(Req = #{plugins := Plugins}, Env) ->
    %% Determine which pre-plugin this is
    PrePlugins = proplists:get_value(pre_request, Plugins, []),
    run_plugins(PrePlugins, pre_request, Req, Env);
execute(Req, Env) ->
    %% The router could not find any match for us
    {ok, Req, Env}.


run_plugins([], Callback, Req, Env) ->
    {ok, Req, Env#{plugin_state => Callback}};
run_plugins([{Module, Options}|Tl], Callback, Req, Env) ->
    ExtraState = maps:get(extra_state, Env, #{}),
    Req0 = Req#{extra_state => ExtraState},
    Args = case proplists:get_value(Callback, Module:module_info(exports)) of
               2 -> [Req0, Options];
               3 -> [Req0, Env, Options];
               _ -> {throw, bad_callback}
           end,
    try erlang:apply(Module, Callback, Args) of
        {ok, Req1} ->
            run_plugins(Tl, Callback, Req1, Env);
        {ok, Reply, Req1} ->
            Req2 = handle_reply(Reply, Req1),
            run_plugins(Tl, Callback, Req2, Env);
        {break, Req1} ->
            {ok, Req1};
        {break, Reply, Req1} ->
            Req2 = handle_reply(Reply, Req1),
            {ok, Req2};
        {stop, Req1} ->
            {stop, Req1};
        {stop, Reply, Req1} ->
            Req2 = handle_reply(Reply, Req1),
            {stop, Req2}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{msg => <<"Plugin crashed">>, class => Class, reason => Reason, stacktrace => Stacktrace}),
            Req1 = Req0#{crash_info => #{class => Class,
                                         reason => Reason,
                                         stacktrace => Stacktrace}},
            nova_router:render_status_page('_', 500, #{}, Req1, Env)
    end.

handle_reply({reply, Body}, Req) ->
    handle_reply({reply, 200, #{}, Body}, Req);
handle_reply({reply, Status, Body}, Req) ->
    handle_reply({reply, Status, #{}, Body}, Req);
handle_reply({reply, Status, Headers, Body}, Req) ->
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    Req1#{resp_status_code => Status};
handle_reply(_, Req) ->
    Req.
