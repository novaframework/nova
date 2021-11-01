-module(nova_plugin_handler).
-behaviour(cowboy_middleware).

-export([
         execute/2
        ]).

-include_lib("nova/include/nova.hrl").

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
    try Module:Callback(Req, Options) of
        {ok, Req0} ->
            run_plugins(Tl, Callback, Req0, Env);
        {break, Req0} ->
            {ok, Req0};
        {stop, Req0} ->
            {stop, Req0}
    catch
        Class:Reason:Stacktrace ->
            ?ERROR("Plugin crashed (~p, ~p), Stacktrace: ~p", [Class, Reason, Stacktrace]),
            Req0 = Req#{crash_info => #{class => Class,
                                        reason => Reason,
                                        stacktrace => Stacktrace}},
            nova_router:render_status_page('_', 500, #{}, Req0, Env)
    end.
