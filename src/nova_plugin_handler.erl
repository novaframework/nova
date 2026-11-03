-module(nova_plugin_handler).
-moduledoc false.
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
run_plugins([{Module, Options}|Tl], Callback, Req, Env) when is_atom(Module) ->
    run_plugins([{fun Module:Callback/1, Options}|Tl], Callback, Req, Env);
run_plugins([{Callback, Options}|Tl], CallbackType, Req, Env) when is_function(Callback) ->
    %% Fetch state
    State =
        case nova_plugin_manager:get_state(Callback) of
            {ok, State0} ->
                State0;
            {error, _Reason} ->
                undefined
        end,

    try Callback(Req, Env, Options, State) of
        Result ->
            handle_result(Result, Callback, State, Tl, CallbackType, Req, Env)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{msg => <<"Plugin crashed">>, class => Class, reason => Reason, stacktrace => Stacktrace}),
            Req0 = Req#{crash_info => #{class => Class,
                                        reason => Reason,
                                        stacktrace => Stacktrace}},
            nova_router:render_status_page('_', 500, #{}, Req0, Env)
    end.

handle_result({ok, Req0, State0}, Callback, State, Tl, CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    run_plugins(Tl, CallbackType, Req0, Env);
handle_result({ok, Reply, Req0, State0}, Callback, State, Tl, CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    run_plugins(Tl, CallbackType, handle_reply(Reply, Req0), Env);
%% Break skips the rest of this phase's plugins; the request carries on.
handle_result({break, Req0, State0}, Callback, State, _Tl, CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    run_plugins([], CallbackType, Req0, Env);
handle_result({break, Reply, Req0, State0}, Callback, State, _Tl, CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    run_plugins([], CallbackType, handle_reply(Reply, Req0), Env);
handle_result({stop, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, _Env) ->
    set_state(Callback, State, State0),
    {stop, Req0};
handle_result({stop, Req0}, _Callback, _State, _Tl, _CallbackType, _Req, _Env) when is_map(Req0) ->
    {stop, Req0};
handle_result({stop, Reply, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, _Env) ->
    set_state(Callback, State, State0),
    Req1 = handle_reply(Reply, Req0),
    %% Since we are stopping we need to send the statuscode to the requester. This is a special case for stop.
    StatusCode = maps:get(resp_status_code, Req1, 200),
    {stop, cowboy_req:reply(StatusCode, Req1)};
handle_result({error, Reason}, Callback, _State, _Tl, _CallbackType, Req, Env) ->
    {module, Module} = erlang:fun_info(Callback, module),
    ?LOG_ERROR(#{msg => ~"Plugin returned an error", plugin => Module, reason => Reason}),
    nova_router:render_status_page('_', 500, #{}, Req#{crash_info => #{class => error, reason => Reason}}, Env);
handle_result(Result, Callback, _State, _Tl, _CallbackType, Req, Env) ->
    {module, Module} = erlang:fun_info(Callback, module),
    ?LOG_ERROR(#{msg => ~"Plugin returned an unsupported result", plugin => Module,
                 result => Result}),
    nova_router:render_status_page('_', 500, #{}, Req, Env).

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



%% Plugin state lives in nova_plugin_manager's table and is written through
%% one gen_server. Most plugins hand their state back untouched, so only
%% changes are written.
set_state(_Callback, State, State) ->
    ok;
set_state(Callback, _OldState, NewState) ->
    nova_plugin_manager:set_state(Callback, NewState).
