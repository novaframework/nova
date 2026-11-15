-module(nova_plugin_handler).
-moduledoc false.
-behaviour(cowboy_middleware).

-export([
         execute/2,
         after_security/2
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

%% The pre_request plugins a route lists under plugins_after_security.
%% nova_security_handler calls this once the request is let through.
-spec after_security(Req, Env) -> {ok, Req, Env} | {stop, Req}
              when Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
after_security(Req = #{plugins := Plugins}, Env) when is_list(Plugins) ->
    case proplists:get_value(after_security, Plugins, []) of
        [] -> {ok, Req, Env};
        AfterSecurity -> run_plugins(AfterSecurity, pre_request, Req, Env)
    end;
after_security(Req, Env) ->
    {ok, Req, Env}.


run_plugins([], Callback, Req, Env) ->
    {ok, Req, Env#{plugin_state => Callback}};
run_plugins([{Module, Options}|Tl], Callback, Req, Env) when is_atom(Module) ->
    run_plugins([{fun Module:Callback/4, Options}|Tl], Callback, Req, Env);
run_plugins([{Callback, Options}|Tl], CallbackType, Req, Env) when is_function(Callback) ->
    %% Fetch state
    State =
        case nova_plugin_manager:get_state(Callback) of
            {ok, State0} ->
                State0;
            {error, _Reason} ->
                undefined
        end,

    Span = nova_telemetry:start([nova, plugin],
                                #{plugin => plugin_module(Callback),
                                  callback => CallbackType,
                                  app => maps:get(app, Env, undefined),
                                  route => maps:get(nova_route, Req, undefined)}),
    try Callback(Req, Env, Options, State) of
        Result ->
            nova_telemetry:stop(Span, #{result => plugin_result(Result)}),
            handle_result(Result, Callback, State, Tl, CallbackType, Req, Env)
    catch
        %% The client's doing, which Cowboy answers with a 4xx when nothing
        %% catches it. After the controller the response is already out.
        exit:{request_error, Kind, _Message} when CallbackType =:= pre_request ->
            Status = nova_handler:request_error_status(Kind),
            nova_telemetry:stop(Span, #{result => stop}),
            ?LOG_WARNING(#{msg => ~"Request error in plugin", plugin => plugin_module(Callback),
                           kind => nova_redact:term(Kind), status_code => Status}),
            {stop, cowboy_req:reply(Status, Req)};
        Class:Reason0:Stacktrace0 ->
            nova_telemetry:exception(Span, Class, Reason0, Stacktrace0),
            Reason = nova_redact:term(Reason0),
            Stacktrace = nova_redact:stacktrace(Stacktrace0),
            ?LOG_ERROR(#{msg => <<"Plugin crashed">>, class => Class, reason => Reason, stacktrace => Stacktrace}),
            render_500(Req, Env, nova_redact:crash_info(Class, Reason0, #{stacktrace => Stacktrace}))
    end.

handle_result({ok, Req0, State0}, Callback, State, Tl, CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    run_plugins(Tl, CallbackType, Req0, Env);
%% Only a reply can be handed on: the controller answers after this, so a
%% redirect or a bare status would be overwritten. Those need stop.
handle_result({ok, Reply, Req0, State0}, Callback, State, Tl, CallbackType, _Req, Env)
  when element(1, Reply) =:= reply ->
    set_state(Callback, State, State0),
    case handle_reply(Reply, Req0, Callback) of
        {ok, Req1} -> run_plugins(Tl, CallbackType, Req1, Env);
        error -> unsupported_reply(Callback, Reply, Req0, Env)
    end;
handle_result({ok, Reply, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, Env) when is_tuple(Reply) ->
    set_state(Callback, State, State0),
    ?LOG_ERROR(#{msg => ~"Plugin returned {ok, Reply, Req, State} with a reply other than {reply, ...}. Return {stop, Reply, Req, State} to redirect or answer with a status",
                 plugin => plugin_module(Callback), reply => nova_redact:term(Reply)}),
    render_500(Req0, Env, #{class => error, reason => unsupported_plugin_reply});
handle_result({break, Req0, State0}, Callback, State, Tl, CallbackType, Req, Env) ->
    deprecated_break(Callback),
    handle_result({stop, Req0, State0}, Callback, State, Tl, CallbackType, Req, Env);
handle_result({break, Reply, Req0, State0}, Callback, State, Tl, CallbackType, Req, Env) ->
    deprecated_break(Callback),
    handle_result({stop, Reply, Req0, State0}, Callback, State, Tl, CallbackType, Req, Env);
handle_result({stop, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, _Env) ->
    set_state(Callback, State, State0),
    {stop, Req0};
handle_result({stop, Req0}, Callback, _State, _Tl, _CallbackType, _Req, _Env) when is_map(Req0) ->
    deprecated_stop(Callback),
    {stop, Req0};
%% The controller has answered by the time the post_request plugins run, and
%% a plugin may have answered itself before it asks Nova to. Cowboy exits on
%% a reply to a request it marked as answered. A second answer to a request
%% without the mark is dropped by nova_stream_h.
handle_result({stop, _Reply, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, _Env)
  when is_map_key(has_sent_resp, Req0) ->
    set_state(Callback, State, State0),
    warn_once(Callback, late_reply_warned,
              ~"Plugin replied after the response was sent. The reply is dropped"),
    {stop, Req0};
handle_result({stop, Reply, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    case handle_reply(Reply, Req0, Callback) of
        {ok, Req1} ->
            %% Stopping means nobody else will answer, so the reply goes out here.
            StatusCode = maps:get(resp_status_code, Req1, 200),
            {stop, cowboy_req:reply(StatusCode, Req1)};
        error ->
            unsupported_reply(Callback, Reply, Req0, Env)
    end;
handle_result({error, Reason}, Callback, _State, _Tl, _CallbackType, Req, Env) ->
    ?LOG_ERROR(#{msg => ~"Plugin returned an error", plugin => plugin_module(Callback),
                 reason => nova_redact:term(Reason)}),
    render_500(Req, Env, nova_redact:crash_info(error, Reason, #{}));
handle_result(Result, Callback, _State, _Tl, _CallbackType, Req, Env) ->
    ?LOG_ERROR(#{msg => ~"Plugin returned an unsupported result", plugin => plugin_module(Callback),
                 result => nova_redact:term(Result)}),
    render_500(Req, Env, #{class => error, reason => unsupported_plugin_result}).

handle_reply({reply, Body}, Req, Callback) ->
    handle_reply({reply, 200, #{}, Body}, Req, Callback);
handle_reply({reply, Status, Body}, Req, Callback) ->
    handle_reply({reply, Status, #{}, Body}, Req, Callback);
handle_reply({reply, Status, Headers, Body}, Req, _Callback)
  when is_integer(Status), is_map(Headers) orelse is_list(Headers) ->
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    {ok, Req1#{resp_status_code => Status}};
handle_reply({redirect, Url}, Req, _Callback) when is_binary(Url); is_list(Url) ->
    case nova_basic_handler:location(Url) of
        error ->
            error;
        Location ->
            Req0 = cowboy_req:set_resp_headers(#{~"location" => Location}, Req),
            {ok, Req0#{resp_status_code => 302}}
    end;
handle_reply({redirect_local, Url}, Req, Callback) when is_binary(Url); is_list(Url) ->
    case nova_basic_handler:local_location(Url) of
        error ->
            ?LOG_WARNING(#{msg => ~"Plugin refused to redirect to a location that is not a local path",
                           plugin => plugin_module(Callback),
                           location => nova_basic_handler:loggable_location(Url)}),
            {ok, Req#{resp_status_code => 400}};
        Location ->
            Req0 = cowboy_req:set_resp_headers(#{~"location" => Location}, Req),
            {ok, Req0#{resp_status_code => 302}}
    end;
handle_reply({status, Status}, Req, Callback) ->
    handle_reply({status, Status, #{}}, Req, Callback);
handle_reply({status, Status, Headers}, Req, _Callback)
  when is_integer(Status), is_map(Headers) orelse is_list(Headers) ->
    {ok, (cowboy_req:set_resp_headers(Headers, Req))#{resp_status_code => Status}};
handle_reply(_Reply, _Req, _Callback) ->
    error.

unsupported_reply(Callback, Reply, Req, Env) ->
    ?LOG_ERROR(#{msg => ~"Plugin returned an unsupported reply", plugin => plugin_module(Callback),
                 reply => nova_redact:term(loggable_reply(Reply))}),
    render_500(Req, Env, #{class => error, reason => unsupported_plugin_reply}).

loggable_reply({Redirect, Url}) when Redirect =:= redirect; Redirect =:= redirect_local ->
    {Redirect, nova_basic_handler:loggable_location(Url)};
loggable_reply(Reply) ->
    Reply.

%% The status page runs through nova_handler and then the post_request phase.
%% Marking the pre_request phase done keeps the pre plugins from running twice.
%% The default 500 page needs crash_info. The plugins waiting for the
%% security callback are dropped: the request they were to prepare is over.
render_500(Req, Env, CrashInfo) ->
    {ok, Req0, Env0} = nova_router:render_status_page('_', 500, #{}, Req#{crash_info => CrashInfo}, Env),
    {ok, without_after_security(Req0), Env0#{plugin_state => pre_request}}.

without_after_security(Req = #{plugins := Plugins}) ->
    Req#{plugins => proplists:delete(after_security, Plugins)};
without_after_security(Req) ->
    Req.

%% Once per plugin module, so a plugin returning break on every request does
%% not flood the log.
deprecated_break(Callback) ->
    warn_once(Callback, break_warned,
              ~"Plugin returned break, which is deprecated and behaves like stop. Return stop instead").

deprecated_stop(Callback) ->
    warn_once(Callback, stop_req_warned,
              ~"Plugin returned {stop, Req}, which is deprecated and will be removed in 1.0. Return {stop, Req, State} instead").

warn_once(Callback, Tag, Msg) ->
    Module = plugin_module(Callback),
    Key = {?MODULE, Tag, Module},
    case nova_once:first(Key) of
        true ->
            ?LOG_WARNING(#{msg => Msg, plugin => Module});
        false ->
            ok
    end.

plugin_module(Callback) ->
    {module, Module} = erlang:fun_info(Callback, module),
    Module.

%% Names the return, whatever Nova goes on to do with it. {ok, Reply, Req,
%% State} is part of the plugin contract only with a {reply, ...} reply.
plugin_result({ok, _Req, _State})                                         -> ok;
plugin_result({ok, Reply, _Req, _State}) when element(1, Reply) =:= reply -> ok;
plugin_result({break, _Req, _State})                                      -> break;
plugin_result({break, _Reply, _Req, _State})                              -> break;
plugin_result({stop, _Req, _State})                                       -> stop;
plugin_result({stop, _Reply, _Req, _State})                               -> stop;
plugin_result({stop, Req}) when is_map(Req)                               -> stop;
plugin_result({error, _Reason})                                           -> error;
plugin_result(_Return)                                                    -> invalid.

%% Plugin state lives in nova_plugin_manager's table and is written through
%% one gen_server. Most plugins hand their state back untouched, so only
%% changes are written.
set_state(_Callback, State, State) ->
    ok;
set_state(Callback, _OldState, NewState) ->
    nova_plugin_manager:set_state(Callback, NewState).
