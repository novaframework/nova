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
        Class:Reason0:Stacktrace0 ->
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
    case handle_reply(Reply, Req0) of
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
handle_result({stop, Req0}, _Callback, _State, _Tl, _CallbackType, _Req, _Env) when is_map(Req0) ->
    {stop, Req0};
handle_result({stop, Reply, Req0, State0}, Callback, State, _Tl, _CallbackType, _Req, Env) ->
    set_state(Callback, State, State0),
    case handle_reply(Reply, Req0) of
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

handle_reply({reply, Body}, Req) ->
    handle_reply({reply, 200, #{}, Body}, Req);
handle_reply({reply, Status, Body}, Req) ->
    handle_reply({reply, Status, #{}, Body}, Req);
handle_reply({reply, Status, Headers, Body}, Req) when is_integer(Status), is_map(Headers) orelse is_list(Headers) ->
    Req0 = cowboy_req:set_resp_headers(Headers, Req),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    {ok, Req1#{resp_status_code => Status}};
handle_reply({redirect, Url}, Req) when is_binary(Url); is_list(Url) ->
    Req0 = cowboy_req:set_resp_headers(#{~"location" => location(Url)}, Req),
    {ok, Req0#{resp_status_code => 302}};
handle_reply({status, Status}, Req) ->
    handle_reply({status, Status, #{}}, Req);
handle_reply({status, Status, Headers}, Req) when is_integer(Status), is_map(Headers) orelse is_list(Headers) ->
    {ok, (cowboy_req:set_resp_headers(Headers, Req))#{resp_status_code => Status}};
handle_reply(_Reply, _Req) ->
    error.

unsupported_reply(Callback, Reply, Req, Env) ->
    ?LOG_ERROR(#{msg => ~"Plugin returned an unsupported reply", plugin => plugin_module(Callback),
                 reply => nova_redact:term(Reply)}),
    render_500(Req, Env, #{class => error, reason => unsupported_plugin_reply}).

%% The status page runs through nova_handler and then the post_request phase.
%% Marking the pre_request phase done keeps the pre plugins from running twice.
%% The default 500 page needs crash_info.
render_500(Req, Env, CrashInfo) ->
    {ok, Req0, Env0} = nova_router:render_status_page('_', 500, #{}, Req#{crash_info => CrashInfo}, Env),
    {ok, Req0, Env0#{plugin_state => pre_request}}.

%% Once per plugin module, so a plugin returning break on every request does
%% not flood the log.
deprecated_break(Callback) ->
    Module = plugin_module(Callback),
    Key = {?MODULE, break_warned, Module},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?LOG_WARNING(#{msg => ~"Plugin returned break, which is deprecated and behaves like stop. Return stop instead",
                           plugin => Module})
    end.

plugin_module(Callback) ->
    {module, Module} = erlang:fun_info(Callback, module),
    Module.

location(Url) when is_binary(Url) ->
    Url;
location(Url) ->
    case unicode:characters_to_binary(Url) of
        Location when is_binary(Location) -> Location
    end.

%% Plugin state lives in nova_plugin_manager's table and is written through
%% one gen_server. Most plugins hand their state back untouched, so only
%% changes are written.
set_state(_Callback, State, State) ->
    ok;
set_state(Callback, _OldState, NewState) ->
    nova_plugin_manager:set_state(Callback, NewState).
