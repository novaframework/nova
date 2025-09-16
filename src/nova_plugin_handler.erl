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
            set_state(Callback, Result),
            case Result of
                {ok, Req0, _State0} ->
                    run_plugins(Tl, CallbackType, Req0, Env);
                {ok, Reply, Req0, _State0} ->
                    Req1 = handle_reply(Reply, Req0),
                    run_plugins(Tl, CallbackType, Req1, Env);
                {break, Req0, _State0} ->
                    {ok, Req0};
                {break, Reply, Req0, _State0} ->
                    Req1 = handle_reply(Reply, Req0),
                    {ok, Req1};
                {stop, Req0, _State0} ->
                    {stop, Req0};
                {stop, Reply, Req0, _State0} ->
                    Req1 = handle_reply(Reply, Req0),
                    {stop, Req1}
            end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{msg => <<"Plugin crashed">>, class => Class, reason => Reason, stacktrace => Stacktrace}),
            Req0 = Req#{crash_info => #{class => Class,
                                        reason => Reason,
                                        stacktrace => Stacktrace}},
            nova_router:render_status_page('_', 500, #{}, Req0, Env)
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



set_state(Callback, Return) when is_function(Callback) ->
    ReturnList = erlang:tuple_to_list(Return),
    State = lists:last(ReturnList),
    nova_plugin_manager:set_state(Callback, State).
