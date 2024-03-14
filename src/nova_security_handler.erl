-module(nova_security_handler).
-behaviour(cowboy_middleware).

-export([
         execute/2
        ]).

-include_lib("kernel/include/logger.hrl").

execute(Req, Env = #{secure := false}) ->
    {ok, Req, Env};
execute(Req = #{host := Host}, Env = #{secure := SecurityCb}) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try do_call(SecurityCb, Req) of
        Result ->
            handle_response(Result, Req, Env)
    catch
        Class:Reason:Stacktrace when UseStacktrace == true ->
            ?LOG_ERROR(#{msg => <<"Security handler crashed">>,
                         class => Class,
                         reason => Reason,
                         stacktrace => Stacktrace}),
            Payload = #{status_code => 500,
                        stacktrace => Stacktrace,
                        class => Class,
                        reason => Reason},
            {ok, Req0, _Env} = nova_router:render_status_page(Host, 500, #{}, Req#{crash_info => Payload}, Env),
            Req1 = cowboy_req:reply(500, Req0),
            {stop, Req1};
        Class:Reason ->
            ?LOG_ERROR(#{msg => <<"Security handler crashed">>,
                         class => Class,
                         reason => Reason}),
            Payload = #{status_code => 500,
                        class => Class,
                        reason => Reason},
            {ok, Req0, _Env} = nova_router:render_status_page(Host, 500, #{}, Req#{crash_info => Payload}, Env),
            Req1 = cowboy_req:reply(500, Req0),
            {stop, Req1}
    end.

do_call(Fun, Req) when is_function(Fun, 1) ->
    apply(Fun, [Req]);
do_call({Module, Function}, Req) ->
    apply(Module, Function, [Req]).


handle_response({true, AuthData}, Req, Env) ->
    case maps:get(cowboy_handler, Env, undefined) of
        nova_ws_handler ->
            Args = maps:get(arguments, Env, #{}),
            {ok, Req, Env#{arguments => Args#{controller_data => #{auth_data => AuthData}}}};
        _ ->
            {ok, Req#{auth_data => AuthData}, Env}
    end;
handle_response(true, Req, Env) ->
    {ok, Req, Env};
handle_response({false, Headers}, Req, Env) ->
    handle_response({false, 401, Headers, false}, Req, Env);
handle_response({false, StatusCode, Headers}, Req, Env) ->
    handle_response({false, StatusCode, Headers, false}, Req, Env);
handle_response({false, StatusCode, Headers, Body}, Req = #{host := Host}, Env) ->
    {ok, Req1, _Env1} =
        case Body of
            false ->
                {ok, _Req0, _Env0} = nova_router:render_status_page(Host, StatusCode, #{}, Req, Env);
            _ ->
                Req0 = cowboy_req:set_resp_body(Body, Req),
                {ok, Req0, Env}
        end,
    Req2 = cowboy_req:set_resp_headers(Headers, Req1),
    Req3 = cowboy_req:reply(StatusCode, Req2),
    {stop, Req3};
handle_response({redirect, Route}, Req, _Env) ->
    Req0 = cowboy_req:set_resp_headers(#{<<"Location">> => list_to_binary(Route)}, Req),
    {stop, Req0};
handle_response(_, Req = #{host := Host}, Env) ->
    {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
    Req1 = cowboy_req:reply(401, Req0),
    {stop, Req1}.
