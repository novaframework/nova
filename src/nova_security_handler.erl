-module(nova_security_handler).
-behaviour(cowboy_middleware).

-export([
         execute/2
        ]).

-include_lib("kernel/include/logger.hrl").

execute(Req, Env = #{secure := false}) ->
    {ok, Req, Env};
execute(Req = #{host := Host}, Env = #{secure := {Module, Function}}) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try Module:Function(Req) of
        {true, AuthData} ->
            case maps:get(cowboy_handler, Env, undefined) of
                nova_ws_handler ->
                    Args = maps:get(arguments, Env, #{}),
                    {ok, Req, Env#{arguments => Args#{controller_data => #{auth_data => AuthData}}}};
                _ ->
                    {ok, Req#{auth_data => AuthData}, Env}
            end;
        true ->
            {ok, Req, Env};
        {false, Headers} ->
            {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
            ExistingHeaders = cowboy_req:headers(Req),
            FinalHeaders = maps:merge(ExistingHeaders, Headers),
            Req1 = cowboy_req:set_resp_headers(FinalHeaders, Req0),
            Req2 = cowboy_req:reply(401, Req1),
            {stop, Req2};
        {redirect, Route} ->
            Req0 = cowboy_req:set_resp_headers(#{<<"Location">> => list_to_binary(Route)}, Req),
            {stop, Req0};
        _ ->
            {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
            Req1 = cowboy_req:reply(401, Req0),
            {stop, Req1}
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
