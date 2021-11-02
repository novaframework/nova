-module(nova_security_handler).
-behaviour(cowboy_middleware).

-include_lib("nova/include/nova.hrl").

-export([
         execute/2
        ]).



execute(Req, Env = #{secure := false}) ->
    {ok, Req, Env};
execute(Req = #{host := Host}, Env = #{secure := {Module, Function}}) ->
    try Module:Function(Req) of
        {true, AuthData} ->
            case maps:get(cowboy_handler, Env) of
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
            {stop, Req1};
        {redirect, Route} ->
            Req0 = cowboy_req:set_resp_headers(#{<<"Location">> => list_to_binary(Route)}, Req),
            {stop, Req0};
        _ ->
            {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
            {stop, Req0}
    catch _Class:_Reason ->
            nova_router:render_status_page(Host, 500, #{}, Req, Env)
    end.
