-module(nova_security_handler).
-behaviour(cowboy_middleware).

-include_lib("nova/include/nova.hrl").

-export([
         execute/2
        ]).



execute(Req, Env = #{secure := false}) ->
    {ok, Req, Env};
execute(Req = #{host := Host, path := Path, method := Method}, Env = #{secure := {Module, Function}}) ->
    try Module:Function(Req) of
        {true, AuthData} ->
            {ok, Req, Env#{controller_data => #{auth_data => AuthData}}};
        true ->
            {ok, Req, Env};
        {false, Headers} ->
            {ok, Req0, Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
            ExistingHeaders = cowboy_req:headers(Req),
            Req1 = cowboy_req:set_resp_headers(ExistingHeaders ++ Headers),
            {stop, Req1};
        {redirect, Route} ->
            Req0 = cowboy_req:set_resp_headers(#{<<"Location">> => list_to_binary(Route)}, Req),
            {stop, Req0};
        _ ->
            {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401),
            {stop, Req0}
    end.
