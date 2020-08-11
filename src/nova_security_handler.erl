-module(nova_security_handler).
-behaviour(cowboy_middleware).

-include_lib("nova/include/nova.hrl").

-export([
         execute/2
        ]).

%%--------------------------------------------------------------------
%% @doc
%% If a security module/func is defined in the route of the request that
%% module/func will be called. If the result of that is false it will
%% return a 401 to the requester. Otherwise it will continue to handle
%% the request.
%% @end
%%--------------------------------------------------------------------
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
                               when Req :: cowboy_req:req(),
                                    Env :: cowboy_middleware:env().
execute(Req, Env = #{handler_opts := HandlerOpts = #{secure := {Mod, Func}}}) ->
    try Mod:Func(Req) of
        {true, AuthData} ->
            ControllerData = #{auth_data => AuthData},
            {ok, Req, Env#{handler_opts => HandlerOpts#{controller_data => ControllerData}}};
        true ->
            {ok, Req, Env};
        false ->
            {stop, cowboy_req:reply(401, Req)};
        {redirect, Route} ->
            Req1 = cowboy_req:reply(
                     302,
                     #{<<"Location">> => list_to_binary(Route)},
                     Req),
            {stop, Req1};
        {cowboy_req, Req1} ->
            {ok, Req1, Env}
    catch
        Class:Reason ->
            ?ERROR("Security module ~p:~p failed with ~p/~p", [Mod, Func, Class, Reason]),
            {stop, cowboy_req:reply(500, Req)}
    end;
execute(Req, Env) ->
    {ok, Req, Env}.
