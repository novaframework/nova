-module(nova_security_handler).
-moduledoc false.
-behaviour(cowboy_middleware).

-export([
         execute/2,
         deny/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova.hrl").

execute(Req, Env = #{secure := false}) ->
    nova_plugin_handler:after_security(Req, Env);
execute(Req, Env = #{secure := [_ | _] = Callbacks}) ->
    all_allow(Callbacks, Req, Env);
execute(Req, Env = #{secure := Invalid}) when not is_function(Invalid, 1) ->
    execute(Req, Env#{secure => fun deny/1});
execute(Req = #{host := Host}, Env = #{secure := Callback}) ->
    UseStacktrace = persistent_term:get(nova_use_stacktrace, false),
    try Callback(Req) of
        Result ->
            handle_response(Result, Req, Env)
    catch
        Class:Reason0:Stacktrace0 when UseStacktrace == true ->
            Reason = nova_redact:term(Reason0),
            Stacktrace = nova_redact:stacktrace(Stacktrace0),
            ?LOG_ERROR(#{msg => <<"Security handler crashed">>,
                         class => Class,
                         reason => Reason,
                         stacktrace => Stacktrace}),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500, stacktrace => Stacktrace}),
            %% On to nova_handler with the 500 page in place of the route.
            nova_router:render_status_page(Host, 500, #{}, Req#{crash_info => Payload}, Env);
        Class:Reason0 ->
            Reason = nova_redact:term(Reason0),
            ?LOG_ERROR(#{msg => <<"Security handler crashed">>,
                         class => Class,
                         reason => Reason}),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500}),
            nova_router:render_status_page(Host, 500, #{}, Req#{crash_info => Payload}, Env)
    end.


%% A route with several callbacks: each has to allow the request, in order,
%% and the first that does not answers it. The data a later callback returns
%% replaces that of an earlier one, which a plain true leaves in place.
all_allow([], Req, Env) ->
    {ok, Req, Env};
all_allow([Callback | Rest], Req, Env = #{secure := Callbacks}) ->
    case execute(Req, Env#{secure => Callback}) of
        {ok, Req1, Env1} -> all_allow(Rest, Req1, Env1#{secure => Callbacks});
        Stop -> Stop
    end.

%% The router refuses a security value it cannot use when it compiles the
%% routes. This stands in for one that reaches a request some other way, so
%% the route is closed rather than left open.
-doc false.
-spec deny(Req :: cowboy_req:req()) -> false.
deny(_Req) ->
    false.

handle_response({true, AuthData}, Req, Env) ->
    case maps:get(cowboy_handler, Env, undefined) of
        nova_ws_handler ->
            Args = maps:get(arguments, Env, #{}),
            nova_plugin_handler:after_security(
              Req, Env#{arguments => Args#{controller_data => #{auth_data => AuthData}}});
        _ ->
            nova_plugin_handler:after_security(Req#{auth_data => AuthData}, Env)
    end;
handle_response(true, Req, Env) ->
    nova_plugin_handler:after_security(Req, Env);
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
handle_response({redirect, Route}, Req, Env) when is_binary(Route); is_list(Route) ->
    case nova_basic_handler:location(Route) of
        error ->
            ?LOG_ERROR(#{msg => ~"Security callback redirected to an invalid location"}),
            handle_response(invalid_redirect, Req, Env);
        Location ->
            Req0 = cowboy_req:set_resp_headers(#{~"location" => Location}, Req),
            Req1 = cowboy_req:reply(302, Req0),
            {stop, Req1}
    end;
handle_response(_, Req = #{host := Host}, Env) ->
    {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
    Req1 = cowboy_req:reply(401, Req0),
    {stop, Req1}.
