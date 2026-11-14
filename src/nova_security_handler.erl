-module(nova_security_handler).
-moduledoc false.
-behaviour(cowboy_middleware).

-export([
         execute/2,
         deny/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("../include/nova.hrl").

execute(Req, Env = #{secure := Secure}) ->
    case check(Secure, Req, Env) of
        {allowed, Req1, Env1} -> nova_plugin_handler:after_security(Req1, Env1);
        {answered, Answer} -> Answer
    end.

%% Whether the request may go on. The plugins listed under
%% plugins_after_security run once, after every callback has allowed it.
check(false, Req, Env) ->
    {allowed, Req, Env};
check([_ | _] = Callbacks, Req, Env) ->
    all_allow(Callbacks, Req, Env);
check(Callback, Req = #{host := Host}, Env) when is_function(Callback, 1) ->
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
            {answered, nova_router:render_status_page(Host, 500, #{}, Req#{crash_info => Payload}, Env)};
        Class:Reason0 ->
            Reason = nova_redact:term(Reason0),
            ?LOG_ERROR(#{msg => <<"Security handler crashed">>,
                         class => Class,
                         reason => Reason}),
            Payload = nova_redact:crash_info(Class, Reason0, #{status_code => 500}),
            {answered, nova_router:render_status_page(Host, 500, #{}, Req#{crash_info => Payload}, Env)}
    end;
check(_Invalid, Req, Env) ->
    check(fun deny/1, Req, Env).

%% A route with several callbacks: each has to allow the request, in order,
%% and the first that does not answers it. The data a later callback returns
%% replaces that of an earlier one, which a plain true leaves in place.
all_allow([], Req, Env) ->
    {allowed, Req, Env};
all_allow([Callback | Rest], Req, Env) ->
    case check(Callback, Req, Env) of
        {allowed, Req1, Env1} -> all_allow(Rest, Req1, Env1);
        Answered -> Answered
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
            {allowed, Req, Env#{arguments => Args#{controller_data => #{auth_data => AuthData}}}};
        _ ->
            {allowed, Req#{auth_data => AuthData}, Env}
    end;
handle_response(true, Req, Env) ->
    {allowed, Req, Env};
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
    {answered, {stop, Req3}};
handle_response({redirect, Route}, Req, Env) when is_binary(Route); is_list(Route) ->
    case nova_basic_handler:location(Route) of
        error ->
            ?LOG_ERROR(#{msg => ~"Security callback redirected to an invalid location"}),
            handle_response(invalid_redirect, Req, Env);
        Location ->
            Req0 = cowboy_req:set_resp_headers(#{~"location" => Location}, Req),
            Req1 = cowboy_req:reply(302, Req0),
            {answered, {stop, Req1}}
    end;
handle_response(_, Req = #{host := Host}, Env) ->
    {ok, Req0, _Env0} = nova_router:render_status_page(Host, 401, #{}, Req, Env),
    Req1 = cowboy_req:reply(401, Req0),
    {answered, {stop, Req1}}.
