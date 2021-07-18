-module(nova_security_plugin).
-behaviour(nova_plugin).

-include_lib("nova/include/nova.hrl").

-export([
         pre_ws_upgrade/2,
         pre_http_request/2,
         post_http_request/2,
         plugin_info/0
        ]).



%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
-spec pre_http_request(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
                              {ok, State0 :: nova_http_handler:nova_http_state()} |
                              {stop, State0 :: nova_http_handler:nova_http_state()} |
                              {error, Reason :: term()}.
pre_http_request(State = #{req := Req, secure := {Module, Function}}, _Options) ->
    try Module:Function(Req) of
        {true, AuthData} ->
            {ok, State#{controller_data =>
                            #{auth_data => AuthData}}};
        true ->
            {ok, State};
        false ->
            {ok, _Req0, State0} = nova_router:render_status_page(401, Req),
            {stop, State0};
        {false, Headers} ->
            {ok, Req0, State0} = nova_router:render_status_page(401, State),
            Headers0 = cowboy_req:resp_headers(Req0),
            Req1 = cowboy_req:set_resp_headers(maps:merge(Headers0, Headers), Req0),
            {stop, State0#{req => Req0}};
        {redirect, Route} ->
            Req0 = cowboy_req:set_resp_headers(#{<<"Location">> => list_to_binary(Route)}, Req),
            {stop, State#{resp_status := 302, req := Req0}}
    catch
        Class:Reason ->
            ?ERROR("pre_http_request: ~p:~p failed with ~p/~p", [Module, Function, Class, Reason]),
            {error, Reason}
    end;
pre_http_request(State, _Options) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Will be called on if a websocket route is defined as secure in the same
%% way a http route is. This will check in the upgrade flow if the client
%% is allowed to continue.
%% @end
%%--------------------------------------------------------------------
-spec pre_ws_upgrade(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
                            {ok, State0 :: nova_http_handler:nova_http_state()} |
                            {stop, State0 :: nova_http_handler:nova_http_state()} |
                            {error, Reason :: term()}.
pre_ws_upgrade(State = #{req := Req, secure := {Module, Function}}, _Options) ->
    try Module:Function(Req) of
        {true, AuthData} ->
            {ok, State#{controller_data =>
                            #{auth_data => AuthData}}};
        true ->
            {ok, State};
        false ->
            Req0 = cowboy_req:reply(401, Req),
            {stop, State#{req => Req0}};
        {false, Headers} ->
            Headers0 = cowboy_req:resp_headers(Req),
            Req0 = cowboy_req:set_resp_headers(maps:merge(Headers0, Headers), Req),
            Req1 = cowboy_req:reply(401, Headers, Req0),
            {stop, State#{req => Req1}}
    catch
        Class:Reason ->
            ?ERROR("pre_ws_upgrade: ~p:~p failed with ~p/~p", [Module, Function, Class, Reason]),
            {error, Reason}
    end;
pre_ws_upgrade(State, _Options) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_http_request(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
                               {ok, State0 :: nova_http_handler:nova_http_state()} |
                               {stop, State0 :: nova_http_handler:nova_http_state()} |
                               {error, Reason :: term()}.
post_http_request(State, _Options) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() -> {Title :: binary(),
                        Version :: binary(),
                        Author :: binary(),
                        Description :: binary(),
                        Options :: [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    {<<"Nova Security Plugin">>,
     <<"0.0.1">>,
     <<"Nova Team <info@novaframework.org">>,
     <<"Plugin that handles the security around paths.">>,
    []}.
