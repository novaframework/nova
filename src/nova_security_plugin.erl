-module(nova_security_plugin).
-behaviour(nova_plugin).

-include_lib("nova/include/nova.hrl").

-export([
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
            {ok, State2} = nova_http_handler:render_page(401, State),
            {stop, State2};
        {redirect, Route} ->
            Req0 = cowboy_req:set_resp_headers(#{<<"Location">> => list_to_binary(Route)}, Req),
            {stop, State#{resp_status := 302, req := Req0}}
    catch
        Class:Reason ->
            Msg = io_lib:format("Security module: ~p:~p failed with ~p/~p", [Module, Function, Class, Reason]),
            ?ERROR(Msg),
            {error, Msg}
    end;
pre_http_request(State, _Options) ->
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
