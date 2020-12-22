-module({{pluginname}}_plugin).
-behaviour(nova_plugin).

-include_lib("nova/include/nova.hrl").

-export([
         pre_request/2,
         post_request/2,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
-spec pre_request(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
                         {ok, State0 :: nova_http_handler:nova_http_state()} |
                         {stop, State0 :: nova_http_handler:nova_http_state()} |
                         {error, Reason :: term()}.
pre_request(State, _Options) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(State :: nova_http_handler:nova_http_state(), Options :: map()) ->
                          {ok, State0 :: nova_http_handler:nova_http_state()} |
                          {stop, State0 :: nova_http_handler:nova_http_state()} |
                          {error, Reason :: term()}.
post_request(State, _Options) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() -> {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary(),
                       [{Key :: atom(), OptionDescription :: atom()}]}.
plugin_info() ->
    {<<"{{pluginname}} plugin">>,
     <<"0.0.1">>,
     <<"User <user@email.com">>,
     <<"Descriptive text">>,
     []}. %% Options is specified as {Key, Description}
