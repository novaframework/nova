-module({{module}}).
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
-spec pre_request(Req :: cowboy_req:req(), State :: nova_http_handler:nova_http_state()) ->
                         {ok, Req0 :: cowboy_req:req(), State0 :: nova_http_handler:nova_http_state()} |
                         {stop, Req0 :: cowboy_req:req(), State0 :: nova_http_handler:nova_http_state()} |
                         {error, Reason :: term()}.
pre_request(Req, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), State :: nova_http_handler:nova_http_state()) ->
                          {ok, Req0 :: cowboy_req:req(), State0 :: nova_http_handler:nova_http_state()} |
                          {stop, Req0 :: cowboy_req:req(), State0 :: nova_http_handler:nova_http_state()} |
                          {error, Reason :: term()}.
post_request(Req, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() -> {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary()}.
plugin_info() ->
    {<<"{{module}} plugin">>,
     <<"0.0.1">>,
     <<"User <user@email.com">>,
     <<"Descriptive text">>}.
