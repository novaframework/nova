-module(nova_request_plugin).
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
pre_request(State, Options) ->
    Options0 = [ K || {K, V} <- maps:to_list(Options),
                      V == true ],
    {ok, State0} = modulate_state(State, Options0),
    {ok, State0}.


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
-spec plugin_info() -> {Title :: binary(), Version :: binary(), Author :: binary(), Description :: binary()}.
plugin_info() ->
    {<<"Nova body plugin">>,
     <<"0.0.1">>,
     <<"Nova team <info@novaframework.org">>,
     <<"This plugin modulates the body of a request.">>,
     [
      {parse_bindings, <<"Used to parse bindings and put them in state under `bindings` key">>},
      {read_body, <<"Reads the body and put it under the `body`">>},
      {decode_json_body, <<"Decodes the body as JSON and puts it under `json`">>}
     ]}.


modulate_state(State, []) -> {ok, State};
modulate_state(State = #{req := Req}, [parse_bindings|Tl]) ->
    Bindings = cowboy_req:bindings(Req),
    modulate_state(State#{bindings => Bindings}, Tl);
modulate_state(State = #{req :=  Req = #{headers := #{<<"content-type">> := <<"application/json">>}}}, [decode_json_body|Tl]) ->
    %% First read in the body
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    %% Decode the data
    JSON = json:decode(Data, [maps, binary]),
    modulate_state(State#{req => Req0, json => JSON}, Tl);
modulate_state(State = #{req := Req}, [read_body|Tl]) ->
    %% Fetch the body
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    modulate_state(State#{req => Req0, body => Data}, Tl);
modulate_state(State, [_|Tl]) ->
    modulate_state(State, Tl).
