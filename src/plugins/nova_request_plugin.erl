-module(nova_request_plugin).
-behaviour(nova_plugin).

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
-spec pre_request(Req :: cowboy_req:req(), Options :: map()) ->
                         {ok, Req0 :: cowboy_req:req()}.
pre_request(Req, Options) ->
    Options0 = nova_plugin_utilities:parse_options(Options),
    {ok, _Req0} = modulate_state(Req, Options0).

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Options :: map()) ->
                               {ok, Req0 :: cowboy_req:req()}.
post_request(Req, _Options) ->
    {ok, Req}.


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
    {<<"Nova body plugin">>,
     <<"0.0.1">>,
     <<"Nova team <info@novaframework.org">>,
     <<"This plugin modulates the body of a request.">>,
     [
      {parse_bindings, <<"Used to parse bindings and put them in state under `bindings` key">>},
      {read_body, <<"Reads the body and put it under the `body`">>},
      {decode_json_body, <<"Decodes the body as JSON and puts it under `json`">>},
      {parse_qs, <<"Used to parse qs and put hem in state under `qs` key">>}
     ]}.


%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

modulate_state(Req, []) ->
    {ok, Req};

modulate_state(Req = #{headers := #{<<"content-type">> := <<"application/json", _/binary>>}}, [decode_json_body|Tl]) ->
    case cowboy_req:has_body(Req) of
        true ->
            %% First read in the body
            {ok, Data, Req0} = cowboy_req:read_body(Req),
            %% Decode the data
            JsonLib = nova:get_env(json_lib, thoas),
            {ok, JSON} = erlang:apply(JsonLib, decode, [Data]),
            modulate_state(Req0#{json => JSON}, Tl);
        false ->
            modulate_state(Req#{json => #{}}, Tl)
    end;
modulate_state(#{headers := #{<<"content-type">> := <<"application/x-www-form-urlencoded", _/binary>>}} = Req,
               [read_urlencoded_body|Tl]) ->
    case cowboy_req:has_body(Req) of
        true ->
            %% First read in the body
            {ok, Data, Req0} = cowboy_req:read_urlencoded_body(Req),
            Params = maps:from_list(Data),
            modulate_state(Req0#{params => Params}, Tl);
        false ->
            modulate_state(Req#{params => #{}}, Tl)
    end;
modulate_state(Req, [parse_qs|T1]) ->
    Qs = cowboy_req:parse_qs(Req),
    MapQs = maps:from_list(Qs),
    modulate_state(Req#{parsed_qs => MapQs}, T1);
modulate_state(Req, [read_body|Tl]) ->
    %% Fetch the body
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    modulate_state(Req0#{body => Data}, Tl);
modulate_state(State, [_|Tl]) ->
    modulate_state(State, Tl).
