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
    ListOptions = maps:to_list(Options),
    %% Read the body and put it into the Req object
    BodyReq = case cowboy_req:has_body(Req) of
                   true ->
                        read_body(Req, <<>>);
                   false ->
                        Req#{body => <<>>}
               end,
    modulate_state(BodyReq, ListOptions).

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
      {decode_json_body, <<"Decodes the body as JSON and puts it under `json`">>},
      {parse_qs, <<"Used to parse qs and put hem in state under `qs` key">>}
     ]}.


%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

modulate_state(Req, []) ->
    {ok, Req};

modulate_state( Req = #{method := Method}, [{decode_json_body, true}|Tail]) when Method =:= <<"GET">>; Method =:= <<"DELETE">> ->
    modulate_state(Req, Tail);
modulate_state(Req = #{headers := #{<<"content-type">> := <<"application/json", _/binary>>}, body := Body}, [{decode_json_body, true}|Tl]) ->
    %% Decode the data
    JsonLib = nova:get_env(json_lib, thoas),
    case erlang:apply(JsonLib, decode, [Body]) of
        {ok, JSON} ->
            modulate_state(Req#{json => JSON}, Tl);
        Error ->
            Req400 = cowboy_req:reply(400, Req),
            logger:warning(#{status_code => 400,
                             msg => "Failed to decode json.",
                             error => Error}),
            {stop, Req400}
    end;
modulate_state(#{headers := #{<<"content-type">> := <<"application/x-www-form-urlencoded", _/binary>>}, body := Body} = Req,
               [{read_urlencoded_body, true}|Tl]) ->
    Data = cow_qs:parse_qs(Body),
    %% First read in the body
    Params = maps:from_list(Data),
    modulate_state(Req#{params => Params}, Tl);
modulate_state(Req, [{parse_qs, Type}|T1]) ->
    Qs = cowboy_req:parse_qs(Req),
    case Type of
        true -> MapQs = maps:from_list(Qs),
                modulate_state(Req#{parsed_qs => MapQs}, T1);
        list -> modulate_state(Req#{parsed_qs => Qs}, T1)
    end;
modulate_state(State, [_|Tl]) ->
    modulate_state(State, Tl).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> Req0#{body => <<Acc/binary, Data/binary>>};
        {more, Data, Req0} -> read_body(Req0, <<Acc/binary, Data/binary>>)
    end.
