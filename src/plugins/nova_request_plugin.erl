-module(nova_request_plugin).
-behaviour(nova_plugin).

-export([
         pre_request/4,
         post_request/4,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback
%% @end
%%--------------------------------------------------------------------
-spec pre_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                         {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_request(Req, _Env, Options, State) ->
    ListOptions = maps:to_list(Options),
    %% Read the body and put it into the Req object
    BodyReq = case should_read_body(ListOptions) andalso
                  cowboy_req:has_body(Req) of
                  true ->
                      read_body(Req, <<>>);
                  false ->
                      Req#{body => <<>>}
              end,
    modulate_state(BodyReq, ListOptions, State).

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
          {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% nova_plugin callback. Returns information about the plugin.
%% @end
%%--------------------------------------------------------------------
-spec plugin_info() -> #{title := binary(),
                         version := binary(),
                         url := binary(),
                         authors := [binary()],
                         description := binary(),
                         options := [{Key :: atom(), OptionDescription :: binary()}]}.
plugin_info() ->
    #{title => <<"Nova body plugin">>,
      version => <<"0.0.1">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"This plugin modulates the body of a request.">>,
      options => [
                  {decode_json_body, <<"Decodes the body as JSON and puts it under `json`">>},
                  {read_urlencoded_body, <<"Used to parse body as query-string and put them in state under `qs` key">>}
                 ]
     }.


%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

modulate_state(Req, [], State) ->
    {ok, Req, State};

modulate_state( Req = #{method := Method}, [{decode_json_body, true}|Tail], State) when Method =:= <<"GET">>; Method =:= <<"DELETE">> ->
    modulate_state(Req, Tail, State);
modulate_state(Req = #{headers := #{<<"content-type">> := <<"application/json", _/binary>>}, body := <<>>}, [{decode_json_body, true}|_Tl], State) ->
    Req400 = cowboy_req:reply(400, Req),
    logger:warning(#{status_code => 400,
                     msg => "Failed to decode json.",
                     error => "No body to decode."}),
    {stop, Req400, State};
modulate_state(Req = #{headers := #{<<"content-type">> := <<"application/json", _/binary>>}, body := Body}, [{decode_json_body, true}|Tl], State) ->
    %% Decode the data
    JsonLib = nova:get_env(json_lib, thoas),
    case JsonLib:decode(Body) of
        {ok, JSON} ->
            modulate_state(Req#{json => JSON}, Tl, State);
        Error ->
            Req400 = cowboy_req:reply(400, Req),
            logger:warning(#{status_code => 400,
                             msg => "Failed to decode json.",
                             error => Error}),
            {stop, Req400, State}
    end;
modulate_state(#{headers := #{<<"content-type">> := <<"application/x-www-form-urlencoded", _/binary>>}, body := Body} = Req,
               [{read_urlencoded_body, true}|Tl], State) ->
    Data = cow_qs:parse_qs(Body),
    %% First read in the body
    Params = maps:from_list(Data),
    modulate_state(Req#{params => Params}, Tl, State);
modulate_state(Req, [{parse_qs, Type}|T1], State) ->
    Qs = cowboy_req:parse_qs(Req),
    case Type of
        true -> MapQs = maps:from_list(Qs),
                modulate_state(Req#{parsed_qs => MapQs}, T1, State);
        list -> modulate_state(Req#{parsed_qs => Qs}, T1, State)
    end;
modulate_state(Req, [_|Tl], State) ->
    modulate_state(Req, Tl, State).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> Req0#{body => <<Acc/binary, Data/binary>>};
        {more, Data, Req0} -> read_body(Req0, <<Acc/binary, Data/binary>>)
    end.

should_read_body([]) -> false;
should_read_body([{decode_json_body, true}|_Tl]) -> true;
should_read_body([{read_urlencoded_body, true}|_Tl]) -> true;
should_read_body([_|Tl]) -> should_read_body(Tl).

-ifdef(TEST).
-compile(export_all).
-endif.
