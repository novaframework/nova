-module(nova_cors_plugin).
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
pre_request(Req, _Env, #{allow_origins := Origins}, State) ->
    ReqWithOptions = add_cors_headers(Req, Origins),
    continue(ReqWithOptions, State).

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                               {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Opts, State) ->
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
    #{
      title => <<"nova_cors_plugin">>,
      version => <<"0.2.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Add CORS headers to request">>,
      options => [
                  {allow_origins, <<"Specifies which origins to insert into Access-Control-Allow-Origin">>}
                 ]
     }.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

continue(#{method := <<"OPTIONS">>} = Req, State) ->
    Reply = cowboy_req:reply(200, Req),
    {stop, Reply, State};
continue(Req, State) ->
    {ok, Req, State}.

add_cors_headers(Req, Origins) ->
    OriginsReq = cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Origin">>, Origins, Req),
    HeadersReq = cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Headers">>, <<"*">>, OriginsReq),
    cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Methods">>, <<"*">>, HeadersReq).
