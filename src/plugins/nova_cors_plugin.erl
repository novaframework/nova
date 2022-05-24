-module(nova_cors_plugin).
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
    ParsedOptions = nova_plugin_utilities:parse_options(Options),
    ReqWithOptions = set_headers(Req, ParsedOptions),
    continue(ReqWithOptions).

%%--------------------------------------------------------------------
%% @doc
%% Post-request callback
%% @end
%%--------------------------------------------------------------------
-spec post_request(Req :: cowboy_req:req(), Options :: map()) ->
                               {ok, Req0 :: cowboy_req:req()}.
post_request(Req, _) ->
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
    {
     <<"nova_cors_plugin">>,
     <<"0.2.0">>,
     <<"Nova team <info@novaframework.org">>,
     <<"Add CORS headers to request">>,
     [
      {
       allow_origins,
       <<"Specifies which origins to insert into Access-Control-Allow-Origin">>
      }
    ]}.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

continue(#{method := <<"OPTIONS">>} = Req) ->
    Reply = cowboy_req:reply(200, Req),
    {stop, Reply};
continue(Req) ->
    {ok, Req}.

set_headers(Req, []) -> Req;
set_headers(Req, [{allow_origins, Origins}|T]) ->
    CorsReq = add_cors_headers(Req, Origins),
    set_headers(CorsReq, T);
set_headers(Req, [_H|T]) ->
    set_headers(Req, T).

add_cors_headers(Req, Origins) ->
    OriginsReq = cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Origin">>, Origins, Req),
    HeadersReq = cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Headers">>, <<"*">>, OriginsReq),
    cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Methods">>, <<"*">>, HeadersReq).
