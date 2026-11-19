-module(nova_cors_plugin).
-moduledoc false.
-behaviour(nova_plugin).

-export([
         pre_security/4,
         post_request/4,
         plugin_info/0
        ]).

%%--------------------------------------------------------------------
-doc "Pre-request callback".
-spec pre_security(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                          {ok, Req0 :: cowboy_req:req(), NewState :: any()} |
                          {stop, Req0 :: cowboy_req:req(), NewState :: any()}.
pre_security(Req, _Env, #{allow_origins := Origins}, State) ->
    ReqWithOptions = add_cors_headers(Req, Origins),
    continue(ReqWithOptions, State).

%%--------------------------------------------------------------------
-doc "Post-request callback".
-spec post_request(Req :: cowboy_req:req(), Env :: any(), Options :: map(), State :: any()) ->
                               {ok, Req0 :: cowboy_req:req(), NewState :: any()}.
post_request(Req, _Env, _Opts, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
-doc "nova_plugin callback. Returns information about the plugin.".
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

continue(Req, State) ->
    case is_preflight(Req) of
        true -> {stop, cowboy_req:reply(204, Req), State};
        false -> {ok, Req, State}
    end.

is_preflight(#{method := ~"OPTIONS"} = Req) ->
    cowboy_req:header(~"origin", Req) =/= undefined andalso
        cowboy_req:header(~"access-control-request-method", Req) =/= undefined;
is_preflight(_Req) ->
    false.

add_cors_headers(Req, Origins) ->
    OriginsReq = cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Origin">>, Origins, Req),
    HeadersReq = cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Headers">>, <<"*">>, OriginsReq),
    cowboy_req:set_resp_header(
        <<"Access-Control-Allow-Methods">>, <<"*">>, HeadersReq).
