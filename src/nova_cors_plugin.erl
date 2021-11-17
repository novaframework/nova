-module(nova_cors_plugin).
-behaviour(nova_plugin).


-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

pre_request(#{headers := #{<<"origin">> := Origin}, method := <<"OPTIONS">>} = Req, _) ->
    CorsReq = add_cors_headers(Origin, Req),
    Req2 = cowboy_req:reply(200, CorsReq),
    {stop, Req2};
pre_request(#{headers := #{<<"origin">> := Origin}} = Req, _) -> {ok, add_cors_headers(Origin, Req)};
pre_request(Req, _) -> {ok, add_cors_headers(<<"null">>, Req)}.

add_cors_headers(Origin, Req) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"*">>, Req1),
    cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"*">>, Req2).

post_request(Req, _) ->
    {ok, Req}.

plugin_info() ->
    {<<"nova_cors_plugin">>, <<"0.2.0">>, <<"">>, <<"Add CORS headers to request">>, []}.