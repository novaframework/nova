-module(nova_cors_plugin).
-behaviour(nova_plugin).


-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

pre_request(Req, _) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"*">>, Req2),
    case Req3 of
        #{method := <<"OPTIONS">>} ->
            {stop, Req3};
        #{method := _} ->
            {ok, Req3}
    end.

post_request(Req, _) ->
    {ok, Req}.

plugin_info() ->
    {<<"nova_cors_plugin">>, <<"0.1.0">>, <<"">>, <<"Add CORS headers to request">>, []}.
