-module(nova_cors_plugin).
-behaviour(nova_plugin).


-export([pre_http_request/2,
         post_http_request/2,
         plugin_info/0]).

pre_http_request(#{req := Req} = NovaState, _) ->
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"*">>, Req2),
    NewState = maps:put(req, Req3, NovaState),
    case Req of
        #{method := <<"OPTIONS">>} ->
            {stop, NewState};
        #{method := _} ->
            {ok, maps:put(req, Req1, NovaState)}
    end.

post_http_request(NovaState, _) ->
    {ok, NovaState}.

plugin_info() ->
    {<<"nova_cors_plugin">>, <<"0.1.0">>, <<"">>, <<"Add CORS headers to request">>, []}.
