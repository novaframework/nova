-module(nova_basic_pre_handler).
-export([
         inject_correlation_id/1
        ]).

-include_lib("nova/include/nova.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Injects a corrrelation id into the headers
%% @end
%%--------------------------------------------------------------------
inject_correlation_id(Req) ->
    UUID = uuid:get_v4(),
    {ok, cowboy_req:set_resp_header(<<"X-Correlation-ID">>, UUID, Req)}.
