-module(nova_error_handler).

-behaviour(cowboy_middleware).

-export([
         execute/2
        ]).

-include_lib("nova/include/nova.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public functions        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%% Callback function from cowboy. If we reach this segment we will return
%% a 404 since all the other handlers have failed.
%% @end
%%--------------------------------------------------------------------
execute(Req, Env) ->
    logger:info("Reached error handler: Req: ~p~nEnv: ~p", [Req, Env]),
    %%Req1 = nova_router:get_status_page(404, Req),
    {stop, Req}.
