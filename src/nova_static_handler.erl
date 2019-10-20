-module(nova_static_handler).

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
execute(Req, Env = #{handler := cowboy_static, handler_opts := HandlerOpts}) ->
    {Mod, Req1, State} = cowboy_static:init(Req, HandlerOpts),
    Mod:upgrade(Req1, Env, cowboy_static, State);
execute(Req, Env) ->
    {ok, Req, Env}.
