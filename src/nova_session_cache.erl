%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2021, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(nova_session_cache).

%% API
-export([
         init/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2
        ]).

-include_lib("nova/include/nova.hrl").

-define(SERVER, ?MODULE).
-define(CACHE_NAME, ?MODULE).
-define(CACHE_TTL, application:get_env(nova, session_ttl, 3600000)).
-define(TOPIC, "nova_session").

%%%===================================================================
%%% API
%%%===================================================================

init() ->
    {ok, _Pid} = nova_cache:init_cache(?CACHE_NAME),
    CB = fun(Msg) ->
                 parse_msg(Msg)
         end,
    Handler = ebus_proc:spawn_handler(CB),
    ebus:sub(Handler, "nova_session").


-spec get_value(SessionId :: binary(), Key :: binary()) -> {ok, Value :: binary()} | {error, not_found}.
get_value(SessionId, Key) ->
    nova_cache:get(?CACHE_NAME, {SessionId, Key}).

-spec set_value(SessionId :: binary(), Key :: binary(), Value :: binary()) -> ok | {error, Reason :: term()}.
set_value(SessionId, Key, Value) ->
    ebus:pub(?TOPIC, {set, SessionId, Key, Value}).

-spec delete_value(SessionId :: binary()) -> ok | {error, Reason :: term()}.
delete_value(_SessionId) ->
    ok.

-spec delete_value(SessionId :: binary(), Key :: binary()) -> ok | {error, Reason :: term()}.
delete_value(SessionId, Key) ->
    ebus:pub(?TOPIC, {delete, SessionId, Key}).

parse_msg({set, SessionId, Key, Value}) ->
    nova_cache:set(?CACHE_NAME, {SessionId, Key}, Value, ?CACHE_TTL);
parse_msg({delete, SessionId, Key}) ->
    nova_cache:delete(?CACHE_NAME, {SessionId, Key});
parse_msg(Msg) ->
    ?WARNING("Session cache received invalid message: ~p", [Msg]).
