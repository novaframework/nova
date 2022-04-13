%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% All kind of operations on sessions is handled by the nova_session-module. The module also
%%% presents a behaviour that can be used to create customized backends for the session-data.
%%%
%%% @end
-module(nova_session).
-export([
         get/2,
         set/3,
         delete/1,
         delete/2,
         generate_session_id/0
        ]).

-include("../include/nova_logger.hrl").

%%%===================================================================
%%% Callbacks
%%%===================================================================

%% Get a value from a given session_id
-callback get_value(SessionId, Key) ->
    {ok, Value :: any()} |
    {error, Reason :: atom()}
        when SessionId :: binary(),
             Key :: binary().

%% Set a value
-callback set_value(SessionId, Key, Value) ->
    ok |
    {error, Reason :: atom()}
    when SessionId :: binary(),
         Key :: binary(),
         Value :: binary().

%% Deletes a whole session
-callback delete_value(SessionId) ->
    ok |
    {error, Reason :: atom()}
        when SessionId :: binary().

%% Deletes a specific key of a session
-callback delete_value(SessionId, Key) ->
    ok |
    {error, Reason :: atom()}
        when SessionId :: binary(),
             Key :: binary().

%%%===================================================================
%%% Public functions
%%%===================================================================
-spec get(Req :: cowboy_req:req(), Key :: binary()) ->
                 {ok, Value :: binary()} | {error, Reason :: atom()} | no_return().
get(Req, Key) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            erlang:apply(Mod, get_value, [SessionId, Key]);
        _ ->
            {error, not_found}
    end.

-spec set(Req :: cowboy_req:req(), Key :: binary(), Value :: binary()) ->
                 ok | {error, Reason :: atom()} | no_return().
set(Req, Key, Value) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            erlang:apply(Mod, set_value, [SessionId, Key, Value]);
        _ ->
            {error, session_id_not_set}
    end.

-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} |
                                                                      {error, Reason :: atom()}.
delete(Req) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            erlang:apply(Mod, delete_value, [SessionId]),
            Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req,
                                              #{max_age => 0}),
            {ok, Req1};
        _ ->
            %% Session not found
            {ok, Req}
    end.

-spec delete(Req :: cowboy_req:req(), Key :: binary()) -> {ok, Req :: cowboy_req:req()} |
                                                          {error, Reason :: atom()} | no_return().
delete(Req, Key) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            erlang:apply(Mod, delete_value, [SessionId, Key]),
            {ok, Req};
        _ ->
            %% Session not found
            {ok, Req}
    end.


%%%===================================================================
%%% Private functions
%%%===================================================================
get_session_module() ->
    application:get_env(nova, session_manager, nova_session_ets).

get_session_id(Req) ->
    case nova:get_env(use_sessions, true) of
        true ->
            #{session_id := SessionId} = cowboy_req:match_cookies([{session_id, [], undefined}], Req),
            case SessionId of
                undefined ->
                    {error, not_found};
                _ ->
                    {ok, SessionId}
            end;
        _ ->
            ?LOG_ERROR(#{msg => "Session called but 'use_session' option is set to false"}),
            throw({nova_session, unsupported_session_used})
    end.

generate_session_id() ->
    SessionId =
        << <<X:8/unsigned-integer>> ||
            X <- [ rand:uniform(255) || _ <- lists:seq(0, 31) ] >>,
    {ok, base64:encode(SessionId)}.
