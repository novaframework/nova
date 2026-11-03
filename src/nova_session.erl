%%% Author: Niclas Axelsson <niclas@burbas.se>
-module(nova_session).
-moduledoc """
All kind of operations on sessions is handled by the nova_session-module. The module also presents a behaviour that can be used to create customized backends for the session-data.

The session id lives in the `session_id` cookie. `nova_stream_h` issues it with
the response of the first request that writes to the session, so visitors that
never store anything get no cookie. A presented id the backend does not know is
ignored and replaced, so a client cannot choose its own session id.

Call `renew/1` whenever the privilege level of the client changes (sign-in,
sign-out, password change) so a session id planted or observed before that
point stops working.
""".
-export([
         get/2,
         set/3,
         delete/1,
         delete/2,
         renew/1,
         generate_session_id/0
        ]).

%% Used by nova_stream_h
-export([
         enabled/0,
         known/1
        ]).

-include_lib("kernel/include/logger.hrl").

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

%% Start a server process for session backend, if necessary
-callback start_link() ->
    {ok, Pid :: pid()} |
    {error, Error :: {already_started, pid()}} |
    {error, Error :: term()} |
    ignore.

-doc """
Tells whether the backend holds a session with this id. Nova calls it for the
id a client presents and replaces ids that return `false`. A backend without
it accepts every presented id, which leaves it open to session fixation.
""".
-callback session_exists(SessionId :: binary()) -> boolean().

-optional_callbacks([start_link/0, session_exists/1]).

%%%===================================================================
%%% Public functions
%%%===================================================================
-spec get(Req :: cowboy_req:req(), Key :: binary()) ->
          {ok, Value :: binary()} | {error, Reason :: atom()} | no_return().
get(Req, Key) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:get_value(SessionId, Key);
        _ ->
            {error, not_found}
    end.

-spec set(Req :: cowboy_req:req(), Key :: binary(), Value :: binary()) ->
          ok | {error, Reason :: atom()} | no_return().
set(Req, Key, Value) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            case Mod:set_value(SessionId, Key, Value) of
                ok ->
                    notify(Req, {written, SessionId}),
                    ok;
                Error ->
                    Error
            end;
        _ ->
            {error, session_id_not_set}
    end.

-doc """
Deletes the whole session and clears the `session_id` cookie. The returned
request carries a fresh session id, so a `set/3` on it later in the same
request starts a new session instead of reviving the deleted one.
""".
-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} |
          {error, Reason :: atom()}.
delete(Req) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId),
            {ok, NewSessionId} = generate_session_id(),
            notify(Req, {deleted, NewSessionId}),
            {ok, Req#{nova_session_id => NewSessionId}};
        _ ->
            %% Session not found
            {ok, Req}
    end.

-doc """
Moves the client to a new, empty session: the current session's data is
deleted and the response carries a new `session_id` cookie. Use the returned
request for the `set/3` calls that follow, for example:

```erlang
login(Req) ->
    {ok, Req1} = nova_session:renew(Req),
    ok = nova_session:set(Req1, ~"user_id", UserId),
    {redirect, "/"}.
```
""".
-spec renew(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} | no_return().
renew(Req) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId);
        _ ->
            ok
    end,
    {ok, NewSessionId} = generate_session_id(),
    notify(Req, {renewed, NewSessionId}),
    {ok, Req#{nova_session_id => NewSessionId}}.

-spec delete(Req :: cowboy_req:req(), Key :: binary()) -> {ok, Req :: cowboy_req:req()} |
          {error, Reason :: atom()} | no_return().
delete(Req, Key) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId, Key),
            {ok, Req};
        _ ->
            %% Session not found
            {ok, Req}
    end.


%%%===================================================================
%%% Functions used by nova_stream_h
%%%===================================================================
-doc false.
-spec enabled() -> boolean().
enabled() ->
    case application:get_env(nova, use_sessions) of
        {ok, UseSessions} ->
            UseSessions =:= true;
        undefined ->
            nova:get_env(use_sessions, true) =:= true
    end.

-doc false.
-spec known(SessionId :: binary()) -> boolean().
known(SessionId) ->
    Mod = get_session_module(),
    _ = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, session_exists, 1) of
        true ->
            try Mod:session_exists(SessionId) =:= true
            catch
                Class:Reason ->
                    ?LOG_WARNING(#{msg => ~"Session backend failed to look up a session",
                                   backend => Mod, class => Class, reason => Reason}),
                    false
            end;
        false ->
            true
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================
get_session_module() ->
    application:get_env(nova, session_manager, nova_session_ets).

get_session_id(Req) ->
    case enabled() of
        true ->
            case maps:get(nova_session_id, Req, undefined) of
                undefined ->
                    #{session_id := SessionId} = cowboy_req:match_cookies([{session_id, [], undefined}], Req),
                    case SessionId of
                        undefined ->
                            {error, not_found};
                        _ ->
                            {ok, SessionId}
                    end;
                SessionId ->
                    {ok, SessionId}
            end;
        _ ->
            ?LOG_ERROR(#{msg => <<"Session called but 'use_session' option is set to false">>}),
            throw({nova_session, unsupported_session_used})
    end.

%% Tells nova_stream_h which session id the response has to hand out. The
%% stream handler runs in the connection process; a websocket over HTTP/1.1
%% runs there too, and must not be sent the message.
notify(#{pid := Pid, streamid := StreamID}, Event) when is_pid(Pid), Pid =/= self() ->
    Pid ! {{Pid, StreamID}, {nova_session, Event}},
    ok;
notify(_Req, _Event) ->
    ok.

-spec generate_session_id() -> {ok, binary()}.
generate_session_id() ->
    {ok, base64:encode(crypto:strong_rand_bytes(32))}.
