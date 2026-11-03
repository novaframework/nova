%%% Author: Niclas Axelsson <niclas@burbas.se>
-module(nova_session).
-moduledoc """
All kind of operations on sessions is handled by the nova_session-module. The module also presents a behaviour that can be used to create customized backends for the session-data.

The session id lives in the `session_id` cookie. `nova_stream_h` issues it with
the response of the first request that writes to the session, so visitors that
never store anything get no cookie.

Call `renew/1` whenever the privilege level of the client changes (sign-in,
sign-out, password change). That is the defence against session fixation: a
session id planted or observed before that point stops working. As defence in
depth, a presented id the backend does not know is ignored and replaced, so a
client cannot simply choose its own session id.

Session changes reach the cookie only when they are made from the process
serving the request (where controllers, plugins and `nova_websocket:init/1`
run). A process spawned with the request can read and write the session data,
but the response will not carry a new cookie for it.

Cookies are written by `nova_stream_h`. A `stream_handlers` configuration
without it still stores session data, but `renew/1`, `delete/1` and new
sessions no longer reach the client's cookie, and presented ids are used
without the check above.
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
         enabled/0
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
id a client presents and replaces ids that return `false`. It runs in the
process calling `nova_session`, once for every `get/2`, `set/3` and `delete`
on a request that presented a cookie, so keep it as cheap as a key lookup. A
backend without it accepts every presented id.
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
                {error, _} = Error ->
                    Error;
                Result ->
                    notify(Req, {written, SessionId}),
                    Result
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
            {ok, with_session_id(NewSessionId, Req)};
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
    {ok, with_session_id(NewSessionId, Req)}.

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
    nova_config:get_env(use_sessions, true) =:= true.

%%%===================================================================
%%% Private functions
%%%===================================================================
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

get_session_module() ->
    application:get_env(nova, session_manager, nova_session_ets).

get_session_id(Req) ->
    case enabled() of
        true ->
            session_id(Req);
        _ ->
            ?LOG_ERROR(#{msg => <<"Session called but 'use_session' option is set to false">>}),
            throw({nova_session, unsupported_session_used})
    end.

%% A new visitor's request carries its id in nova_session_id. When the client
%% sent a cookie, nova_stream_h hands over the presented id and a fresh one,
%% and the presented one is used only while the backend knows it, so a client
%% cannot choose its own id. The check runs here, in the process calling
%% nova_session, and not in the connection process.
session_id(#{nova_session_id := SessionId}) ->
    {ok, SessionId};
session_id(#{nova_session_presented := Presented, nova_session_pending := Pending}) ->
    case known(Presented) of
        true -> {ok, Presented};
        false -> {ok, Pending}
    end;
session_id(Req) ->
    %% Without nova_stream_h there is only the cookie.
    case cowboy_req:match_cookies([{session_id, [], undefined}], Req) of
        #{session_id := SessionId} when is_binary(SessionId) -> {ok, SessionId};
        _ -> {error, not_found}
    end.

with_session_id(SessionId, Req) ->
    (maps:without([nova_session_presented, nova_session_pending], Req))#{nova_session_id => SessionId}.

%% Tells nova_stream_h which session id the response has to hand out. Only
%% the process serving the request (stamped by nova_router) does: a process
%% spawned with the request, or a websocket running in the connection
%% process after the upgrade, would deliver the message to whatever handles
%% the connection by then.
notify(#{pid := Pid, streamid := StreamID, nova_session_owner := Owner}, Event) when Owner =:= self() ->
    Pid ! {{Pid, StreamID}, {nova_session, Event}},
    ok;
notify(_Req, _Event) ->
    ok.

-spec generate_session_id() -> {ok, binary()}.
generate_session_id() ->
    {ok, base64:encode(crypto:strong_rand_bytes(32))}.
