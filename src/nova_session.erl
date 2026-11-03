%%% Author: Niclas Axelsson <niclas@burbas.se>
-module(nova_session).
-moduledoc """
All kind of operations on sessions is handled by the nova_session-module. The module also presents a behaviour that can be used to create customized backends for the session-data.

The session id lives in the `session_id` cookie (see `session_cookie_name` in
the configuration guide). `nova_stream_h` issues it with the response of the
first request that writes to the session, so visitors that never store
anything get no cookie. A request carrying two or more different session ids
is treated as having no session.

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
without it still stores session data, and `renew/1` and `delete/1` set or
expire the cookie on the returned request themselves, but a `set/3` without a
cookie does not start a session and presented ids are used without the check
above.
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
         cookie_name/0,
         cookie_opts/1,
         presented_session_id/1,
         max_age/0
        ]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_MAX_AGE, 1209600).

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
Deletes the whole session and clears the session cookie. The returned
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
            Req0 = case stream_handled(Req) of
                       true -> Req;
                       false -> set_cookie(<<>>, (cookie_opts(Req))#{max_age => 0}, Req)
                   end,
            {ok, with_session_id(NewSessionId, Req0)};
        _ ->
            %% Session not found
            {ok, Req}
    end.

-doc """
Moves the client to a new, empty session: the current session's data is
deleted and the response carries a new session cookie. Use the returned
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
    Req0 = case stream_handled(Req) of
               true -> Req;
               false -> set_cookie(NewSessionId, cookie_opts(Req), Req)
           end,
    {ok, with_session_id(NewSessionId, Req0)}.

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

-doc false.
-spec cookie_name() -> binary().
cookie_name() ->
    case nova_config:get_env(session_cookie_name, ~"session_id") of
        Name when is_binary(Name), Name =/= <<>> -> Name;
        _ -> ~"session_id"
    end.

-doc false.
-spec max_age() -> pos_integer() | infinity.
max_age() ->
    case nova_config:get_env(session_max_age, ?DEFAULT_MAX_AGE) of
        MaxAge when is_integer(MaxAge), MaxAge > 0 -> MaxAge;
        infinity -> infinity;
        _ -> ?DEFAULT_MAX_AGE
    end.

%% Secure unless the environment is explicitly dev: behind a proxy that
%% terminates TLS the request itself looks like plain http. A __Host- cookie
%% is only stored by browsers when it is Secure, has Path=/ and no Domain.
-doc false.
-spec cookie_opts(cowboy_req:req()) -> cow_cookie:cookie_opts().
cookie_opts(Req) ->
    Secure = application:get_env(nova, environment) =/= {ok, dev}
        orelse maps:get(scheme, Req, ~"http") =:= ~"https",
    Defaults = #{path => ~"/", http_only => true, same_site => lax, secure => Secure},
    Defaults0 = case max_age() of
                    infinity -> Defaults;
                    MaxAge -> Defaults#{max_age => MaxAge}
                end,
    Opts = case nova_config:get_env(session_cookie_opts, #{}) of
               Configured when is_map(Configured) -> merge_cookie_opts(maps:to_list(Configured), Defaults0);
               _ -> Defaults0
           end,
    case cookie_name() of
        <<"__Host-", _/binary>> -> maps:remove(domain, Opts#{secure => true, path => ~"/"});
        _ -> Opts
    end.

merge_cookie_opts([], Opts) ->
    Opts;
merge_cookie_opts([{Key, Value} | Rest], Opts) when (Key =:= path orelse Key =:= domain), is_binary(Value) ->
    merge_cookie_opts(Rest, Opts#{Key => Value});
merge_cookie_opts([{Key, Value} | Rest], Opts) when (Key =:= secure orelse Key =:= http_only), is_boolean(Value) ->
    merge_cookie_opts(Rest, Opts#{Key => Value});
merge_cookie_opts([{max_age, Value} | Rest], Opts) when is_integer(Value), Value >= 0 ->
    merge_cookie_opts(Rest, Opts#{max_age => Value});
merge_cookie_opts([{same_site, Value} | Rest], Opts)
  when Value =:= default; Value =:= none; Value =:= strict; Value =:= lax ->
    merge_cookie_opts(Rest, Opts#{same_site => Value});
merge_cookie_opts([_ | Rest], Opts) ->
    merge_cookie_opts(Rest, Opts).

%% The id the client sent. No cookie, or two or more different ids (a cookie
%% set for a parent domain or another path next to ours), means no session.
-doc false.
-spec presented_session_id(cowboy_req:req()) -> binary() | undefined.
presented_session_id(Req) ->
    Name = cookie_name(),
    try lists:usort([Value || {Key, Value} <- cowboy_req:parse_cookies(Req), Key =:= Name]) of
        [SessionId] when is_binary(SessionId), SessionId =/= <<>> ->
            SessionId;
        [_, _ | _] ->
            warn_ambiguous_cookie(Name),
            undefined;
        _ ->
            undefined
    catch
        _:_ ->
            undefined
    end.

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
                                   backend => Mod, class => Class, reason => nova_redact:term(Reason)}),
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
    case presented_session_id(Req) of
        undefined -> {error, not_found};
        SessionId -> {ok, SessionId}
    end.

with_session_id(SessionId, Req) ->
    (maps:without([nova_session_presented, nova_session_pending], Req))#{nova_session_id => SessionId}.

%% nova_stream_h puts the session ids in the request. Without it the cookie
%% goes on the request's response headers instead.
stream_handled(#{nova_session_direct := true}) ->
    false;
stream_handled(Req) ->
    is_map_key(nova_session_id, Req) orelse is_map_key(nova_session_presented, Req).

set_cookie(Value, Opts, Req) ->
    Req0 = cowboy_req:set_resp_cookie(cookie_name(), Value, Req, Opts),
    Req0#{nova_session_direct => true}.

warn_ambiguous_cookie(Name) ->
    Key = {?MODULE, ambiguous_cookie_warned},
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?LOG_WARNING(#{msg => ~"Request carried more than one session cookie, treated as no session. A cookie with the same name is probably set for a parent domain or path; a __Host- session_cookie_name prevents that",
                           cookie => Name})
    end.

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
