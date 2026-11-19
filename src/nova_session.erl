%%% Author: Niclas Axelsson <niclas@burbas.se>
-module(nova_session).
-moduledoc "All kind of operations on sessions is handled by the nova_session-module. The module also presents a behaviour that can be used to create customized backends for the session-data.".
-export([
         get/2,
         set/3,
         delete/1,
         delete/2,
         renew/1,
         generate_session_id/0
        ]).

-export([
         enabled/0,
         cookie_name/0,
         needs_https/0,
         cookie_opts/1,
         validate_cookie_config/0,
         valid_cookie/2,
         presented_session_id/1,
         max_age/0
        ]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_MAX_AGE, 1209600).
-define(KNOWN, {?MODULE, known}).
-define(KNOWN_FOR, 1000).

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
process calling `nova_session`, which keeps the answer for a second for
reads: a request that presented a cookie asks once however often it reads
its session, and again for every write and after it deleted or renewed it.
A backend without it accepts every presented id.
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
    case get_session_id(Req, write) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            written(Mod:set_value(SessionId, Key, Value), SessionId, Req);
        _ ->
            {error, session_id_not_set}
    end.

written({error, _} = Error, _SessionId, _Req) ->
    Error;
written(Result, SessionId, Req) ->
    notify(Req, {written, SessionId}),
    Result.

-doc """
Signs a user out: deletes the whole session and clears the session cookie.
The returned request carries a fresh session id, and is the one to use for
what follows in the same request: a `set/3` on it starts a new session. A
`set/3` on the request that was passed in writes under the id that was just
deleted, which the client no longer holds.

```erlang
logout(Req) ->
    {ok, SignedOut} = nova_session:delete(Req),
    {redirect, "/", SignedOut}.
```
""".
-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} | no_return().
delete(Req) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId),
            ended(SessionId),
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
deleted and the response carries a new session cookie. It is the lower-level
piece of `sign_in/3`, which is what to call where a user signs in. Call
`renew/1` itself for another change of what the client may do, such as a
second factor or a new password, and use the returned request for the
`set/3` calls that follow:

```erlang
second_factor_passed(Req, UserId) ->
    {ok, Renewed} = nova_session:renew(Req),
    ok = nova_session:set(Renewed, ~"user_id", UserId),
    ok = nova_session:set(Renewed, ~"second_factor", true),
    {redirect, "/"}.
```
""".
-spec renew(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} | no_return().
renew(Req) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId),
            ended(SessionId);
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
    case get_session_id(Req, write) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId, Key),
            {ok, Req};
        _ ->
            %% Session not found
            {ok, Req}
    end.


-doc false.
-spec enabled() -> boolean().
enabled() ->
    nova_config:get_env(use_sessions, true) =:= true.

-doc false.
-spec cookie_name() -> binary().
cookie_name() ->
    case nova_config:get_env(session_cookie_name, undefined) of
        Name when is_binary(Name), Name =/= <<>> -> Name;
        _ -> default_cookie_name()
    end.

-spec default_cookie_name() -> binary().
default_cookie_name() ->
    case nova:is_dev_environment() of
        true -> ~"session_id";
        false -> ~"__Host-session_id"
    end.

-doc false.
-spec max_age() -> pos_integer() | infinity.
max_age() ->
    case nova_config:get_env(session_max_age, ?DEFAULT_MAX_AGE) of
        MaxAge when is_integer(MaxAge), MaxAge > 0 -> MaxAge;
        infinity -> infinity;
        _ -> ?DEFAULT_MAX_AGE
    end.

-doc false.
-spec needs_https() -> boolean().
needs_https() ->
    enabled() andalso maps:get(secure, cookie_opts_for(~"http"), false).

-doc false.
-spec cookie_opts(cowboy_req:req()) -> cow_cookie:cookie_opts().
cookie_opts(Req) ->
    cookie_opts_for(maps:get(scheme, Req, ~"http")).

-spec cookie_opts_for(binary()) -> cow_cookie:cookie_opts().
cookie_opts_for(Scheme) ->
    Secure = not nova:is_dev_environment() orelse Scheme =:= ~"https",
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

-doc false.
-spec validate_cookie_config() -> ok.
validate_cookie_config() ->
    ok = check_opts(),
    ok = check_host_prefix(),
    Name = nova_config:get_env(session_cookie_name, default_cookie_name()),
    Opts = cookie_opts_for(~"http"),
    Valid = is_binary(Name) andalso valid_cookie(Name, Opts) andalso
        try cow_cookie:setcookie(Name, ~"x", Opts) of
            _ -> true
        catch
            error:_ -> false
        end,
    case Valid of
        true -> check_secure(Name, Opts);
        false -> erlang:error(invalid_cookie_config(Name, Opts))
    end.

-spec check_opts() -> ok.
check_opts() ->
    case nova_config:get_env(session_cookie_opts, #{}) of
        #{} = Configured ->
            refuse_dropped(dropped_opts(Configured));
        _ ->
            ?LOG_ERROR(#{msg => ~"session_cookie_opts is not a map",
                         setting => session_cookie_opts}),
            erlang:error({invalid_setting, session_cookie_opts, not_a_map})
    end.

-spec dropped_opts(map()) -> [term()].
dropped_opts(Configured) ->
    lists:sort([Key || Key := Value <- Configured, merge_cookie_opts([{Key, Value}], #{}) =:= #{}]).

-spec refuse_dropped([term()]) -> ok.
refuse_dropped([]) ->
    ok;
refuse_dropped(Options) ->
    ?LOG_ERROR(#{msg => ~"session_cookie_opts has options the session cookie does not take",
                 setting => session_cookie_opts, options => Options}),
    erlang:error({invalid_setting, session_cookie_opts, {not_taken, Options}}).

-spec check_host_prefix() -> ok.
check_host_prefix() ->
    Named = nova_config:get_env(session_cookie_name, undefined) =/= undefined,
    case Named orelse nova:is_dev_environment() of
        true -> ok;
        false -> refuse_prefix_conflicts(prefix_conflicts())
    end.

-spec refuse_prefix_conflicts([domain | path | secure]) -> ok.
refuse_prefix_conflicts([]) ->
    ok;
refuse_prefix_conflicts(Options) ->
    ?LOG_ERROR(#{msg => ~"session_cookie_opts conflicts with the __Host- cookie prefix: set session_cookie_name or drop the options",
                 setting => session_cookie_opts, options => Options, requires => session_cookie_name}),
    erlang:error({invalid_setting, session_cookie_opts, {requires, session_cookie_name}}).

-spec prefix_conflicts() -> [domain | path | secure].
prefix_conflicts() ->
    case nova_config:get_env(session_cookie_opts, #{}) of
        #{} = Configured -> [Key || Key <- [domain, path, secure], prefix_conflict(Key, Configured)];
        _ -> []
    end.

-spec prefix_conflict(domain | path | secure, map()) -> boolean().
prefix_conflict(domain, Configured) -> is_map_key(domain, Configured);
prefix_conflict(path, Configured) -> maps:get(path, Configured, ~"/") =/= ~"/";
prefix_conflict(secure, Configured) -> maps:get(secure, Configured, true) =:= false.

-spec check_secure(term(), cow_cookie:cookie_opts()) -> ok.
check_secure(<<"__Secure-", _/binary>>, #{secure := false}) ->
    ?LOG_ERROR(#{msg => ~"A __Secure- session_cookie_name needs secure => true in session_cookie_opts",
                 setting => session_cookie_name, requires => secure}),
    erlang:error({invalid_setting, session_cookie_name, {requires, secure}});
check_secure(_Name, #{same_site := none, secure := false}) ->
    ?LOG_ERROR(#{msg => ~"same_site => none needs secure => true in session_cookie_opts",
                 setting => session_cookie_opts, option => same_site, requires => secure}),
    erlang:error({invalid_setting, session_cookie_opts, {requires, secure}});
check_secure(_Name, _Opts) ->
    ok.

-doc false.
-spec valid_cookie(term(), cow_cookie:cookie_opts()) -> boolean().
valid_cookie(Name, Opts) ->
    Name =/= <<>> andalso is_token(Name) andalso
        is_av_value(maps:get(path, Opts, <<>>)) andalso
        is_av_value(maps:get(domain, Opts, <<>>)).

invalid_cookie_config(Name, Opts) ->
    {invalid_session_cookie_config,
     #{name => Name, path => maps:get(path, Opts, undefined), domain => maps:get(domain, Opts, undefined)}}.

is_token(<<C, Rest/binary>>) when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9 ->
    is_token(Rest);
is_token(<<C, Rest/binary>>) ->
    lists:member(C, "!#$%&'*+-.^_`|~") andalso is_token(Rest);
is_token(<<>>) ->
    true;
is_token(_) ->
    false.

is_av_value(<<C, Rest/binary>>) when C >= 16#20, C =< 16#7E, C =/= $; ->
    is_av_value(Rest);
is_av_value(<<>>) ->
    true;
is_av_value(_) ->
    false.

-doc false.
-spec presented_session_id(cowboy_req:req()) -> binary() | undefined | unparsable.
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
        exit:{request_error, _, _} ->
            unparsable
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================
known(SessionId, read) ->
    Now = erlang:monotonic_time(millisecond),
    case get(?KNOWN) of
        {SessionId, Known, At} when Now - At < ?KNOWN_FOR ->
            Known;
        _ ->
            known(SessionId, write)
    end;
known(SessionId, write) ->
    Known = backend_knows(SessionId),
    put(?KNOWN, {SessionId, Known, erlang:monotonic_time(millisecond)}),
    Known.

ended(_SessionId) ->
    erase(?KNOWN),
    ok.

backend_knows(SessionId) ->
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
    get_session_id(Req, read).

get_session_id(Req, Access) ->
    case enabled() of
        true ->
            session_id(Req, Access);
        _ ->
            ?LOG_ERROR(#{msg => ~"Session called but the 'use_sessions' option is set to false"}),
            throw({nova_session, unsupported_session_used})
    end.

session_id(#{nova_session_id := SessionId}, _Access) ->
    {ok, SessionId};
session_id(#{nova_session_presented := Presented, nova_session_pending := Pending}, Access) ->
    case known(Presented, Access) of
        true -> {ok, Presented};
        false -> {ok, Pending}
    end;
session_id(Req, _Access) ->
    case presented_session_id(Req) of
        SessionId when is_binary(SessionId) -> {ok, SessionId};
        _ -> {error, not_found}
    end.

with_session_id(SessionId, Req) ->
    (maps:without([nova_session_presented, nova_session_pending], Req))#{nova_session_id => SessionId}.

stream_handled(#{nova_session_direct := true}) ->
    false;
stream_handled(Req) ->
    is_map_key(nova_session_id, Req) orelse is_map_key(nova_session_presented, Req).

set_cookie(Value, Opts, Req) ->
    Name = cookie_name(),
    valid_cookie(Name, Opts) orelse erlang:error(invalid_cookie_config(Name, Opts)),
    Req0 = cowboy_req:set_resp_cookie(Name, Value, Req, Opts),
    Req0#{nova_session_direct => true}.

warn_ambiguous_cookie(Name) ->
    case nova_once:first({?MODULE, ambiguous_cookie_warned}) of
        true ->
            ?LOG_WARNING(#{msg => ~"Request carried more than one session cookie, treated as no session",
                           cookie => Name});
        false ->
            ok
    end.

notify(#{pid := _, streamid := _, nova_session_owner := Owner} = Req, Event) when Owner =:= self() ->
    cowboy_req:cast({nova_session, Event}, Req);
notify(_Req, _Event) ->
    ok.

-spec generate_session_id() -> {ok, binary()}.
generate_session_id() ->
    {ok, base64:encode(crypto:strong_rand_bytes(32))}.
