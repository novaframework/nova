%%% Author: Niclas Axelsson <niclas@burbas.se>
-module(nova_session).
-moduledoc """
All kind of operations on sessions is handled by the nova_session-module. The module also presents a behaviour that can be used to create customized backends for the session-data.

The session id lives in the `__Host-session_id` cookie, and in `session_id`
when the `environment` is `dev` (see `session_cookie_name` in the
configuration guide). A browser keeps a cookie with the `__Host-` prefix only
when it is `Secure`, has `Path=/` and no `Domain`, so no other host of the
domain can set or shadow it. `nova_stream_h` issues it with the response of the
first request that writes to the session, so visitors that never store
anything get no cookie. A request carrying two or more different session ids
is treated as having no session. A request whose Cookie header does not parse
gets a session for that request only and no cookie, so a session cookie the
client already holds is not overwritten.

Sign a user in with `sign_in/3` and out with `delete/1`. `sign_in/3` moves
the client to a new session id, stores who signed in under it and returns the
request that carries the new id. That is the defence against session
fixation: a session id planted or observed before that point stops working,
and so does everything stored under it. `renew/1` is the piece of it that
changes the id, for any other change of what a client may do (a second
factor, a password change, a higher role). As defence in depth, a presented
id the backend does not know is ignored and replaced, so a client cannot
simply choose its own session id.

With `secret_key_base` set in the `nova` environment, the ids Nova makes are
signed with a key of that secret. A client can then hold an id that no
session is stored under yet, which is what a visitor of a page with a token
of `nova_csrf_plugin` gets, and the first `set/3` of its request stores the
session under that id and leaves the cookie as it is. The id of a client
changes with `sign_in/3`, `renew/1` and `delete/1` and with nothing else, so
an application that writes who is signed in with `set/3` alone signs in
whoever holds the id, also somebody who put it in the browser of the user. An id that is
not signed by a secret in use is replaced as above. So is one whose session
the request has read, deleted or renewed before it writes: what brought a
session back that another request ended would not be a new one. A signed id
is the 44 characters Nova handed out and nothing else: the same bytes
written another way, with whitespace in it or other bits to spare in its
last character, are an id nobody signed.

A session that was deleted, by `delete/1`, `renew/1` or `sign_in/3`, does
not come back. A request that was on its way when another one ended its
session writes to a session of its own, under another id, and never under
the one the client was moved off. `nova_session_ets` sees to that for five
minutes after a session was deleted, and a backend of your own does with
`c:session_ended/1`.

Session changes reach the cookie only when they are made from the process
serving the request (where controllers, plugins and `c:nova_websocket:init/1`
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
         set_provisional/3,
         delete/1,
         delete/2,
         sign_in/3,
         renew/1,
         client_id/1,
         generate_session_id/0
        ]).

%% Used by nova_stream_h
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
-define(HAD_A_SESSION, {?MODULE, had_a_session}).
-define(ID_RANDOM_BYTES, 16).
-define(ID_SIGNATURE_BYTES, 16).
-define(ID_CHARACTERS, 44).

%%%===================================================================
%%% Callbacks
%%%===================================================================

%% Get a value from a given session_id
-callback get_value(SessionId, Key) ->
    {ok, Value :: any()} |
    {error, Reason :: atom()}
        when SessionId :: binary(),
             Key :: binary().

-doc """
Stores `Value` under `Key`. `Value` is any term, as it was given to
`set/3`, and `c:get_value/2` has to return it as it was: a backend that
stores binaries encodes it, for instance with `term_to_binary/1`, or returns
`{error, Reason}` for a value it cannot store.
""".
-callback set_value(SessionId, Key, Value) ->
    ok |
    {error, Reason :: atom()}
        when SessionId :: binary(),
             Key :: binary(),
             Value :: term().

-doc """
Stores a value like `c:set_value/3`, for `set_provisional/3`. A session
that only holds values written this way is not in use yet, and the backend
may keep it for a shorter time and drop it first when it runs out of room.
A backend without it gets the value through `c:set_value/3`.
""".
-callback set_provisional_value(SessionId, Key, Value) ->
    ok |
    {error, Reason :: atom()}
        when SessionId :: binary(),
             Key :: binary(),
             Value :: term().

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

-doc """
Tells whether the session with this id was deleted with `c:delete_value/1` a
short while ago: at least for as long as a request to the application can
take. Nova asks before it starts a session under a signed id that no session
is stored under, so that a request that was on its way when its client
signed out, or signed in, does not start one under the id the client was
moved off. Such a request writes to a session of its own instead.

A backend that has it should also refuse to store under such an id, and
answer `{error, session_ended}` from `c:set_value/3` and
`c:set_provisional_value/3`: the session may be deleted between the moment
Nova asks `c:session_exists/1` and the write, and Nova stores what a backend
refuses this way in a session of its own as well. `nova_session_ets`
remembers a deleted id for five minutes, on every node.

A backend without it relies on what the process of the request knows: a
session it has read, deleted or renewed itself is not written again, and one
that another request ended before this one touched it can be.
""".
-callback session_ended(SessionId :: binary()) -> boolean().

-optional_callbacks([start_link/0, session_exists/1, session_ended/1, set_provisional_value/3]).

%%%===================================================================
%%% Public functions
%%%===================================================================
-doc "The value stored under `Key` in the session of the request, as it was given to `set/3`.".
-spec get(Req :: cowboy_req:req(), Key :: binary()) ->
          {ok, Value :: term()} | {error, Reason :: atom()} | no_return().
get(Req, Key) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:get_value(SessionId, Key);
        _ ->
            {error, not_found}
    end.

-doc """
Stores `Value` under `Key` in the session of the request. The value is
handed to the session backend as it is. `nova_session_ets` keeps any term
and returns it unchanged from `get/2`. A backend of your own decides what it
can store, and returns `{error, Reason}` for a value it cannot.
""".
-spec set(Req :: cowboy_req:req(), Key :: binary(), Value :: term()) ->
          ok | {error, Reason :: atom()} | no_return().
set(Req, Key, Value) ->
    write(set_value, Req, Key, Value).

-doc """
Stores `Value` under `Key` like `set/3`, for a value that every visitor gets
and that can be made again, such as a CSRF token. A session that holds
nothing else is not in use yet. `nova_session_ets` drops such a session after
a day and before any other when it is full, so requests without a cookie
cannot fill it and keep clients from signing in. The first `set/3` on the
session makes it an ordinary one. A backend of your own that does not have
`c:set_provisional_value/3` stores the value as `set/3` does.
""".
-spec set_provisional(Req :: cowboy_req:req(), Key :: binary(), Value :: term()) ->
          ok | {error, Reason :: atom()} | no_return().
set_provisional(Req, Key, Value) ->
    Mod = get_session_module(),
    _ = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, set_provisional_value, 3) of
        true -> write(set_provisional_value, Req, Key, Value);
        false -> write(set_value, Req, Key, Value)
    end.

%% A backend answers {error, session_ended} for a session that was deleted
%% after it said the session was there. What is written then goes where it
%% would have gone a moment later: to the session the request starts.
write(Function, Req, Key, Value) ->
    case get_session_id(Req, write) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            case {Mod:Function(SessionId, Key, Value), Req} of
                {{error, session_ended}, #{nova_session_pending := Pending}} when Pending =/= SessionId ->
                    ended(SessionId),
                    written(Mod:Function(Pending, Key, Value), Pending, Req);
                {Result, _Req} ->
                    written(Result, SessionId, Req)
            end;
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
The returned request carries a fresh session id, so a `set/3` on it later in
the same request starts a new session instead of reviving the deleted one.

```erlang
logout(Req) ->
    {ok, _SignedOut} = nova_session:delete(Req),
    {redirect, "/"}.
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
Signs a user in. The client is moved to a new session id as `renew/1` moves
it, `Value` is stored under `Key` in the new session, and the request that
comes back is the one that carries the new id. This is the call for the
place where a user signs in, in an application or in a library that signs in
for it:

```erlang
login(Req) ->
    {ok, SignedIn} = nova_session:sign_in(Req, ~"user_id", UserId),
    ok = nova_session:set(SignedIn, ~"signed_in_at", erlang:system_time(second)),
    {redirect, "/"}.
```

The session the client had is deleted with all it held, so an id that
somebody else knows, because they planted it in the browser or saw it, is not
the id of the user who signed in. Go on with the returned request: the one
that was passed in names the session that is gone. A token of
`nova_csrf_plugin` that was made for the old id is refused from then on, and
the next page the client loads carries one for the new id.

`{error, Reason}` is what the session backend answered when it did not store
the value. The id has changed by then and nobody is signed in.
""".
-spec sign_in(Req, Key :: binary(), Value :: term()) -> {ok, Req} | {error, Reason :: atom()} | no_return()
              when Req :: cowboy_req:req().
sign_in(Req, Key, Value) ->
    {ok, SignedIn} = renew(Req),
    case set(SignedIn, Key, Value) of
        {error, _Reason} = Error -> Error;
        _Stored -> {ok, SignedIn}
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

%% The session id the client of the request holds: the one in the session
%% cookie it sent, whether or not there is a session for it, or, when it sent
%% none, the id of the request, which the response then hands out in the
%% session cookie. Nothing is stored. For nova_csrf_plugin, whose signed
%% token is tied to the browser and not to what a session holds.
%%
%% A request whose Cookie header does not parse gets the id of the request
%% as well, but no cookie is handed out for it, since the client may hold a
%% session cookie Nova could not read. That id is not seen again, so what
%% was tied to it is refused when it comes back. {error, not_found} means
%% there is no id to give: the request has no session cookie Nova can read
%% and nova_stream_h is not there to hand one out.
-doc false.
-spec client_id(Req :: cowboy_req:req()) -> {ok, SessionId :: binary()} | {error, not_found} | no_return().
client_id(Req) ->
    case enabled() of
        true ->
            held_id(Req);
        _ ->
            ?LOG_ERROR(#{msg => ~"Session called but the 'use_sessions' option is set to false"}),
            throw({nova_session, unsupported_session_used})
    end.

held_id(#{nova_session_presented := Presented}) ->
    {ok, Presented};
held_id(#{nova_session_id := SessionId} = Req) ->
    notify(Req, {issued, SessionId}),
    {ok, SessionId};
held_id(Req) ->
    case presented_session_id(Req) of
        SessionId when is_binary(SessionId) -> {ok, SessionId};
        _ -> {error, not_found}
    end.

-spec delete(Req :: cowboy_req:req(), Key :: binary()) -> {ok, Req :: cowboy_req:req()} | no_return().
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
    case nova_config:get_env(session_cookie_name, undefined) of
        Name when is_binary(Name), Name =/= <<>> -> Name;
        _ -> default_cookie_name()
    end.

%% A cookie with the __Host- prefix is one no other host of the domain can
%% set or shadow. A browser keeps it over https only, which a development
%% machine does not have.
-spec default_cookie_name() -> binary().
default_cookie_name() ->
    case nova:is_dev_environment() of
        true -> ~"session_id";
        false -> ~"__Host-session_id"
    end.

%% Whether a browser keeps the session cookie only where it reached the site
%% over https. nova_sup tells a listener without TLS about it.
-doc false.
-spec needs_https() -> boolean().
needs_https() ->
    enabled() andalso maps:get(secure, cookie_opts_for(~"http"), false).

-doc false.
-spec max_age() -> pos_integer() | infinity.
max_age() ->
    case nova_config:get_env(session_max_age, ?DEFAULT_MAX_AGE) of
        MaxAge when is_integer(MaxAge), MaxAge > 0 -> MaxAge;
        infinity -> infinity;
        _ -> ?DEFAULT_MAX_AGE
    end.

%% Secure unless the environment is set to dev: behind a proxy that
%% terminates TLS the request itself looks like plain http. A __Host- cookie
%% is only stored by browsers when it is Secure, has Path=/ and no Domain.
-doc false.
-spec cookie_opts(cowboy_req:req()) -> cow_cookie:cookie_opts().
cookie_opts(Req) ->
    cookie_opts_for(maps:get(scheme, Req, ~"http")).

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

%% Checked at boot so a bad setting stops the node instead of failing every
%% response that sets the cookie. cookie_name/0 falls back to the default for a
%% name that is not a non-empty binary, so the configured value is checked here.
-doc false.
-spec validate_cookie_config() -> ok.
validate_cookie_config() ->
    ok = default_name_can_have_the_options(),
    Opts = cookie_opts_for(~"http"),
    Configured = nova_config:get_env(session_cookie_name, default_cookie_name()),
    Valid = is_binary(Configured) andalso valid_cookie(Configured, Opts) andalso
        try cow_cookie:setcookie(Configured, ~"x", Opts) of
            _ -> true
        catch
            error:_ -> false
        end,
    case Valid of
        true -> ok;
        false -> erlang:error(invalid_cookie_config(Configured, Opts))
    end.

%% With a __Host- name the cookie is always Secure, has Path=/ and no Domain.
%% An application that asks for something else and has not named its cookie
%% would lose what it asked for without a word, so it is told to name it.
-spec default_name_can_have_the_options() -> ok.
default_name_can_have_the_options() ->
    Named = nova_config:get_env(session_cookie_name, undefined) =/= undefined,
    case Named orelse nova:is_dev_environment() of
        true -> ok;
        false -> no_option_ruled_out_by_the_prefix(ruled_out_by_the_prefix())
    end.

-spec no_option_ruled_out_by_the_prefix([domain | path | secure]) -> ok.
no_option_ruled_out_by_the_prefix([]) ->
    ok;
no_option_ruled_out_by_the_prefix(Options) ->
    ?LOG_ERROR(#{msg => ~"session_cookie_opts asks for what a cookie with the __Host- prefix cannot have. The session cookie is named __Host-session_id when session_cookie_name is not set, and is then always Secure, with Path=/ and no Domain. Set session_cookie_name, to session_id for the cookie as it was, or take the options out",
                 setting => session_cookie_opts, options => Options, requires => session_cookie_name}),
    erlang:error({invalid_setting, session_cookie_opts, {requires, session_cookie_name}}).

-spec ruled_out_by_the_prefix() -> [domain | path | secure].
ruled_out_by_the_prefix() ->
    case nova_config:get_env(session_cookie_opts, #{}) of
        #{} = Configured ->
            [Key || Key <- [domain, path, secure], is_ruled_out_by_the_prefix(Key, Configured)];
        _ ->
            []
    end.

-spec is_ruled_out_by_the_prefix(domain | path | secure, map()) -> boolean().
is_ruled_out_by_the_prefix(domain, Configured) -> is_binary(maps:get(domain, Configured, undefined));
is_ruled_out_by_the_prefix(path, Configured) -> is_binary(maps:get(path, Configured, undefined)) andalso maps:get(path, Configured) =/= ~"/";
is_ruled_out_by_the_prefix(secure, Configured) -> maps:get(secure, Configured, true) =:= false.

%% The name must be an RFC 6265 token and path and domain ASCII without control
%% characters or ";". cowlib 2.21 refuses anything else, older versions only
%% some of it, so Nova checks it itself.
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
%% A header that does not parse may still hold our cookie, so it is reported
%% apart.
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
%% What the backend said about a presented id is kept by the calling process,
%% so a request that reads its session asks once. For a second and no longer:
%% the process of a websocket goes on with the request of its upgrade for as
%% long as it lives. A write always asks, or it would bring back a session
%% that another process deleted in the meantime.
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
    _ = Known andalso put(?HAD_A_SESSION, SessionId),
    Known.

%% The session of this id is over as far as this process goes, whatever the
%% id says about who handed it out.
ended(SessionId) ->
    erase(?KNOWN),
    put(?HAD_A_SESSION, SessionId),
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

%% Whether the backend says the session of this id was deleted a while ago.
%% One that cannot say is taken to say it was: the answer decides whether a
%% session is started under an id the client presented.
backend_ended(SessionId) ->
    Mod = get_session_module(),
    _ = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, session_ended, 1) of
        true ->
            try Mod:session_ended(SessionId) =/= false
            catch
                Class:Reason ->
                    ?LOG_WARNING(#{msg => ~"Session backend failed to say whether a session has ended",
                                   backend => Mod, class => Class, reason => nova_redact:term(Reason)}),
                    true
            end;
        false ->
            false
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

%% A new visitor's request carries its id in nova_session_id. When the client
%% sent a cookie, nova_stream_h hands over the presented id and a fresh one,
%% and the presented one is used only while the backend knows it, so a client
%% cannot choose its own id. The check runs here, in the process calling
%% nova_session, and not in the connection process.
session_id(#{nova_session_id := SessionId}, _Access) ->
    {ok, SessionId};
session_id(Req = #{nova_session_presented := Presented, nova_session_pending := Pending}, Access) ->
    case known(Presented, Access) orelse may_start_under(Presented, Req) of
        true -> {ok, Presented};
        false -> {ok, Pending}
    end;
session_id(Req, _Access) ->
    %% Without nova_stream_h there is only the cookie.
    case presented_session_id(Req) of
        SessionId when is_binary(SessionId) -> {ok, SessionId};
        _ -> {error, not_found}
    end.

%% Whether a session may be started under an id the backend has no session
%% for. Nova handed the id out, as its signature says, and no session was
%% stored then: a visitor got it with a page. The process serving the
%% request is the one that can keep the cookie as it is, and it must not
%% have seen a session under the id, which then was ended by somebody. Nor
%% must the backend have: a request that was on its way when another one
%% signed its client out, or in, has seen nothing of the session itself.
may_start_under(SessionId, #{nova_session_owner := Owner}) when Owner =:= self() ->
    get(?HAD_A_SESSION) =/= SessionId andalso is_signed(SessionId) andalso not backend_ended(SessionId);
may_start_under(_SessionId, _Req) ->
    false.

%% The decoder skips whitespace and ignores the bits of the last character
%% that belong to no byte, so the bytes of one id can be written in many
%% ways, each of them another session to the backend. The id is the one
%% spelling generate_session_id/0 gives those bytes.
-spec is_signed(SessionId :: binary()) -> boolean().
is_signed(<<SessionId:?ID_CHARACTERS/binary>>) ->
    case nova_secret:keys(session_id) of
        [] ->
            false;
        Keys ->
            try base64:decode(SessionId) of
                <<Random:?ID_RANDOM_BYTES/binary, Signature:?ID_SIGNATURE_BYTES/binary>> = Decoded ->
                    crypto:hash_equals(base64:encode(Decoded), SessionId) andalso
                        [] =/= [Key || Key <- Keys, crypto:hash_equals(Signature, id_signature(Key, Random))];
                _AnotherLength ->
                    false
            catch
                error:_NotBase64 -> false
            end
    end;
is_signed(_AnotherLength) ->
    false.

-spec id_signature(Key :: binary(), Random :: binary()) -> binary().
id_signature(Key, Random) ->
    crypto:macN(hmac, sha256, Key, Random, ?ID_SIGNATURE_BYTES).

with_session_id(SessionId, Req) ->
    (maps:without([nova_session_presented, nova_session_pending], Req))#{nova_session_id => SessionId}.

%% nova_stream_h puts the session ids in the request. Without it the cookie
%% goes on the request's response headers instead.
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
    Key = {?MODULE, ambiguous_cookie_warned},
    case nova_once:first(Key) of
        true ->
            ?LOG_WARNING(#{msg => ~"Request carried more than one session cookie, treated as no session. A cookie with the same name is probably set for a parent domain or path; a __Host- session_cookie_name prevents that",
                           cookie => Name});
        false ->
            ok
    end.

%% Tells nova_stream_h which session id the response has to hand out. Only
%% the process serving the request (stamped by nova_router) does: a process
%% spawned with the request, or a websocket running in the connection
%% process after the upgrade, would deliver the message to whatever handles
%% the connection by then.
notify(#{pid := _, streamid := _, nova_session_owner := Owner} = Req, Event) when Owner =:= self() ->
    cowboy_req:cast({nova_session, Event}, Req);
notify(_Req, _Event) ->
    ok.

-doc """
A new session id: 32 random bytes in base64, or, with `secret_key_base` set,
16 random bytes and 16 of their HMAC-SHA-256 under the key Nova has from the
secret for session ids, in base64.
""".
-spec generate_session_id() -> {ok, binary()}.
generate_session_id() ->
    case nova_secret:keys(session_id) of
        [Key | _] ->
            Random = crypto:strong_rand_bytes(?ID_RANDOM_BYTES),
            {ok, base64:encode(<<Random/binary, (id_signature(Key, Random))/binary>>)};
        [] ->
            {ok, base64:encode(crypto:strong_rand_bytes(?ID_RANDOM_BYTES + ?ID_SIGNATURE_BYTES))}
    end.
