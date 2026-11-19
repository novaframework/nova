%%% Author: Niclas Axelsson <niclas@burbas.se>
-module(nova_session).
-moduledoc "All kind of operations on sessions is handled by the nova_session-module. The module also presents a behaviour that can be used to create customized backends for the session-data.".
-export([
         get/2,
         set/3,
         delete/1,
         delete/2,
         generate_session_id/0
        ]).

-export([
         enabled/0,
         cookie_opts/1
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

-optional_callbacks([start_link/0]).

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
            Mod:set_value(SessionId, Key, Value);
        _ ->
            {error, session_id_not_set}
    end.

-spec delete(Req :: cowboy_req:req()) -> {ok, Req :: cowboy_req:req()} |
          {error, Reason :: atom()}.
delete(Req) ->
    case get_session_id(Req) of
        {ok, SessionId} ->
            Mod = get_session_module(),
            Mod:delete_value(SessionId),
            Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req,
                                              (cookie_opts(Req))#{max_age => 0}),
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
-spec cookie_opts(cowboy_req:req()) -> cow_cookie:cookie_opts().
cookie_opts(Req) ->
    Secure = not nova:is_dev_environment() orelse maps:get(scheme, Req, ~"http") =:= ~"https",
    Defaults = #{path => ~"/", http_only => true, same_site => lax, secure => Secure},
    case nova_config:get_env(session_cookie_opts, #{}) of
        Configured when is_map(Configured) -> merge_cookie_opts(maps:to_list(Configured), Defaults);
        _ -> Defaults
    end.

merge_cookie_opts([], Opts) ->
    Opts;
merge_cookie_opts([{Key, Value} | Rest], Opts) when (Key =:= path orelse Key =:= domain), is_binary(Value) ->
    merge_cookie_opts(Rest, Opts#{Key => Value});
merge_cookie_opts([{Key, Value} | Rest], Opts) when (Key =:= secure orelse Key =:= http_only), is_boolean(Value) ->
    merge_cookie_opts(Rest, Opts#{Key => Value});
merge_cookie_opts([{same_site, Value} | Rest], Opts)
  when Value =:= default; Value =:= none; Value =:= strict; Value =:= lax ->
    merge_cookie_opts(Rest, Opts#{same_site => Value});
merge_cookie_opts([_ | Rest], Opts) ->
    merge_cookie_opts(Rest, Opts).

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
            ?LOG_ERROR(#{msg => ~"Session called but the 'use_sessions' option is set to false"}),
            throw({nova_session, unsupported_session_used})
    end.

-spec generate_session_id() -> {ok, binary()}.
generate_session_id() ->
    {ok, base64:encode(crypto:strong_rand_bytes(32))}.
