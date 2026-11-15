-module(nova_settings).
-moduledoc false.

%%% The settings of the `nova` environment that are checked when Nova
%%% starts. One that cannot be used stops the boot, and what is read
%%% afterwards is the value that was checked, so no request meets a setting
%%% nobody has looked at. They are read from `nova` alone, whichever
%%% application is the bootstrap one.

-export([validate/0, value/1, checked/2, expected/1]).

-include_lib("kernel/include/logger.hrl").

-type key() :: allowed_origins | csrf_secret | csrf_token | max_sessions | non_canonical_paths |
               plugins_before_security.
-export_type([key/0]).

-define(MIN_SECRET_BYTES, 32).
-define(DEFAULT_MAX_SESSIONS, 1000000).

-doc """
Checks every setting and keeps what `value/1` returns from then on. Raises
`{invalid_setting, Key, Reason}` for the first one that cannot be used.
`Reason` is the value that was refused, `redacted` where that is a secret,
or `not_set`. For `nova_sup`, which calls it before it starts anything.
""".
-spec validate() -> ok.
validate() ->
    Settings = #{Key => valid(Key) || Key <- keys()},
    ok = secret_to_sign_with(Settings),
    persistent_term:put(?MODULE, Settings).

-doc """
The checked value of a setting, or its default. `csrf_secret` is the list
of secrets, empty when none is set, and the origins of an `allowed_origins`
list are binaries. On a node where Nova was not started, the settings are
checked the first time one is read.
""".
-spec value(Key :: key()) -> term().
value(Key) ->
    case persistent_term:get(?MODULE, undefined) of
        #{Key := Value} ->
            Value;
        _NotChecked ->
            ok = validate(),
            maps:get(Key, persistent_term:get(?MODULE))
    end.

-doc """
What `Value` is kept as when it can be used for `Key`, and otherwise the
`Reason` of `{invalid_setting, Key, Reason}`. Never raises, and never
returns a secret that was refused. Also for a route group or a plugin that
takes one of the settings as an option of its own.
""".
-spec checked(Key :: key(), Value :: term()) -> {ok, term()} | {error, Reason :: term()}.
checked(non_canonical_paths, Value) when Value =:= reject; Value =:= resolve ->
    {ok, Value};
checked(allowed_origins, Value) when Value =:= same_host; Value =:= same_origin; Value =:= any ->
    {ok, Value};
checked(allowed_origins, Value) ->
    case origins(Value, []) of
        {ok, Origins} -> {ok, Origins};
        error -> {error, Value}
    end;
checked(csrf_token, Value) when Value =:= undefined; Value =:= stateless; Value =:= session ->
    {ok, Value};
checked(csrf_secret, undefined) ->
    {ok, []};
checked(csrf_secret, Secret) when is_binary(Secret) ->
    checked(csrf_secret, [Secret]);
checked(csrf_secret, Secrets) ->
    case is_list_of_secrets(Secrets) of
        true -> {ok, Secrets};
        false -> {error, redacted}
    end;
checked(max_sessions, Value) when Value =:= infinity; is_integer(Value), Value > 0 ->
    {ok, Value};
checked(plugins_before_security, all) ->
    {ok, all};
checked(plugins_before_security, Value) ->
    case is_list_of_modules(Value) of
        true -> {ok, Value};
        false -> {error, Value}
    end;
checked(_Key, Value) ->
    {error, Value}.

-doc "What a setting may be, in the words of the error that is logged for one that is not.".
-spec expected(Key :: key()) -> binary().
expected(non_canonical_paths) -> ~"non_canonical_paths must be reject or resolve";
expected(allowed_origins) -> ~"allowed_origins must be same_host, same_origin, any or a list of origins";
expected(csrf_token) -> ~"csrf_token must be stateless or session";
expected(csrf_secret) -> ~"csrf_secret must be a binary of at least 32 bytes, or a list of such";
expected(max_sessions) -> ~"max_sessions must be infinity or an integer above 0";
expected(plugins_before_security) -> ~"plugins_before_security must be all or a list of plugin modules".

-spec keys() -> [key()].
keys() ->
    [non_canonical_paths, allowed_origins, csrf_token, csrf_secret, max_sessions, plugins_before_security].

-spec default(key()) -> term().
default(non_canonical_paths) -> reject;
default(allowed_origins) -> same_host;
default(csrf_token) -> undefined;
default(csrf_secret) -> undefined;
default(max_sessions) -> ?DEFAULT_MAX_SESSIONS;
default(plugins_before_security) -> [].

-spec valid(key()) -> term().
valid(Key) ->
    case checked(Key, application:get_env(nova, Key, default(Key))) of
        {ok, Value} ->
            Value;
        {error, Reason} ->
            ?LOG_ERROR(#{msg => expected(Key), setting => Key, value => Reason}),
            erlang:error({invalid_setting, Key, Reason})
    end.

-spec secret_to_sign_with(map()) -> ok.
secret_to_sign_with(#{csrf_token := stateless, csrf_secret := []}) ->
    ?LOG_ERROR(#{msg => ~"csrf_token is stateless and csrf_secret is not set", setting => csrf_secret,
                 value => not_set}),
    erlang:error({invalid_setting, csrf_secret, not_set});
secret_to_sign_with(_Settings) ->
    ok.

-spec origins(term(), [binary()]) -> {ok, [binary()]} | error.
origins([], Origins) ->
    {ok, lists:reverse(Origins)};
origins([Origin | Rest], Origins) when is_binary(Origin) ->
    origins(Rest, [Origin | Origins]);
origins([Origin | Rest], Origins) when is_list(Origin) ->
    case string_as_binary(Origin) of
        {ok, Binary} -> origins(Rest, [Binary | Origins]);
        error -> error
    end;
origins(_NotOrigins, _Origins) ->
    error.

-spec string_as_binary(list()) -> {ok, binary()} | error.
string_as_binary(String) ->
    try unicode:characters_to_binary(String) of
        Binary when is_binary(Binary) -> {ok, Binary};
        _NotUnicode -> error
    catch
        error:badarg -> error
    end.

%% Written without a call that raises on what the setting holds: a reason
%% that is raised carries the value into the report of the boot that failed.
-spec is_list_of_secrets(term()) -> boolean().
is_list_of_secrets([Secret]) ->
    is_secret(Secret);
is_list_of_secrets([Secret | Secrets]) ->
    is_secret(Secret) andalso is_list_of_secrets(Secrets);
is_list_of_secrets(_NotAList) ->
    false.

-spec is_secret(term()) -> boolean().
is_secret(Secret) ->
    is_binary(Secret) andalso byte_size(Secret) >= ?MIN_SECRET_BYTES.

-spec is_list_of_modules(term()) -> boolean().
is_list_of_modules([]) ->
    true;
is_list_of_modules([Module | Modules]) ->
    is_atom(Module) andalso is_list_of_modules(Modules);
is_list_of_modules(_NotAList) ->
    false.
