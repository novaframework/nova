-module(nova_settings).
-moduledoc false.

-export([validate/0, value/1, checked/2, expected/1, removed/0, removed_notice/1]).

-include_lib("kernel/include/logger.hrl").

-type key() :: trusted_origins | csrf_token | max_sessions | non_canonical_paths | plugins_before_security |
               secret_key_base.
-export_type([key/0]).

-define(MIN_SECRET_BYTES, 32).
-define(MIN_DISTINCT_BYTES, 8).
-define(DEFAULT_MAX_SESSIONS, 1000000).

-doc """
Checks every setting and keeps what `value/1` returns from then on. Raises
`{invalid_setting, Key, Reason}` for the first one that cannot be used.
`Reason` is the value that was refused, `redacted` where that is a secret
(one that is too short, not a binary, or plainly not out of a random
source), `not_set`, `{requires, Setting}` for one that cannot be used without another,
or `{replaced_by, Replacement}` for a setting of `removed/0`. For
`nova_sup`, which calls it before it starts anything.
""".
-spec validate() -> ok.
validate() ->
    ok = no_removed_setting(removed()),
    Settings = #{Key => valid(Key) || Key <- keys()},
    ok = secret_to_sign_with(Settings),
    persistent_term:put(?MODULE, Settings).

-doc """
The checked value of a setting, or its default. `secret_key_base` is the keys
made from each of its secrets (`nova_secret:derived/1`), empty when none is
set, and never a secret itself. The origins of a `trusted_origins` list are
binaries. On a node where Nova was not started, the settings are
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
checked(trusted_origins, Value) when Value =:= same_host; Value =:= same_origin; Value =:= any ->
    {ok, Value};
checked(trusted_origins, Value) ->
    case origins(Value, []) of
        {ok, Origins} -> {ok, Origins};
        error -> {error, Value}
    end;
checked(csrf_token, Value) when Value =:= undefined; Value =:= stateless; Value =:= session ->
    {ok, Value};
checked(secret_key_base, undefined) ->
    {ok, []};
checked(secret_key_base, Secret) when is_binary(Secret) ->
    checked(secret_key_base, [Secret]);
checked(secret_key_base, Value) ->
    case secrets(Value) of
        {ok, Secrets} -> {ok, [nova_secret:derived(Secret) || Secret <- Secrets]};
        error -> {error, redacted}
    end;
checked(max_sessions, Value) when Value =:= infinity; is_integer(Value), Value > 0 ->
    {ok, Value};
checked(plugins_before_security, all) ->
    {ok, all};
checked(plugins_before_security, Value) ->
    case is_list_of_plugins(Value) of
        true -> {ok, Value};
        false -> {error, Value}
    end;
checked(_Key, Value) ->
    {error, Value}.

-doc "What a setting may be, in the words of the error that is logged for one that is not.".
-spec expected(Key :: key()) -> binary().
expected(non_canonical_paths) -> ~"non_canonical_paths must be reject or resolve";
expected(trusted_origins) -> ~"trusted_origins must be same_host, same_origin, any or a list of origins";
expected(csrf_token) -> ~"csrf_token must be stateless or session";
expected(secret_key_base) ->
    ~"secret_key_base must be a binary of at least 32 bytes out of a random source, or a list of such. A value that repeats a few bytes, an example out of the documentation or a ${VARIABLE} that was not filled in is refused. base64:encode(crypto:strong_rand_bytes(48)) in an Erlang shell, or openssl rand -base64 48, makes one";
expected(max_sessions) -> ~"max_sessions must be infinity or an integer above 0";
expected(plugins_before_security) ->
    ~"plugins_before_security must be all or a list of plugin modules that have pre_request/4".

-doc """
The settings that were in no release and are gone, each with the one that
took its place. One that is still set stops the boot with
`{invalid_setting, Key, {replaced_by, Replacement}}`.
""".
-spec removed() -> [{Key :: atom(), Replacement :: key()}].
removed() ->
    [{plugins_after_security, plugins_before_security},
     {allowed_origins, trusted_origins},
     {csrf_secret, secret_key_base}].

-doc "What is logged for a setting of `removed/0` that is still set.".
-spec removed_notice(Key :: atom()) -> binary().
removed_notice(allowed_origins) ->
    ~"allowed_origins is now trusted_origins, with the values it had";
removed_notice(csrf_secret) ->
    ~"csrf_secret is now secret_key_base: one secret for the node, from which nova_csrf_plugin has a key of its own";
removed_notice(plugins_after_security) ->
    ~"plugins_after_security is gone. pre_security/4 of a plugin always runs ahead of the security callback and pre_request/4 behind it, and plugins_before_security is the one setting that changes that".

-spec no_removed_setting([{atom(), key()}]) -> ok.
no_removed_setting([]) ->
    ok;
no_removed_setting([{Key, Replacement} | Rest]) ->
    case application:get_env(nova, Key) of
        undefined ->
            no_removed_setting(Rest);
        {ok, _Value} ->
            ?LOG_ERROR(#{msg => removed_notice(Key), setting => Key, replaced_by => Replacement}),
            erlang:error({invalid_setting, Key, {replaced_by, Replacement}})
    end.

-spec keys() -> [key()].
keys() ->
    [non_canonical_paths, trusted_origins, csrf_token, secret_key_base, max_sessions, plugins_before_security].

-spec default(key()) -> term().
default(non_canonical_paths) -> reject;
default(trusted_origins) -> same_host;
default(csrf_token) -> undefined;
default(secret_key_base) -> undefined;
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
secret_to_sign_with(#{csrf_token := stateless, secret_key_base := []}) ->
    ?LOG_ERROR(#{msg => ~"csrf_token is stateless and secret_key_base, which signs the tokens, is not set",
                 setting => secret_key_base, value => not_set}),
    erlang:error({invalid_setting, secret_key_base, not_set});
secret_to_sign_with(#{csrf_token := stateless}) ->
    case nova_session:enabled() of
        true ->
            ok;
        false ->
            ?LOG_ERROR(#{msg => ~"csrf_token is stateless and use_sessions is false. A signed token is for the session cookie of a client, which Nova does not hand out without sessions",
                         setting => csrf_token, value => stateless}),
            erlang:error({invalid_setting, csrf_token, {requires, use_sessions}})
    end;
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

-spec secrets(term()) -> {ok, [binary()]} | error.
secrets([Secret]) when is_binary(Secret), byte_size(Secret) >= ?MIN_SECRET_BYTES ->
    case is_plainly_not_random(Secret) of
        false -> {ok, [Secret]};
        true -> error
    end;
secrets([Secret | Rest]) when is_binary(Secret), byte_size(Secret) >= ?MIN_SECRET_BYTES ->
    case is_plainly_not_random(Secret) orelse secrets(Rest) of
        {ok, Secrets} -> {ok, [Secret | Secrets]};
        _NotSecrets -> error
    end;
secrets(_NotSecrets) ->
    error.

-spec is_plainly_not_random(Secret :: binary()) -> boolean().
is_plainly_not_random(Secret) ->
    length(lists:usort(binary_to_list(Secret))) < ?MIN_DISTINCT_BYTES orelse
        lists:member(Secret, documented_examples()) orelse is_variable_not_filled_in(Secret).

-spec documented_examples() -> [binary()].
documented_examples() ->
    [~"at least 32 bytes that every node of the system is given",
     ~"the-new-secret-of-at-least-32-bytes-0001",
     ~"the-old-secret-of-at-least-32-bytes-0000"].

-spec is_variable_not_filled_in(Secret :: binary()) -> boolean().
is_variable_not_filled_in(<<"${", Name/binary>>) ->
    is_variable_name(Name);
is_variable_not_filled_in(_Secret) ->
    false.

-spec is_variable_name(binary()) -> boolean().
is_variable_name(<<"}">>) ->
    true;
is_variable_name(<<Char, Rest/binary>>)
  when Char >= $A, Char =< $Z; Char >= $a, Char =< $z; Char >= $0, Char =< $9; Char =:= $_ ->
    is_variable_name(Rest);
is_variable_name(_NotAName) ->
    false.

-spec is_list_of_plugins(term()) -> boolean().
is_list_of_plugins([]) ->
    true;
is_list_of_plugins([Module | Modules]) ->
    is_atom(Module) andalso has_pre_request(Module) andalso is_list_of_plugins(Modules);
is_list_of_plugins(_NotAList) ->
    false.

-spec has_pre_request(module()) -> boolean().
has_pre_request(Module) ->
    _ = code:ensure_loaded(Module),
    erlang:function_exported(Module, pre_request, 4).
