-module(nova_settings).
-moduledoc false.

-export([validate/0, value/1]).

-include_lib("kernel/include/logger.hrl").

-type key() :: max_sessions.
-export_type([key/0]).

-define(DEFAULT_MAX_SESSIONS, 1000000).

-spec validate() -> ok.
validate() ->
    Settings = #{Key => valid(Key) || Key <- keys()},
    persistent_term:put(?MODULE, Settings).

-spec value(Key :: key()) -> term().
value(Key) ->
    case persistent_term:get(?MODULE, undefined) of
        #{Key := Value} ->
            Value;
        _NotChecked ->
            ok = validate(),
            maps:get(Key, persistent_term:get(?MODULE))
    end.

-spec checked(Key :: key(), Value :: term()) -> {ok, term()} | {error, Reason :: term()}.
checked(max_sessions, Value) when Value =:= infinity; is_integer(Value), Value > 0 ->
    {ok, Value};
checked(_Key, Value) ->
    {error, Value}.

-spec expected(Key :: key()) -> binary().
expected(max_sessions) -> ~"max_sessions must be infinity or an integer above 0".

-spec keys() -> [key()].
keys() ->
    [max_sessions].

-spec default(key()) -> term().
default(max_sessions) -> ?DEFAULT_MAX_SESSIONS.

-spec valid(key()) -> term().
valid(Key) ->
    case checked(Key, application:get_env(nova, Key, default(Key))) of
        {ok, Value} ->
            Value;
        {error, Reason} ->
            ?LOG_ERROR(#{msg => expected(Key), setting => Key, value => Reason}),
            erlang:error({invalid_setting, Key, Reason})
    end.
