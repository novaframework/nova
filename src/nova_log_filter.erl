%% @doc Logger filter for redacting sensitive parameters from log output.
%%
%% Configurable via application environment:
%%   {nova, [{filter_parameters, [<<"password">>, <<"secret">>, <<"token">>]}]}
%%
%% Default filtered parameters: password, secret, token, api_key, authorization.
%%
%% Usage in sys.config logger configuration:
%%   {kernel, [{logger, [{handler, default, logger_std_h,
%%     #{filters => [{nova_param_filter,
%%       {fun nova_log_filter:filter/2, #{}}}]}}]}]}
-module(nova_log_filter).

-export([
         filter/2,
         redact_params/1,
         redact_params/2
        ]).

-define(DEFAULT_FILTERED, [<<"password">>, <<"secret">>, <<"token">>,
                           <<"api_key">>, <<"authorization">>,
                           <<"passwd">>, <<"credit_card">>]).
-define(REDACTED, <<"[FILTERED]">>).

%% OTP logger filter callback
-spec filter(logger:log_event(), map()) -> logger:filter_return().
filter(#{msg := {report, Report}} = Event, _Extra) when is_map(Report) ->
    Event#{msg := {report, redact_map(Report)}};
filter(Event, _Extra) ->
    Event.

%% Redact sensitive keys from a map using default filtered parameters
-spec redact_params(map()) -> map().
redact_params(Params) when is_map(Params) ->
    FilteredKeys = filtered_keys(),
    redact_params(Params, FilteredKeys).

%% Redact sensitive keys from a map using explicit filter list
-spec redact_params(map(), [binary()]) -> map().
redact_params(Params, FilteredKeys) when is_map(Params) ->
    maps:map(fun(Key, Value) ->
        BinKey = to_binary(Key),
        case lists:any(fun(F) -> matches(BinKey, F) end, FilteredKeys) of
            true -> ?REDACTED;
            false when is_map(Value) -> redact_params(Value, FilteredKeys);
            false -> Value
        end
    end, Params);
redact_params(Other, _FilteredKeys) ->
    Other.

%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%

filtered_keys() ->
    nova:get_env(filter_parameters, ?DEFAULT_FILTERED).

redact_map(Map) ->
    FilteredKeys = filtered_keys(),
    maps:map(fun(_Key, Value) when is_map(Value) ->
        redact_params(Value, FilteredKeys);
    (Key, Value) ->
        BinKey = to_binary(Key),
        case lists:any(fun(F) -> matches(BinKey, F) end, FilteredKeys) of
            true -> ?REDACTED;
            false -> Value
        end
    end, Map).

matches(Key, Filter) ->
    binary:match(string:lowercase(Key), string:lowercase(Filter)) =/= nomatch.

to_binary(Key) when is_binary(Key) -> Key;
to_binary(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
to_binary(Key) when is_list(Key) -> list_to_binary(Key);
to_binary(_) -> <<>>.
