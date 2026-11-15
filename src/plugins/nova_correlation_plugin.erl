%%%-------------------------------------------------------------------
-module(nova_correlation_plugin).
-moduledoc """
## Plugin Configuration

To not break backwards compatibility in a minor release, some behavior is behind configuration items.

### Request Correlation Header Name

Set `request_correlation_header` in the plugin config to read the correlation ID from the request headers.

Notice: Cowboy request headers are always in lowercase.

The value is used when it is 1 to 255 visible ASCII characters (no spaces, control characters or bytes above 126). Any other value is ignored and an ID is generated, since the ID is written to the logger metadata and to the response.

### Default Correlation ID Generation

If the header name is not defined or the request lacks a correlation ID header, then the plugin generates a v4 UUID automatically.

### Logger Metadata Key Override

Use `logger_metadata_key` to customize the correlation ID key in OTP logger process metadata. By default it is the atom `'correlation-id'`. Logger metadata keys are atoms, so write the key as an atom. A binary is accepted when it names an atom the node already knows, such as `<<"correlation-id">>`, and is logged under that atom.

### Correlation ID in Request Object

The plugin defines a field called `correlation_id` in the request object for controller use if it makes further requests that it want to pass on the correlation id to.

### Example configuration

```erlang
{plugins, [
    {pre_request, nova_correlation_plugin, #{
        request_correlation_header => <<"x-correlation-id">>,
        logger_metadata_key => correlation_id
    }}
]}
```
""".
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

-include_lib("kernel/include/logger.hrl").

-define(MAX_CORRELATION_ID_SIZE, 255).
-define(DEFAULT_LOGGER_KEY, 'correlation-id').

%%--------------------------------------------------------------------
-doc """
Takes the correlation id from the header named by
`request_correlation_header`, or generates a v4 UUID. Puts it in the logger
metadata of the process, in the `x-correlation-id` response header and in
the request under `correlation_id`. Always returns `{ok, Req, State}`.
""".
pre_request(Req0, _Env, Opts, State) ->
    CorrId = get_correlation_id(Req0, Opts),
    %% Update the loggers metadata with correlation-id
    ok = update_logger_metadata(CorrId, Opts),
    Req1 = cowboy_req:set_resp_header(<<"x-correlation-id">>, CorrId, Req0),
    Req = Req1#{correlation_id => CorrId},
    {ok, Req, State}.

-doc "Does nothing: the plugin works in `pre_request/4`.".
post_request(Req, _Env, _, State) ->
    {ok, Req, State}.

-doc "The title, version, description and options of the plugin.".
plugin_info() ->
    #{
      title => <<"nova_correlation_plugin">>,
      version => <<"0.2.0">>,
      url => <<"https://github.com/novaframework/nova">>,
      authors => [<<"Nova team <info@novaframework.org">>],
      description => <<"Add X-Correlation-ID headers to response">>,
      options => [
                  {logger_metadata_key, <<"Under which key should the UUID be put in logger metadata?">>}
                 ]
     }.

get_correlation_id(Req, #{ request_correlation_header := CorrelationHeader }) ->
    CorrelationHeaderLower = jhn_bstring:to_lower(CorrelationHeader),
    case cowboy_req:header(CorrelationHeaderLower, Req) of
        undefined ->
            uuid();
        CorrId ->
            case is_correlation_id(CorrId) of
                true -> CorrId;
                false -> uuid()
            end
    end;
get_correlation_id(_Req, _Opts) ->
    uuid().

is_correlation_id(CorrId) ->
    byte_size(CorrId) >= 1 andalso byte_size(CorrId) =< ?MAX_CORRELATION_ID_SIZE
        andalso is_visible_ascii(CorrId).

is_visible_ascii(<<Byte, Rest/binary>>) when Byte > $\s, Byte < 127 ->
    is_visible_ascii(Rest);
is_visible_ascii(Bytes) ->
    Bytes =:= <<>>.

uuid() ->
    jhn_uuid:gen(v4, [binary]).

update_logger_metadata(CorrId, Opts) ->
    logger:update_process_metadata(#{logger_key(Opts) => CorrId}).

logger_key(#{logger_metadata_key := Key}) when is_atom(Key) ->
    Key;
logger_key(#{logger_metadata_key := Key}) when is_binary(Key) ->
    Term = {?MODULE, logger_metadata_key, Key},
    case persistent_term:get(Term, undefined) of
        undefined ->
            Atom = known_atom(Key),
            persistent_term:put(Term, Atom),
            Atom;
        Atom when is_atom(Atom) ->
            Atom
    end;
logger_key(_Opts) ->
    ?DEFAULT_LOGGER_KEY.

known_atom(Key) ->
    try
        binary_to_existing_atom(Key)
    catch
        error:badarg ->
            ?LOG_ERROR(#{msg => ~"logger_metadata_key of nova_correlation_plugin is a binary that names no atom this node knows. Write the key as an atom. Until then the correlation id is logged under 'correlation-id'.",
                         logger_metadata_key => Key}),
            ?DEFAULT_LOGGER_KEY
    end.
