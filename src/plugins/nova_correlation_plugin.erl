%%%-------------------------------------------------------------------
-module(nova_correlation_plugin).
-moduledoc """
## Plugin Configuration

To not break backwards compatibility in a minor release, some behavior is behind configuration items.

### Request Correlation Header Name

Set `request_correlation_header` in the plugin config to read the correlation ID from the request headers.

Notice: Cowboy request headers are always in lowercase.

### Default Correlation ID Generation

If the header name is not defined or the request lacks a correlation ID header, then the plugin generates a v4 UUID automatically.

### Logger Metadata Key Override

Use `logger_metadata_key` to customize the correlation ID key in OTP logger process metadata. By default it is set to `<<"correlation-id">>`.

### Correlation ID in Request Object

The plugin defines a field called `correlation_id` in the request object for controller use if it makes further requests that it want to pass on the correlation id to.

### Example configuration

```text
       {plugins, [
           {pre_request, nova_correlation_plugin, #{
               request_correlation_header => <<"x-correlation-id">>,
               logger_metadata_key => correlation_id
           }}
       ]}
   
```
{: lang=erlang }
""".
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
-doc "Pre-request callback to either pick up correlation id from request headers or generate a new uuid correlation id.".
pre_request(Req0, _Env, Opts, State) ->
    CorrId = get_correlation_id(Req0, Opts),
    %% Update the loggers metadata with correlation-id
    ok = update_logger_metadata(CorrId, Opts),
    Req1 = cowboy_req:set_resp_header(<<"x-correlation-id">>, CorrId, Req0),
    Req = Req1#{correlation_id => CorrId},
    {ok, Req, State}.

post_request(Req, _Env, _, State) ->
    {ok, Req, State}.

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
            CorrId
    end;
get_correlation_id(_Req, _Opts) ->
    uuid().

uuid() ->
    jhn_uuid:gen(v4, [binary]).

update_logger_metadata(CorrId, Opts) ->
    LoggerKey = maps:get(logger_metadata_key, Opts, <<"correlation-id">>),
    logger:update_process_metadata(#{LoggerKey => CorrId}).
