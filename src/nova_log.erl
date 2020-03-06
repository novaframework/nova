-module(nova_log).
-export([
         log/2,
         log/3
        ]).

-include_lib("nova/include/nova.hrl").

-ifndef(OTP_RELEASE).
log(error, Format, Data) -> error_logger:error_msg(Format, Data);
log(warning, Format, Data) -> error_logger:warning_msg(Format, Data);
log(_, Format, Data) -> error_logger:info_msg(Format, Data).

log(Level, Msg) ->
    log(Level, Msg, []).
-else.
log(Level, Msg, Metadata) ->
    logger:log(Level, Msg, Metadata).

log(Level, Msg) ->
    log(Level, Msg, []).
-endif.
