-module(nova_log).
-export([
         log/2,
         log/3
        ]).

-include_lib("nova/include/nova.hrl").

-ifndef(OTP_RELEASE).
log(Level, Msg, Metadata) ->
    case Level of
        error -> io:format(?TERM_RED);
        warning -> io:format(?TERM_YELLOW);
        info -> io:format(?TERM_GREEN);
        _ -> ok
    end,
    io:format("[~p] ", [Level]),
    io:format(Msg, Metadata),
    io:format("~s~n", [?TERM_RESET]).

log(Level, Msg) ->
    log(Level, Msg, []).
-else.
log(Level, Msg, Metadata) ->
    logger:log(Level, Msg, Metadata).

log(Level, Msg) ->
    log(Level, Msg, []).
-endif.
