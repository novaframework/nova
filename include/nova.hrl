-ifndef(OTP_RELEASE).
-define(LOG(Level, Msg), io:format("[~p] ~s~n", [Level, Msg])).
-define(LOG(Level, Msg, Metadata),
        case Level of
            error -> io:format(?TERM_RED);
            warning -> io:format(?TERM_YELLOW);
            info -> io:format(?TERM_GREEN);
            _ -> ok
        end,
        io:format("[~p] ", [Level]),
        io:format(Msg, Metadata),
        io:format("~s~n", [?TERM_RESET])).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(), ).
-else.
-define(LOG(Level, Msg), logger:log(Level, Msg)).
-define(LOG(Level, Msg, Metadata), logger:log(Level, Msg, Metadata)).
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.


-define(TERM_RED, "\033[31m").
-define(TERM_GREEN, "\033[32m").
-define(TERM_YELLOW, "\033[33m").
-define(TERM_BLUE, "\033[34m").
-define(TERM_BOLD, "\033[1m").
-define(TERM_RESET, "\033[m").

-define(DEBUG(M), ?LOG(debug, M)).
-define(DEBUG(M, Meta), ?LOG(debug, M, Meta)).
-define(INFO(M), ?LOG(info, ?TERM_GREEN ++ M ++ ?TERM_RESET)).
-define(INFO(M,Meta), ?LOG(info, ?TERM_GREEN ++ M ++ ?TERM_RESET, Meta)).
-define(WARNING(M), ?LOG(warning, ?TERM_YELLOW ++ M ++ ?TERM_RESET)).
-define(WARNING(M,Meta), ?LOG(warning, ?TERM_YELLOW ++ M ++ ?TERM_RESET, Meta)).
-define(ERROR(M), ?LOG(error, ?TERM_RED ++ M ++ ?TERM_RESET)).
-define(ERROR(M,Meta), ?LOG(error, ?TERM_RED ++ M ++ ?TERM_RESET, Meta)).
