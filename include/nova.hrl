-ifndef(OTP_RELEASE).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(), ).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.

-define(TERM_RED, "\033[31m").
-define(TERM_GREEN, "\033[32m").
-define(TERM_YELLOW, "\033[33m").
-define(TERM_BLUE, "\033[34m").
-define(TERM_BOLD, "\033[1m").
-define(TERM_RESET, "\033[m").


-define(DEBUG(M), nova_log:log(debug, M)).
-define(DEBUG(M, Meta), nova_log:log(debug, M, Meta)).
-define(INFO(M), nova_log:log(info, ?TERM_GREEN ++ M ++ ?TERM_RESET)).
-define(INFO(M,Meta), nova_log:log(info, ?TERM_GREEN ++ M ++ ?TERM_RESET, Meta)).
-define(WARNING(M), nova_log:log(warning, ?TERM_YELLOW ++ M ++ ?TERM_RESET)).
-define(WARNING(M,Meta), nova_log:log(warning, ?TERM_YELLOW ++ M ++ ?TERM_RESET, Meta)).
-define(ERROR(M), nova_log:log(error, ?TERM_RED ++ M ++ ?TERM_RESET)).
-define(ERROR(M,Meta), nova_log:log(error, ?TERM_RED ++ M ++ ?TERM_RESET, Meta)).
-define(DEPRECATION(M), nova_log:log(warning, ?TERM_YELLOW ++ "DEPRECATION WARNING! " ++ M ++ ?TERM_RESET)).
-define(DEPRECATED(M), nova_log:log(error, ?TERM_RED ++ "DEPRECATION ERROR!!! " ++ M ++ ?TERM_RESET)).
