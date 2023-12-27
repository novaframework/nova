%% If USE_STACKTRACE is enabled then we return stacktraces
-ifdef(USE_STACKTRACES).
-define(CATCH_CLAUSE(T, R, S), T:R -> S = not_enabled,).
-else.
-define(CATCH_CLAUSE(T, R, S), T:R:S ->).
-endif.
