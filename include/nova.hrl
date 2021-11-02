-ifndef(_NOVA_HRL_).
-define(_NOVA_HRL, 1).
-include_lib("kernel/include/logger.hrl").

-define(TERM_RED, "\033[31m").
-define(TERM_GREEN, "\033[92m").
-define(TERM_YELLOW, "\033[33m").
-define(TERM_BLUE, "\033[96m").
-define(TERM_BOLD, "\033[1m").
-define(TERM_RESET, "\033[m").

-define(MOD_INFO, ?TERM_BOLD ++ "[" ++ erlang:atom_to_list(?MODULE) ++ "] " ++ ?TERM_RESET).

-define(DEBUG(M), ?LOG(debug, ?MOD_INFO ++ ?TERM_BLUE ++ M ++ ?TERM_RESET)).
-define(DEBUG(M, Meta), ?LOG(debug, ?MOD_INFO ++ ?TERM_BLUE ++ M ++ ?TERM_RESET, Meta)).
-define(INFO(M), ?LOG(info, ?MOD_INFO ++ ?TERM_GREEN ++ M ++ ?TERM_RESET)).
-define(INFO(M,Meta), ?LOG(info, ?MOD_INFO ++ ?TERM_GREEN ++ M ++ ?TERM_RESET, Meta)).
-define(NOTICE(M), ?LOG(notice, ?MOD_INFO ++ ?TERM_GREEN ++ M ++ ?TERM_RESET)).
-define(NOTICE(M,Meta), ?LOG(notice, ?MOD_INFO ++ ?TERM_GREEN ++ M ++ ?TERM_RESET, Meta)).
-define(WARNING(M), ?LOG(warning, ?MOD_INFO ++ ?TERM_YELLOW ++ M ++ ?TERM_RESET)).
-define(WARNING(M,Meta), ?LOG(warning, ?MOD_INFO ++ ?TERM_YELLOW ++ M ++ ?TERM_RESET, Meta)).
-define(ERROR(M), ?LOG(error, ?MOD_INFO ++ ?TERM_RED ++ M ++ ?TERM_RESET)).
-define(ERROR(M,Meta), ?LOG(error, ?MOD_INFO ++ ?TERM_RED ++ M ++ ?TERM_RESET, Meta)).

%% Meta levels
-define(DEPRECATION(M), ?LOG(warning, ?TERM_YELLOW ++ ?TERM_BOLD ++ "DEPRECATION WARNING! " ++ ?TERM_RESET ++ ?TERM_YELLOW ++ M ++ ?TERM_RESET)).
-define(DEPRECATED(M), ?LOG(error, ?TERM_RED ++ ?TERM_BOLD ++ "DEPRECATION ERROR!!! " ++ ?TERM_RESET ++ ?TERM_RED ++ M ++ ?TERM_RESET)).

-endif.
