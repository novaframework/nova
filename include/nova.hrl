-ifndef(_NOVA_HRL_).
-define(_NOVA_HRL, 1).
-include_lib("kernel/include/logger.hrl").

-define(TERM_RED, "\033[31m").
-define(TERM_GREEN, "\033[92m").
-define(TERM_YELLOW, "\033[33m").
-define(TERM_BLUE, "\033[96m").
-define(TERM_BOLD, "\033[1m").
-define(TERM_RESET, "\033[m").

-define(DEBUG(M), ?LOG(debug, M)).
-define(DEBUG(M, Meta), ?LOG(debug, M, Meta)).
-define(INFO(M), ?LOG(info, M)).
-define(INFO(M, Meta), ?LOG(info, M, Meta)).
-define(NOTICE(M), ?LOG(notice, M)).
-define(NOTICE(M, Meta), ?LOG(notice, M, Meta)).
-define(WARNING(M), ?LOG(warning, M)).
-define(WARNING(M, Meta), ?LOG(warning, M, Meta)).
-define(ERROR(M), ?LOG(error, M)).
-define(ERROR(M, Meta), ?LOG(error, M, Meta)).


-endif.
