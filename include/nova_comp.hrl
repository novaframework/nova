-ifndef(OTP_RELEASE).
-define(OTP_RELEASE, 20). %% We just want to have nice ifdefs
-endif.


-if(?OTP_RELEASE >= 21).
-define(STACKTRACE(Class, Exception, Stacktrace), Class:Exception:Stacktrace ->).
-else.
-define(STACKTRACE(Class, Exception, Stacktrace), Class:Exception -> Stacktrace = erlang:get_stacktrace(), ).
-endif.
