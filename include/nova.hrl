-define(LOG_DEPRECATED(SinceVersion, Msg, File), case File of
                                                     undefined -> logger:warning("Deprecation warning. Since version: ~s, Message: ~s~nRead more about deprecations on https://github.com/novaframework/nova/blob/master/guides/deprecations.md", [SinceVersion, Msg]);
                                                     _ ->
                                                         logger:warning("Deprecation warning. Since version: ~s, Message: ~s~nRead more about deprecations on https://github.com/novaframework/nova/blob/master/guides/deprecations.md\nFound in file: ~s", [SinceVersion, Msg, File])
                                                 end).
