-define(LOG_DEPRECATED(SinceVersion, Msg), logger:warning("Deprecation warning. Since version: ~s, Message: ~s~nRead more"
                                                          " about deprecations on https://github.com/novaframework/nova/blob/master/guides/deprecations.md", [SinceVersion, Msg])).

-define(LOG_DEPRECATED(SinceVersion, Msg, File), logger:warning("Deprecation warning. Since version: ~s, Message: ~s~nRead more about "
                                                                "deprecations on https://github.com/novaframework/nova/blob/master/guides/deprecations.md\nFound in file: ~s", [SinceVersion, Msg, File])).
