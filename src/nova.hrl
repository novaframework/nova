-define(NOVA_LISTENER, nova_listener).
-define(NOVA_STD_PORT, 8080).
-define(NOVA_STD_SSL_PORT, 8443).


-define(LOG_DEPRECATED(Version, Message),
        logger:warning("[DEPRECATED] Since: ~s. ~s. Read more about "
                       "deprecations on https://github.com/novaframework/nova/blob/master/guides/deprecations.md",
                       [Version, Message])).
