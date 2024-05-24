%% Nova Routing value
-record(nova_handler_value, {
                             app :: atom(),
                             module :: atom(),
                             function :: atom(),
                             callback :: function() | undefined,
                             plugins = [] :: list(),
                             secure = false :: false | {Mod :: atom(), Fun :: atom()},
                             extra_state :: any()
                            }).

-record(cowboy_handler_value, {
                               app :: atom(),
                               handler :: atom(),
                               arguments :: any(),
                               plugins = [] :: list(),
                               secure = false :: false | {Mod :: atom(), Fun :: atom()}
                              }).
