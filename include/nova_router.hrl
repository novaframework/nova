%% Nova Routing value
-record(nova_handler_value, {
                             app :: atom(),
                             module :: atom(),
                             function :: atom(),
                             secure = false :: false | {Mod :: atom(), Fun :: atom()},
                             extra_state :: any(),
                             protocol = http :: http | static | ws
                            }).

-record(cowboy_handler_value, {
                               app :: atom(),
                               handler :: atom(),
                               arguments :: [any()],
                               secure :: tuple()
                              }).
