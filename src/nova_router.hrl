%% Nova Routing value
-record(nova_handler_value, {
                             app :: atom(),
                             module :: atom(),
                             function :: atom(),
                             plugins = [] :: list(),
                             secure = false :: false | {Mod :: atom(), Fun :: atom()},
                             extra_state :: any()
                            }).
