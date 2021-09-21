%% Nova Routing value
-record(nova_router_value, {
              app :: atom(),
              module :: atom(),
              function :: atom(),
              secure = false :: false | {Mod :: atom(), Fun :: atom()},
              extra_state :: list(),
              protocol = http :: http | static | ws
             }).
