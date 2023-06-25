%% Nova Routing value
-record(nova_handler_value, {
                             app :: atom(),
                             module :: atom(),
                             function :: atom(),
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

-record(rt_node, {
                  segment = <<>> :: binary(),
                  value = [] :: any(),
                  comparator = <<>> ::  binary(),
                  children = []
              }).

-type rt_node() :: #rt_node{}.

-export_type([
              rt_node/0
             ]).
