%% Nova Routing value
-record(nova_handler_value, {
                             app :: atom(),
                             module :: atom(),
                             function :: atom(),
                             callback :: function() | undefined,
                             plugins = [] :: list(),
                             secure = false :: false | fun((nova_req:req()) -> term()) |
                                               [fun((nova_req:req()) -> term())] | {Mod :: atom(), Fun :: atom()},
                             extra_state :: any(),
                             http_versions :: undefined | [nova_server:protocol()]
                            }).

%% The value of a route that a module answers from its init/2, as a
%% WebSocket route is by nova_ws_handler
-record(nova_module_value, {
                            app :: atom(),
                            handler :: atom(),
                            arguments :: any(),
                            plugins = [] :: list(),
                            secure = false :: false | fun((nova_req:req()) -> term()) |
                                              [fun((nova_req:req()) -> term())] | {Mod :: atom(), Fun :: atom()},
                            http_versions :: undefined | [nova_server:protocol()]
                           }).
