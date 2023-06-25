-record(path_info, {
                    value = [] :: any(),
                    qs = [] :: [{binary(), binary() | true}],
                    bindings = [] :: [{binary(), binary()}]
                   }).

-type path_info() :: #path_info{}.


-export_type([
              path_info/0
             ]).
