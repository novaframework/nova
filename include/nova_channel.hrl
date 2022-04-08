-record(nova_channel, {
                       channel :: atom(),
                       sender :: pid(),
                       topic :: list() | binary(),
                       payload :: any()
                      }).
