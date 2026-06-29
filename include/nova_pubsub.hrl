-record(nova_pubsub, {
                      channel :: atom() | binary(),
                      sender :: pid(),
                      topic :: list() | binary(),
                      payload :: any()
                     }).
