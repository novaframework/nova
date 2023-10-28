-record(nova_pubsub, {
    channel :: atom(),
    sender :: pid(),
    topic :: list() | binary(),
    payload :: any()
}).
