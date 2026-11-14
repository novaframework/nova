%%% Collects what is logged while a test runs, from any process.
-module(nova_test_log).

-export([start/0, stop/0, log/2]).

-spec start() -> ok.
start() ->
    _ = logger:remove_handler(?MODULE),
    logger:add_handler(?MODULE, ?MODULE, #{level => all, config => #{collector => self()}}).

%% The events logged since start/0, oldest first.
-spec stop() -> [logger:log_event()].
stop() ->
    ok = logger:remove_handler(?MODULE),
    collected().

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(Event, #{config := #{collector := Collector}}) ->
    Collector ! {?MODULE, Event},
    ok.

collected() ->
    receive
        {?MODULE, Event} -> [Event | collected()]
    after 0 ->
        []
    end.
