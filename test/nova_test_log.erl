%%% Collects what is logged while a test runs, from any process.
-module(nova_test_log).

-export([collect/1, errors/1, start/0, stop/0, log/2]).

%% Runs Fun and returns its result with every log event of the meantime,
%% oldest first. A request process may still log after its response is out,
%% so the events are given a moment to arrive.
-spec collect(fun(() -> Result)) -> {Result, [{logger:level(), term()}]}.
collect(Fun) ->
    Tag = list_to_atom("nova_test_log_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Tag, ?MODULE, #{level => all, config => #{collector => self(), tag => Tag}}),
    try
        Result = Fun(),
        timer:sleep(100),
        {Result, received(Tag)}
    after
        logger:remove_handler(Tag)
    end.

%% The reports logged at error level.
-spec errors([{logger:level(), term()}]) -> [map()].
errors(Events) ->
    [Report || {error, {report, Report}} <- Events, is_map(Report)].

-spec start() -> ok.
start() ->
    _ = logger:remove_handler(?MODULE),
    ok = logger:add_handler(?MODULE, ?MODULE, #{level => all, config => #{collector => self()}}).

%% The events logged since start/0, oldest first.
-spec stop() -> [logger:log_event()].
stop() ->
    ok = logger:remove_handler(?MODULE),
    collected().

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := Level, msg := Msg}, #{config := #{collector := Collector, tag := Tag}}) when is_pid(Collector) ->
    Collector ! {Tag, Level, Msg},
    ok;
log(Event, #{config := #{collector := Collector}}) when is_pid(Collector) ->
    Collector ! {?MODULE, Event},
    ok.

received(Tag) ->
    receive
        {Tag, Level, Msg} -> [{Level, Msg} | received(Tag)]
    after 0 ->
        []
    end.

collected() ->
    receive
        {?MODULE, Event} -> [Event | collected()]
    after 0 ->
        []
    end.
