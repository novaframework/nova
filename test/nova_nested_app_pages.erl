%%% The controllers of nova_nested_app_SUITE. A callback does what the query
%%% string of the request tells it to, as in `?outer=deny&inner=allow`, and
%%% tells the test case that it ran.
-module(nova_nested_app_pages).

-export([page/1, outer/1, inner/1, group/1, report_calls_to_me/0, stop_reporting_calls/0, calls/0]).

page(Req) ->
    {status, 200, #{}, io_lib:format("~0p", [maps:get(auth_data, Req, none)])}.

outer(Req) ->
    decide(outer, Req).

inner(Req) ->
    decide(inner, Req).

group(Req) ->
    decide(group, Req).

decide(Callback, Req) ->
    ?MODULE ! {?MODULE, Callback},
    Name = atom_to_binary(Callback),
    case proplists:get_value(Name, nova_req:parse_qs(Req), ~"allow") of
        ~"allow" -> true;
        ~"data" -> {true, Callback};
        ~"deny" -> {false, 403, #{}, Name}
    end.

-spec report_calls_to_me() -> ok.
report_calls_to_me() ->
    true = register(?MODULE, self()),
    ok.

-spec stop_reporting_calls() -> ok.
stop_reporting_calls() ->
    true = unregister(?MODULE),
    ok.

%% The callbacks that have run since the last call, in the order they ran.
-spec calls() -> [atom()].
calls() ->
    receive
        {?MODULE, Callback} when is_atom(Callback) -> [Callback | calls()]
    after 0 ->
        []
    end.
