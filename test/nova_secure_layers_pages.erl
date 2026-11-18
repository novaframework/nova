%%% The controllers of nova_secure_layers_SUITE. A security callback does
%%% what the query string of the request tells it to, as in
%%% `?mount=allow&group=deny`, and tells the test case that it ran.
-module(nova_secure_layers_pages).
-behaviour(nova_websocket).

-export([page/1, mount/1, group/1, override/1,
         init/1, websocket_handle/2, websocket_info/2,
         init/2,
         report_calls_to_me/0, stop_reporting_calls/0, calls/0]).

page(Req) ->
    {status, 200, #{}, auth_data(Req)}.

mount(Req) ->
    decide(mount, Req).

group(Req) ->
    decide(group, Req).

override(Req) ->
    decide(override, Req).

decide(Layer, Req) ->
    ?MODULE ! {?MODULE, Layer},
    Name = atom_to_binary(Layer),
    case proplists:get_value(Name, nova_req:parse_qs(Req), ~"allow") of
        ~"allow" -> true;
        ~"data" -> {true, Layer};
        ~"data-seen" -> {true, {Layer, saw, maps:get(auth_data, Req, none)}};
        ~"deny" -> false;
        ~"deny-headers" -> {false, #{~"x-refused-by" => Name}};
        ~"deny-status" -> {false, 403, #{~"x-refused-by" => Name}};
        ~"deny-body" -> {false, 403, #{~"x-refused-by" => Name}, Name};
        ~"redirect" -> {redirect, "/login-of-" ++ atom_to_list(Layer)};
        ~"redirect-binary" -> {redirect, <<"/login-of-", Name/binary>>};
        ~"raise" -> error({raised_by, Layer});
        ~"unexpected" -> {ok, Layer}
    end.

%% The websocket controller: answers a text frame with the data the
%% security callbacks left for it.
-spec init(map()) -> {ok, map()}.
init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, ~"req"}, ControllerData = #{req := Req}) ->
    {reply, {text, auth_data(Req)}, ControllerData};
websocket_handle({text, _Text}, ControllerData) ->
    {reply, {text, auth_data(ControllerData)}, ControllerData};
websocket_handle(_Frame, ControllerData) ->
    {ok, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.

%% The plain Cowboy handler.
-spec init(nova_req:req(), term()) -> {ok, nova_req:req(), term()}.
init(Req, State) ->
    {ok, nova_req:reply(200, #{}, auth_data(Req), Req), State}.

auth_data(ReqOrControllerData) ->
    unicode:characters_to_binary(io_lib:format("~0p", [maps:get(auth_data, ReqOrControllerData, none)])).

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
        {?MODULE, Layer} when is_atom(Layer) -> [Layer | calls()]
    after 0 ->
        []
    end.
