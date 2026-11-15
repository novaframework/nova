%%% The controllers of nova_ws_upgrade_SUITE.
-module(nova_ws_upgrade_pages).
-behaviour(nova_websocket).

-export([page/1, init/1, websocket_handle/2, websocket_info/2]).
-export([allow/1, pre_security/4, pre_request/4, plugin_info/0, count/0, ran/0]).

-define(SMALL_FRAME, 16).
-define(SECURITY, 1).
-define(PRE_SECURITY, 2).
-define(PRE_REQUEST, 3).
-define(CONTROLLER, 4).

%% Counts what a request to the routes under /guarded runs.
count() ->
    persistent_term:put(?MODULE, counters:new(4, [])).

ran() ->
    Counters = persistent_term:get(?MODULE),
    #{security => counters:get(Counters, ?SECURITY), pre_security => counters:get(Counters, ?PRE_SECURITY),
      pre_request => counters:get(Counters, ?PRE_REQUEST), controller => counters:get(Counters, ?CONTROLLER)}.

ran(What) ->
    counters:add(persistent_term:get(?MODULE), What, 1).

allow(_Req) ->
    ran(?SECURITY),
    true.

pre_security(Req, _Env, _Options, State) ->
    ran(?PRE_SECURITY),
    {ok, Req, State}.

pre_request(Req, _Env, _Options, State) ->
    ran(?PRE_REQUEST),
    {ok, Req, State}.

plugin_info() ->
    #{title => ~"nova_ws_upgrade_pages", version => ~"0.0.0", url => ~"", authors => [],
      description => ~"Counts the requests it sees"}.

page(_Req) ->
    {status, 200, #{}, ~"page"}.

-spec init(map()) -> {ok, map()} | {ok, map(), nova_websocket:opts()}.
init(ControllerData = #{req := #{path := ~"/small/ws"}}) ->
    {ok, ControllerData, #{max_frame_size => ?SMALL_FRAME}};
init(ControllerData = #{req := #{path := <<"/guarded", _/binary>>}}) ->
    ran(?CONTROLLER),
    {ok, ControllerData};
init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, Text}, ControllerData) ->
    {reply, {text, Text}, ControllerData};
websocket_handle(_Frame, ControllerData) ->
    {ok, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.
