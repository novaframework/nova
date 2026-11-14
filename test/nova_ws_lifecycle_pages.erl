%%% A websocket controller without terminate/3, and the post_ws_connection
%%% plugin of nova_ws_lifecycle_SUITE.
-module(nova_ws_lifecycle_pages).

-export([init/1, websocket_handle/2, websocket_info/2]).
-export([post_ws_connection/3, plugin_info/0]).

init(#{req := #{path := ~"/refusing/ws"}}) ->
    {error, not_today};
init(ControllerData) ->
    {ok, ControllerData}.

websocket_handle({text, ~"room"}, ControllerData = #{req := Req}) ->
    {reply, {text, maps:get(room, maps:get(extra_state, Req, #{}), ~"none")}, ControllerData};
websocket_handle({text, ~"route"}, ControllerData = #{req := Req}) ->
    {reply, {text, maps:get(nova_route, Req, ~"none")}, ControllerData};
websocket_handle({text, ~"pathinfo"}, ControllerData = #{req := #{extra_state := #{pathinfo := Pathinfo}}}) ->
    {reply, {text, lists:join(~",", Pathinfo)}, ControllerData};
websocket_handle(_Frame, ControllerData) ->
    {reply, {text, ~"ok"}, ControllerData}.

websocket_info(_Info, ControllerData) ->
    {ok, ControllerData}.

post_ws_connection(ControllerState = #{module := Module}, State, _Options) ->
    nova_ws_lifecycle_SUITE:observed({post_ws_connection, Module}),
    {ok, ControllerState, State}.

plugin_info() ->
    #{title => ~"nova_ws_lifecycle_pages",
      version => ~"0.0.0",
      url => ~"",
      authors => [],
      description => ~"Tells the suite that a websocket connection has ended"}.
