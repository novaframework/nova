-module(nova_ws_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

ws_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Init with websocket upgrade", fun test_init_upgrade/0},
      {"Init with error", fun test_init_error/0},
      {"Websocket init callback", fun test_websocket_init/0},
      {"Websocket handle frame", fun test_websocket_handle/0},
      {"Websocket info message", fun test_websocket_info/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    meck:unload(),
    ok.

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------

test_init_upgrade() ->
    %% Mock WebSocket controller module
    meck:new(test_ws_controller, [non_strict]),
    meck:expect(test_ws_controller, init, fun(#{req := _Req}) ->
                                                  {ok, #{initialized => true}}
                                          end),

    Req = #{method => <<"GET">>, plugins => []},
    State = #{module => test_ws_controller, controller_data => #{}},

    %% Call init
    Result = nova_ws_handler:init(Req, State),

    %% Should upgrade to WebSocket
    ?assertMatch({cowboy_websocket, _, #{controller_data := #{initialized := true}}}, Result),

    meck:unload(test_ws_controller).

test_init_error() ->
    %% Mock controller that returns error
    meck:new(error_ws_controller, [non_strict]),
    meck:expect(error_ws_controller, init, fun(_) ->
                                                   {error, bad_request}
                                           end),

    %% Mock nova_router for error page
    meck:new(nova_router, [passthrough]),
    meck:expect(nova_router, render_status_page, fun(500, Req) ->
                                                         {ok, Req#{error => 500}, #{}}
                                                 end),

    Req = #{method => <<"GET">>, plugins => []},
    State = #{module => error_ws_controller, controller_data => #{}},

    %% Call init
    Result = nova_ws_handler:init(Req, State),

    %% Should render error page
    ?assertMatch({ok, #{error := 500}, #{}}, Result),

    meck:unload([error_ws_controller, nova_router]).

test_websocket_init() ->
    %% Test websocket_init without callback
    %% Use a module that doesn't export websocket_init/1
    State = #{mod => nova_ws_handler_tests, controller_data => #{}},

    Result = nova_ws_handler:websocket_init(State),

    %% Should return ok with ws_handler_process added
    ?assertMatch({ok, #{controller_data := #{ws_handler_process := _}}}, Result).

test_websocket_handle() ->
    %% Mock WebSocket controller with handle callback
    meck:new(handle_ws_controller, [non_strict]),
    meck:expect(handle_ws_controller, websocket_handle, fun({text, <<"ping">>}, ControllerData) ->
                                                                {reply, {text, <<"pong">>}, ControllerData}
                                                        end),

    %% Use the real ws handler
    meck:new(nova_handlers, [passthrough]),
    meck:expect(nova_handlers, get_handler, fun(ws) ->
                                                    {ok, fun nova_basic_handler:handle_ws/2}
                                            end),

    State = #{mod => handle_ws_controller,
              controller_data => #{},
              plugins => []},

    Frame = {text, <<"ping">>},

    %% Call websocket_handle
    Result = nova_ws_handler:websocket_handle(Frame, State),

    %% Should return commands list with pong frame
    ?assertMatch({[{text, <<"pong">>}], _State}, Result),

    meck:unload([handle_ws_controller, nova_handlers]).

test_websocket_info() ->
    %% Mock WebSocket controller with info callback
    meck:new(info_ws_controller, [non_strict]),
    meck:expect(info_ws_controller, websocket_info, fun({notification, Msg}, ControllerData) ->
                                                            {reply, {text, Msg}, ControllerData}
                                                    end),

    %% Use the real ws handler
    meck:new(nova_handlers, [passthrough]),
    meck:expect(nova_handlers, get_handler, fun(ws) ->
                                                    {ok, fun nova_basic_handler:handle_ws/2}
                                            end),

    State = #{mod => info_ws_controller,
              controller_data => #{},
              plugins => []},

    Info = {notification, <<"Hello">>},

    %% Call websocket_info
    Result = nova_ws_handler:websocket_info(Info, State),

    %% Should return commands list with hello message
    ?assertMatch({[{text, <<"Hello">>}], _State}, Result),

    meck:unload([info_ws_controller, nova_handlers]).
