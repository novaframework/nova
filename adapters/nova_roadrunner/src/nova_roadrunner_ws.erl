-module(nova_roadrunner_ws).
-moduledoc false.

%% Runs Nova's WebSocket handler as a Roadrunner WebSocket handler. Frames
%% reach it as `{text, Data}` and `{ping, Data}`, and what it returns becomes
%% Roadrunner's results. The state is the handler, its state, and the
%% request its `terminate/3` gets.

-behaviour(roadrunner_ws_handler).

-export([init/1, handle_frame/2, handle_control/2, handle_info/2, handle_drain/2, terminate/2]).

-type session() :: #{handler := module(), state := term(), req := nova_req:req()}.

-spec init(session()) -> roadrunner_ws_handler:result().
init(#{handler := Handler, state := State} = Session) ->
    %% function_exported/3 is false for a module that is not loaded yet.
    {module, Handler} = code:ensure_loaded(Handler),
    case erlang:function_exported(Handler, websocket_init, 1) of
        true -> answer(Handler, websocket_init, [State], Session);
        false -> {ok, Session}
    end.

-spec handle_frame(roadrunner_ws:frame(), session()) -> roadrunner_ws_handler:result().
handle_frame(#{opcode := Opcode, payload := Payload}, Session) ->
    handle({Opcode, iolist_to_binary(Payload)}, Session).

%% Roadrunner has answered a ping by now. The handler is told of both.
-spec handle_control(roadrunner_ws:frame(), session()) -> roadrunner_ws_handler:result().
handle_control(#{opcode := Opcode, payload := Payload}, Session) ->
    handle({Opcode, iolist_to_binary(Payload)}, Session).

-spec handle_info(term(), session()) -> roadrunner_ws_handler:result().
handle_info(Info, #{handler := Handler, state := State} = Session) ->
    answer(Handler, websocket_info, [Info, State], Session).

%% The listener is draining: close as a server that is going away.
-spec handle_drain(integer(), session()) -> roadrunner_ws_handler:result().
handle_drain(_Deadline, Session) ->
    {close, 1001, <<>>, Session}.

-spec terminate(roadrunner_ws_handler:terminate_reason(), session()) -> ok.
terminate(Reason, #{handler := Handler, state := State, req := Req}) ->
    case erlang:function_exported(Handler, terminate, 3) of
        true ->
            _ = call(Handler, terminate, [reason(Reason), Req, State]),
            ok;
        false ->
            ok
    end.

handle(Frame, #{handler := Handler, state := State} = Session) ->
    answer(Handler, websocket_handle, [Frame, State], Session).

%% Roadrunner logs what a callback raises, and what it returns when that is
%% nothing a handler may. The state of a connection has the request of its
%% upgrade in it, and so has its session, so either is raised as Nova logs
%% a crash.
answer(Handler, Function, Arguments, Session) ->
    Returned = call(Handler, Function, Arguments),
    try
        result(Returned, Session)
    catch
        Class:Reason:Stacktrace ->
            erlang:raise(Class, nova_redact:term(Reason), nova_redact:stacktrace(Stacktrace))
    end.

call(Handler, Function, Arguments) ->
    try
        apply(Handler, Function, Arguments)
    catch
        Class:Reason:Stacktrace ->
            erlang:raise(Class, nova_redact:term(Reason), nova_redact:stacktrace(Stacktrace))
    end.

%% What a callback of the handler returns, as Roadrunner's.
result({stop, State}, Session) ->
    {close, 1000, <<>>, Session#{state := State}};
result({ok, State}, Session) ->
    {ok, Session#{state := State}};
result({ok, State, hibernate}, Session) ->
    {ok, Session#{state := State}, [hibernate]};
result({Commands, State}, Session) when is_list(Commands) ->
    commands(Commands, [], Session#{state := State});
result({Commands, State, hibernate}, Session) when is_list(Commands) ->
    commands(Commands, [hibernate], Session#{state := State}).

%% The frames to send, up to a close. A command that tunes the connection of
%% another server is skipped.
commands(Commands, Opts, Session) ->
    case outbound(Commands) of
        {[], none} when Opts =:= [] -> {ok, Session};
        {[], none} -> {ok, Session, Opts};
        {Frames, none} when Opts =:= [] -> {reply, Frames, Session};
        {Frames, none} -> {reply, Frames, Session, Opts};
        {[], {Code, Reason}} -> {close, Code, Reason, Session};
        {Frames, {Code, Reason}} -> {close, Code, Reason, Frames, Session}
    end.

outbound([]) ->
    {[], none};
outbound([close | _]) ->
    {[], {1000, <<>>}};
outbound([{close, <<Code:16, Reason/binary>>} | _]) ->
    {[], {Code, Reason}};
outbound([{close, _Payload} | _]) ->
    {[], {1000, <<>>}};
outbound([{close, Code, Reason} | _]) ->
    {[], {Code, Reason}};
outbound([Command | Rest]) ->
    {Frames, Close} = outbound(Rest),
    case frame(Command) of
        {ok, Frame} -> {[Frame | Frames], Close};
        skip -> {Frames, Close}
    end.

frame({Opcode, Data}) when Opcode =:= text; Opcode =:= binary; Opcode =:= ping; Opcode =:= pong ->
    {ok, {Opcode, Data}};
frame(Opcode) when Opcode =:= ping; Opcode =:= pong ->
    {ok, {Opcode, <<>>}};
frame(_Command) ->
    skip.

%% Why the connection ended, as nova_websocket names it.
reason(normal) -> stop;
reason(shutdown) -> stop;
reason(Reason) -> Reason.
