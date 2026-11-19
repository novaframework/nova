-module(nova_stream_h).
-moduledoc false.
-behavior(cowboy_stream).
-include_lib("kernel/include/logger.hrl").

-export([
         init/3,
         data/4,
         info/3,
         terminate/3,
         early_error/5
        ]).

-record(session, {
                  presented :: binary() | undefined,
                  current :: binary(),
                  issue = false :: boolean(),
                  expire = false :: boolean()
                 }).

-record(state, {
                next :: any(),
                req,
                session :: #session{} | undefined,
                sent = false :: false | headers | true,
                invalid_headers = error_terminate :: error_terminate | ignore,
                dropped = false :: boolean()
               }).

-define(DROPPED_INTERVAL, 1000).

-type state() :: #state{}.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
          -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
    {Req0, Session} = init_session(Req),
    Req1 = cowboy_req:set_resp_header(<<"server">>, <<"Cowboy/Nova">>, Req0#{nova_stream => true}),
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{req = Req0, next = Next, session = Session,
                      invalid_headers = invalid_headers(Opts)}}.

invalid_headers(#{invalid_response_headers := ignore}) -> ignore;
invalid_headers(_Opts)                                 -> error_terminate.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
          -> {cowboy_stream:commands(), State} when State::state().
data(StreamID, IsFin, Data, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0}}.

-spec info(cowboy_stream:streamid(), any(), State)
          -> {cowboy_stream:commands(), State} when State::state().
info(_StreamID, {nova_session, Event}, State = #state{session = Session}) ->
    {[], State#state{session = session_event(Event, Session)}};
info(_StreamID, {nova_response_begun, From, Ref}, State = #state{sent = Sent}) when is_pid(From) ->
    From ! {Ref, Sent},
    {[], State};
info(_StreamID, {response, Code, _Headers, _Body}, State = #state{sent = Sent}) when Sent =/= false ->
    {[], dropped(Code, State)};
info(_StreamID, {headers, Code, _Headers}, State = #state{sent = Sent}) when Sent =/= false ->
    {[], dropped(Code, State)};
info(StreamID, Info, State) ->
    respond(StreamID, Info, State).

respond(StreamID, {response, Code, Headers, Body}, State) when is_integer(Code); is_binary(Code) ->
    {Headers0, State0} = session_cookie(Headers, State),
    forward(StreamID, {response, Code, Headers0, Body}, begun(true, Headers0, State0));
respond(StreamID, {headers, Code, Headers}, State) when is_integer(Code); is_binary(Code) ->
    {Headers0, State0} = session_cookie(Headers, State),
    forward(StreamID, {headers, Code, Headers0}, begun(headers, Headers0, State0));
respond(StreamID, {switch_protocol, Headers, Module, ModState}, State) when is_map(Headers) ->
    {Headers0, State0} = session_cookie(Headers, upgrading(State)),
    forward(StreamID, {switch_protocol, Headers0, Module, ModState}, State0);
respond(StreamID, Info, State) ->
    forward(StreamID, Info, State).

upgrading(State = #state{session = #session{presented = undefined} = Session}) ->
    State#state{session = Session#session{issue = true}};
upgrading(State) ->
    State.

begun(Sent, Headers, State = #state{invalid_headers = Mode}) ->
    case cowboy_http:validate_response_headers(Headers, #{invalid_response_headers => Mode}) of
        ok -> State#state{sent = Sent};
        error_terminate -> State
    end.

dropped(_Code, State = #state{dropped = true}) ->
    State;
dropped(Code, State = #state{req = Req}) ->
    case nova_once:per_interval({?MODULE, dropped}, ?DROPPED_INTERVAL) of
        {true, NotLogged} ->
            ?LOG_WARNING(#{msg => ~"The request was answered already, so this response is not sent. A controller, plugin or security callback that answers with cowboy_req has to hand back the request it got from it",
                           status => Code, method => maps:get(method, Req, undefined),
                           path => maps:get(path, Req, undefined), not_logged => NotLogged});
        false ->
            ok
    end,
    State#state{dropped = true}.

forward(StreamID, Info, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), state()) -> any().
terminate(StreamID, Reason, #state{next = Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
                  cowboy_stream:partial_req(), Resp, cowboy:opts())
                 -> Resp
                        when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, {_, _Status, _Headers, _} = Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

init_session(Req) ->
    case nova_session:enabled() of
        true ->
            {ok, Pending} = nova_session:generate_session_id(),
            case nova_session:presented_session_id(Req) of
                undefined ->
                    {Req#{nova_session_id => Pending}, #session{current = Pending}};
                unparsable ->
                    {Req#{nova_session_id => Pending}, undefined};
                Presented ->
                    {Req#{nova_session_presented => Presented, nova_session_pending => Pending},
                     #session{presented = Presented, current = Pending}}
            end;
        false ->
            {Req, undefined}
    end.

session_event(_Event, undefined) ->
    undefined;
session_event({written, SessionId}, Session = #session{presented = Presented}) ->
    Session#session{current = SessionId, issue = SessionId =/= Presented};
session_event({issued, SessionId}, Session = #session{presented = undefined, current = SessionId}) ->
    Session#session{issue = true};
session_event({renewed, SessionId}, Session) ->
    Session#session{current = SessionId, issue = true};
session_event({deleted, SessionId}, Session = #session{presented = Presented}) ->
    Session#session{current = SessionId, issue = false, expire = Presented =/= undefined};
session_event(_Event, Session) ->
    Session.

session_cookie(Headers, State = #state{session = #session{issue = true, current = SessionId} = Session,
                                       req = Req}) ->
    {with_cookie(SessionId, nova_session:cookie_opts(Req), Headers),
     State#state{session = Session#session{presented = SessionId, issue = false, expire = false}}};
session_cookie(Headers, State = #state{session = #session{expire = true} = Session, req = Req}) ->
    {with_cookie(<<>>, (nova_session:cookie_opts(Req))#{max_age => 0}, Headers),
     State#state{session = Session#session{presented = undefined, expire = false}}};
session_cookie(Headers, State) ->
    {Headers, State}.

with_cookie(Value, Opts, Headers) ->
    Name = nova_session:cookie_name(),
    try nova_session:valid_cookie(Name, Opts) andalso cow_cookie:setcookie(Name, Value, Opts) of
        false ->
            log_invalid_cookie(Name),
            Headers;
        Cookie ->
            add_cookie(Cookie, Headers)
    catch
        error:_ ->
            log_invalid_cookie(Name),
            Headers
    end.

log_invalid_cookie(Name) ->
    Key = {?MODULE, invalid_cookie_logged, Name},
    case nova_once:first(Key) of
        true ->
            ?LOG_ERROR(#{msg => ~"Session cookie not set: session_cookie_name or session_cookie_opts is invalid",
                         cookie => Name});
        false ->
            ok
    end.

add_cookie(Cookie, Headers) ->
    Cookies = case maps:get(~"set-cookie", Headers, []) of
                  List when is_list(List) -> List;
                  Single -> [Single]
              end,
    Headers#{~"set-cookie" => Cookies ++ [Cookie]}.
