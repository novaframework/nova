-module(nova_stream_h).
-moduledoc false.
-behavior(cowboy_stream).

-export([
         init/3,
         data/4,
         info/3,
         terminate/3,
         early_error/5
        ]).

-record(session, {
                  %% The id the client sent, if any, known or not
                  presented :: binary() | undefined,
                  %% The id the request is working with
                  current :: binary(),
                  issue = false :: boolean(),
                  expire = false :: boolean()
                 }).

-record(state, {
                next :: any(),
                req,
                session :: #session{} | undefined
               }).

-type state() :: #state{}.

-define(COOKIE, ~"session_id").

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
          -> {cowboy_stream:commands(), state()}.
init(StreamID, Req, Opts) ->
    {Req0, Session} = init_session(Req),
    %% Set the correct server-header information
    Req1 = cowboy_req:set_resp_header(<<"server">>, <<"Cowboy/Nova">>, Req0),
    {Commands, Next} = cowboy_stream:init(StreamID, Req1, Opts),
    {Commands, #state{req = Req0, next = Next, session = Session}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
          -> {cowboy_stream:commands(), State} when State::state().
data(StreamID, IsFin, Data, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0}}.

-spec info(cowboy_stream:streamid(), any(), State)
          -> {cowboy_stream:commands(), State} when State::state().
info(_StreamID, {nova_session, Event}, State = #state{session = Session}) ->
    {[], State#state{session = session_event(Event, Session)}};
info(StreamID, {response, Code, Headers, Body}, State) when is_integer(Code) ->
    {Headers0, State0} = session_cookie(Headers, State),
    forward(StreamID, {response, Code, Headers0, Body}, State0);
info(StreamID, {headers, Code, Headers}, State) when is_integer(Code) ->
    {Headers0, State0} = session_cookie(Headers, State),
    forward(StreamID, {headers, Code, Headers0}, State0);
%% A websocket upgrade: a session written in nova_websocket:init/1 gets its
%% cookie on the 101.
info(StreamID, {switch_protocol, Headers, Module, ModState}, State) when is_map(Headers) ->
    {Headers0, State0} = session_cookie(Headers, State),
    forward(StreamID, {switch_protocol, Headers0, Module, ModState}, State0);
info(StreamID, Info, State) ->
    forward(StreamID, Info, State).

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

%%%===================================================================
%%% Sessions
%%%===================================================================

%% The request gets the id the client presented, if any, and a fresh one.
%% nova_session checks the presented id against the backend when it is first
%% used, in the request process, so a slow backend never blocks the
%% connection.
init_session(Req) ->
    case nova_session:enabled() of
        true ->
            {ok, Pending} = nova_session:generate_session_id(),
            case presented_session_id(Req) of
                undefined ->
                    {Req#{nova_session_id => Pending}, #session{current = Pending}};
                Presented ->
                    {Req#{nova_session_presented => Presented, nova_session_pending => Pending},
                     #session{presented = Presented, current = Pending}}
            end;
        false ->
            {Req, undefined}
    end.

presented_session_id(Req) ->
    try lists:keyfind(?COOKIE, 1, cowboy_req:parse_cookies(Req)) of
        {_, SessionId} when is_binary(SessionId), SessionId =/= <<>> ->
            SessionId;
        _ ->
            undefined
    catch
        _:_ ->
            undefined
    end.

session_event(_Event, undefined) ->
    undefined;
session_event({written, SessionId}, Session = #session{presented = Presented}) ->
    Session#session{current = SessionId, issue = SessionId =/= Presented, expire = false};
session_event({renewed, SessionId}, Session) ->
    Session#session{current = SessionId, issue = true, expire = false};
session_event({deleted, SessionId}, Session = #session{presented = Presented}) ->
    Session#session{current = SessionId, issue = false, expire = Presented =/= undefined};
session_event(_Event, Session) ->
    Session.

session_cookie(Headers, State = #state{session = #session{issue = true, current = SessionId} = Session,
                                       req = Req}) ->
    Cookie = cow_cookie:setcookie(?COOKIE, SessionId, cookie_opts(Req)),
    {add_cookie(Cookie, Headers),
     State#state{session = Session#session{presented = SessionId, issue = false}}};
session_cookie(Headers, State = #state{session = #session{expire = true} = Session, req = Req}) ->
    Cookie = cow_cookie:setcookie(?COOKIE, <<>>, (cookie_opts(Req))#{max_age => 0}),
    {add_cookie(Cookie, Headers),
     State#state{session = Session#session{presented = undefined, expire = false}}};
session_cookie(Headers, State) ->
    {Headers, State}.

add_cookie(Cookie, Headers) ->
    Cookies = case maps:get(~"set-cookie", Headers, []) of
                  List when is_list(List) -> List;
                  Single -> [Single]
              end,
    Headers#{~"set-cookie" => Cookies ++ [Cookie]}.

cookie_opts(Req) ->
    Defaults = #{path => ~"/",
                 http_only => true,
                 same_site => lax,
                 secure => maps:get(scheme, Req, ~"http") =:= ~"https"},
    Opts = case application:get_env(nova, session_cookie_opts) of
               {ok, Configured} when is_map(Configured) -> maps:merge(Defaults, Configured);
               _ -> Defaults
           end,
    case application:get_env(nova, session_max_age) of
        {ok, MaxAge} when is_integer(MaxAge), MaxAge > 0 ->
            maps:merge(#{max_age => MaxAge}, Opts);
        _ ->
            Opts
    end.
